let rewrap str maxlen =
  let n = String.length str in
  let res = Buffer.create n in
  let temp = Buffer.create maxlen in
  let flush_temp () =
    Buffer.add_buffer res temp;
    Buffer.reset temp in
  let len = ref 0 in
  let rec read_char = function
  | '\n' -> 
      let temp_len = Buffer.length temp in
      if temp_len > maxlen then
        ( Buffer.add_char res '\n';
         flush_temp ();
         len := 0)
      else if !len + temp_len > maxlen then
        ( Buffer.add_char res '\n';
         len := 0;
         read_char '\n')
      else
        ( len := !len + temp_len;
         flush_temp ())
  | c -> Buffer.add_char temp c in
  for i = 0 to n - 1 do
    if i = 0 || str.[i - 1] <> '\n' then read_char str.[i]
  done;
  read_char '\n';
  Buffer.contents res


(* Maximum line length : can be changed with the -max-line-length option *)
let maxlen = ref 200

class my_gen = object (self)
  inherit Odoc_html.html
  
  method html_of_type_expr b m_name t =
    let s = Odoc_info.remove_ending_newline (Odoc_info.string_of_type_expr t) in
    let s = rewrap s !maxlen in
    let bs = Buffer.add_string in
    bs b "<code class=\"type\">";
    bs b (self#create_fully_qualified_idents_links m_name s);
    bs b "</code>"
    
  method html_of_type_expr_list ?par b m_name sep l =
      let s = Odoc_info.string_of_type_list ?par sep l in
      let s2 = rewrap s !maxlen in
      let bs = Buffer.add_string in
      bs b "<code class=\"type\">";
      bs b (self#create_fully_qualified_idents_links m_name s2);
      bs b "</code>"
end

let my_generator = new my_gen
let _ =
  Odoc_args.set_doc_generator (Some (my_generator :> Odoc_args.doc_generator));
  Odoc_args.add_option ("-max-line-length", Arg.Set_int maxlen, "max line length")

