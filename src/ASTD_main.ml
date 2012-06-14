(* how-to get a specification *)
let get_structure () = print_endline "Enter file name of the structure : " ; 
                           try let name = (input_line stdin)
                               in print_endline ("Reading from " ^ name) ;
                                  ASTD_parser.get_structure_from name 
                           with Sys_error s -> begin
                                               print_endline s ;
                                               print_endline "Reading from stdin." ;
                                               ASTD_parser.get_structure_from_stdin () 
                                               end 
;;

let get_event_list () = print_endline "Enter file name of the instructions : " ; 
                           try let name = (input_line stdin)
                               in print_endline ("Reading from " ^ name) ;
                                  ASTD_parser.get_event_list_from name 
                           with Sys_error s -> begin
                                               print_endline s ;
                                               print_endline "Reading from stdin." ;
                                               ASTD_parser.get_event_list_from_stdin () 
                                               end 
;;



let get_use_of_kappa_indirect () = print_endline "No to refuse to use kappa_indirect " ; 
                        if "No" != (input_line stdin)
                               then true
				else false


(*let get_event () = print_endline "Enter an event : " ;
                   ASTD_parser.get_event_from_stdin () *)

(* Main part *)
let _ = print_endline "========================================" ;
        print_newline () ;
	let structure = get_structure() 
	and affichage = 1
	and kappa_indirect = false
        in  print_endline "========================================" ;
	    let event_list= get_event_list()             
            in let time1=(Sys.time())
               in print_endline "========================================" ;
               print_endline "Main structure : " ;
               ASTD_astd.print structure "" ;
               print_newline () ;
               print_endline "========================================" ;
		begin
		if get_use_of_kappa_indirect () ;
		then
		       let (kappa_opt_list,dep_used_list)=ASTD_static_kappa_indirect.static_analysis structure
		       in let analysed_struct=ASTD_static_kappa_indirect.register_static kappa_opt_list dep_used_list structure
        	       in let state= (ASTD_state.init analysed_struct)
        	       in ASTD_state.print state analysed_struct "" [] [];
               	       let new_state =ASTD_exec_first.execute_event_list affichage state analysed_struct event_list in begin end
		else
		       let state = (ASTD_state.init structure)
        	       in ASTD_state.print state structure "" [] [];
                       let new_state =ASTD_exec_first.execute_event_list affichage state structure event_list in begin end
		end;
                  print_endline "========================================" ;
                  let total_time = (Sys.time())-.time1 
		  in print_newline();
  		  print_float(total_time);
		  print_endline " seconds of EXECUTION";
  		  print_endline ("for "^(string_of_int (List.length event_list))^" events");
	          print_float(total_time/.(float_of_int (List.length event_list)));
		  print_endline " per instruction";
          	  print_newline()
                        

    ;; 

