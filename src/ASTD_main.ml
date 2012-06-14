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

(*let get_event () = print_endline "Enter an event : " ;
                   ASTD_parser.get_event_from_stdin () *)

(* Main part *)
let _ = print_endline "========================================" ;
        print_newline () ;
        let structure = get_structure() 
        in  print_endline "========================================" ;
            let event_list= get_event_list()             
            in print_newline () ;
               print_endline "========================================" ;
               print_endline "Main structure : " ;
               ASTD_astd.print structure "" ;
               print_newline () ;
               print_endline "========================================" ;
               let state= (ASTD_state.init structure)
               in ASTD_state.print state "";
                  print_endline "========================================" ;
                  ASTD_execute.exec_list structure state event_list
                        

    ;; 

