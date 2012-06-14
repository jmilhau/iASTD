(* how-to get a specification *)
let get_structure (place) = print_endline ("Enter file name of the structure : "^place) ; 
                           try let name = (place^(input_line stdin))
                               in print_endline ("Reading from " ^ name) ;
                                  let structure=ASTD_parser.get_structure_from name 
					in print_endline "Read"; structure
                           with Sys_error s -> begin
                                               print_endline s ;
                                               print_endline "Reading from stdin." ;
                                               ASTD_parser.get_structure_from_stdin () 
                                               end 
;;

let get_event_list (place) = print_endline ("Enter file name of the instructions : "^place) ; 
                           try let name = (place^(input_line stdin))
                               in print_endline ("Reading from " ^ name) ;
                                  let events=ASTD_parser.get_event_list_from name 
					in print_endline "Read"; events
                           with Sys_error s -> begin
                                               print_endline s ;
                                               print_endline "Reading from stdin." ;
                                               ASTD_parser.get_event_list_from_stdin () 
                                               end 
;;



let get_use_of_kappa_indirect (starting_choice_possible) =
	if starting_choice_possible
		then begin
			print_endline "Enter main ASTD's name" ; 
			let name=(input_line stdin)
                        in if "" = name
                               then "MAIN"
				else name
			end
		else "MAIN"




(* Main part *)
let _ = print_endline "========================================" ;
        print_newline () ;
	let affichage = 2
	and kappa_indirect = true
	and print_final = false
	and starting_choice_possible = false
	and place_to_read = "TUnit/"
        in get_structure(place_to_read) ;
	let structure=ASTD_astd.get_astd (get_use_of_kappa_indirect (starting_choice_possible))	
	in print_endline "========================================" ;
	    let event_list= get_event_list(place_to_read)             
            in let time1=(Sys.time())
               in print_endline "========================================" ;
               print_endline "Main structure : " ;
               ASTD_astd.print structure "" ;
               print_newline () ;
               print_endline "========================================" ;
		begin
		if kappa_indirect ;
		then
		       let (kappa_opt_list,dep_used_list)=ASTD_static_kappa_indirect.static_analysis structure
		       in let time2start = (Sys.time())
		       in let analysed_struct=ASTD_static_kappa_indirect.register_static kappa_opt_list dep_used_list structure
		       in let time2end = (Sys.time())
        	       in let state= (ASTD_state.init analysed_struct)
      	 	       in print_endline "========================================" ;
		  	  print_float(float_of_int (List.length kappa_opt_list));
			  print_endline "optimisations found" ;
            	          print_endline "========================================" ;
			  print_endline "========================================" ;
               	          print_endline "Starting state";
  		 	  ASTD_state.print state analysed_struct "" [] [];
             		  print_endline "========================================" ;
               	          let new_state = ASTD_exec_first.execute_event_list affichage state analysed_struct event_list
				in begin if print_final 
					  then begin 
						print_endline "========================================" ;
						print_endline "Final state"; ASTD_state.print new_state structure "" [] [];
        	        			print_endline "========================================" 
						end
					else begin end
					end;
           			          let exec_time = (Sys.time())-.time2end 
					  in print_newline();
  	  				    print_float(time1);
					    print_endline " seconds of READING";
  					    print_float(time2end-.time2start);
					    print_endline " seconds of Kappa indirect static analysis";
  					    print_float(exec_time);
					    print_endline " seconds of EXECUTION";
  					    print_endline ("for "^(string_of_int (List.length event_list))^" events");
	   			            print_float(exec_time/.(float_of_int (List.length event_list)));
		  			    print_endline " per instruction";
          	   			    print_newline()
		else
		       let state = (ASTD_state.init structure)
        	       in ASTD_state.print state structure "" [] [];
                       let new_state = ASTD_exec_first.execute_event_list affichage state structure event_list 
        	          in begin if print_final 
				then begin 
					print_endline "========================================" ;
					print_endline "Final state"; ASTD_state.print new_state structure "" [] [];
        	        		print_endline "========================================" 
					end 
				else begin end 
			  end;
        	          let exec_time = (Sys.time())-.time1 
			  in print_newline();
  	 		    print_float(time1);
			    print_endline " seconds of READING";
  			    print_float(exec_time);
			    print_endline " seconds of EXECUTION";
  			    print_endline ("for "^(string_of_int (List.length event_list))^" events");
		            print_float(exec_time/.(float_of_int (List.length event_list)));
			    print_endline " per instruction";
        	  	    print_newline()
             end           

    ;; 

