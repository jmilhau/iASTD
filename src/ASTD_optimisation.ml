type astd_name = string;;

type path = astd_name list

type dependency = (path * ASTD_variable.t list * ASTD_variable.t * ASTD_label.t list * ASTD_label.t list) 

type dep_path = Dep_path of (ASTD_variable.t*dependency*dep_path list)

type optimisation = (ASTD_label.t * path * ASTD_variable.t *dep_path)





let _ASTD_kappa_table_ = Hashtbl.create 5 
;;

let register_kappa dep values value = Hashtbl.add _ASTD_kappa_table_ (dep,values) value 
;;

let get_kappa dep values = Hashtbl.find _ASTD_kappa_table_ (dep,values) 
;;





let rec remove_all_kappa dep values = if Hashtbl.mem _ASTD_kappa_table_ (dep,values) 
					then begin
						Hashtbl.remove _ASTD_kappa_table_ (dep,values);
						remove_all_kappa dep values
						end
					else begin end



let rec automatic_gestion_of_kappa_values env label dep_list value= match dep_list with
	|(path,var_met,var,prod,cons)::t->let values= List.map (ASTD_environment.find_value_of env) var_met
						in begin 
							begin if List.mem label prod
								then begin 
									remove_all_kappa (path,var_met,var,prod,cons) values;
									register_kappa (path,var_met,var,prod,cons) values value
									end
								else begin end
							end;
							
							automatic_gestion_of_kappa_values env label t value
						end
	|[]->begin end




(**Garbage collector using cons*)

(*let garbage dep_list event
begin if List.mem label cons
								then begin print_endline "remove"
									remove_all_kappa (path,var_met,var,prod,cons) values
									end
								else begin end
							end;*)

