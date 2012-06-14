type from_state = string
type to_state = string
type through_state = string
type from_final_state = bool


type t = Local of from_state * to_state * ASTD_transition.t * ASTD_predicate.t list * from_final_state
|From_sub of from_state * to_state * through_state * ASTD_transition.t * ASTD_predicate.t list * from_final_state
|To_sub of from_state * to_state * through_state * ASTD_transition.t * ASTD_predicate.t list * from_final_state





exception ASTD_free_variable of ASTD_term.t * t






let _ASTD_arrow_table_ = Hashtbl.create 5 


let local_arrow from_state to_state transition predicates from_final_state =           
                                    Local(from_state,to_state,transition,predicates,from_final_state)
let fsub_arrow from_state to_state through_state transition predicates from_final_state =           
                                    From_sub(from_state,to_state,through_state,transition,predicates,from_final_state)
let tsub_arrow from_state to_state through_state transition predicates from_final_state =           
                                    To_sub(from_state,to_state,through_state,transition,predicates,from_final_state)







let get_from a = match a with
     | Local ( f,_,_,_,_ ) -> f
     | From_sub ( f,_,_,_,_,_ ) -> f
     | To_sub ( f,_,_,_,_,_ ) -> f
let get_to a = match a with
     | Local ( _,t,_,_,_ ) -> t
     | From_sub ( _,t,_,_,_,_ ) -> t
     | To_sub ( _,t,_,_,_,_ ) -> t
let get_through a = match a with
     | From_sub ( _,_,t,_,_,_ ) -> t
     | To_sub ( _,_,t,_,_,_ ) -> t
     | _ -> failwith "no throught for local transitions"
let get_transition a = match a with
     | Local ( _,_,e,_,_ ) -> e
     | From_sub ( _,_,_,e,_,_ ) -> e
     | To_sub ( _,_,_,e,_,_ ) -> e
let get_predicates a = match a with
     | Local ( _,_,_,p,_ ) -> p
     | From_sub ( _,_,_,_,p,_ ) -> p
     | To_sub ( _,_,_,_,p,_ ) -> p
let get_from_final_state a = match a with
     | Local ( _,_,_,_,f ) -> f
     | From_sub ( _,_,_,_,_,f ) -> f
     | To_sub ( _,_,_,_,_,f ) -> f


let get_label_transition a =   ASTD_transition.get_label(get_transition a)
     





let register = Hashtbl.add _ASTD_arrow_table_ 

let register_arrow a = match a with
     | Local ( from,to_state,transition,pred,final ) -> register (from,ASTD_transition.get_label transition,final) a
     | From_sub ( from,to_state,through,transition,pred,final ) -> register (from,ASTD_transition.get_label transition,final) a
     | To_sub ( from,to_state,through,transition,pred,final ) -> register (from,ASTD_transition.get_label transition,final) a

let get from event from_final= Hashtbl.find_all _ASTD_arrow_table_ (from,(ASTD_event.get_label event),from_final)





let rec evaluate_guard env pred_list = 
begin 
    match pred_list with
    |pred::q -> begin 
                 ((evaluate_guard env q) && (ASTD_predicate.evaluate pred env)) 
                end
    |[] -> true
end




let string_of_bool a = if a then "true" else "false"

let valid_arrow event env arrow = let a =(
                                      (begin ASTD_event.compare_action_with_event env (get_transition arrow) event end)
                                      && 
                                      (begin evaluate_guard env (get_predicates arrow) end)
                                         )
                                  in begin 
                                         (*ASTD_environment.print env;
                                         print_newline ();*)
                                         a
                                     end

;;





