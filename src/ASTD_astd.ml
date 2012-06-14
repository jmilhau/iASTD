type astd_name = string;;



type t = Automata of astd_name * t list * ASTD_arrow.t list * astd_name list * astd_name
    | Sequence of  astd_name * t * t
    | Choice of astd_name * t * t 
    | Kleene of astd_name * t
    | Synchronisation of astd_name * ASTD_transition.t list * t * t
    | QChoice of astd_name * ASTD_variable.t * ASTD_constant.domain * t
    | QSynchronisation of astd_name * ASTD_variable.t * ASTD_constant.domain * ASTD_transition.t list * t * 
                                                   ASTD_transition.t list * ASTD_transition.t list * ASTD_transition.t list
    | Guard of astd_name * ASTD_predicate.t list * t
    | Call of astd_name * astd_name * (ASTD_variable.t *ASTD_term.t) list 
    | Elem of astd_name
;; 





let give_name=
  let n= ref 0 
      in function () -> 
                n:=!n+1;
                "gen_astd_"^(string_of_int !n)
;;


let automata_of name astd_l arrow_l final_states init  = Automata (name,astd_l,arrow_l,final_states,init);;

let sequence_of name astd_l astd_r = Sequence (name,astd_l,astd_r);;

let choice_of name astd1 astd2 = Choice (name,astd1,astd2);;

let kleene_of name a = Kleene (name,a);;

let synchronisation_of name transition_list a1 a2 = Synchronisation (name,transition_list,a1,a2);;

let qchoice_of name var val_list a  = QChoice (name,var,val_list,a);;

let qsynchronisation_of name var val_list transition_list a prod users consumers  = 
                                          QSynchronisation (name,var,val_list,transition_list,a,prod,users,consumers);;

let guard_of name predicate_list a = Guard(name,predicate_list,a);;

let call_of name called_name fct_vect = Call(name,called_name,fct_vect);;

let elem_of name = Elem (name) ;;





let get_name a = match a with
  | Automata (name,_,_,_,_) -> name 
  | Sequence (name,_,_) -> name
  | Choice (name,_,_) -> name
  | Kleene (name,_) -> name
  | Synchronisation (name,_,_,_) -> name
  | QChoice (name,_,_,_) -> name
  | QSynchronisation (name,_,_,_,_,_,_,_) -> name  
  | Guard (name,_,_) -> name
  | Call  (name,_,_) -> name
  | Elem (name) -> name
;;


let get_sub a = match a with
  |Automata (_,l,_,_,_) -> l
  | _ -> failwith "unappropriate request aut sub"
;;


let get_arrows a = match a with
  |Automata (_,_,arrows,_,_) -> arrows 
  | _ -> failwith "unappropriate request get_arrows"
;;

let get_final a = match a with
  |Automata (_,_,_,final,_) -> final
  | _ -> failwith "unappropriate request get_final"
;;

let get_init a = match a with
  |Automata (_,_,_,_,init) -> init
  | _ -> failwith "unappropriate request get_init"
;;



let get_seq_l a = match a with
  |Sequence (_,l,_) -> l
  | _ -> failwith "unappropriate request seq_l"
;;

let get_seq_r a = match a with
  |Sequence (_,_,r) -> r
  | _ -> failwith "unappropriate request seq_r"
;;

let get_choice1 a = match a with
  |Choice (_,un,_) -> un
  | _ -> failwith "unappropriate request choice1"
;;

let get_choice2 a = match a with
  |Choice (_,_,deux) -> deux
  | _ -> failwith "unappropriate request choice2"
;;


let get_astd_kleene a = match a with
  |Kleene (_,astd) -> astd
  | _ -> failwith "unappropriate request astd_kleene"
;;


let get_trans_synchronised a = match a with
  |Synchronisation (_,trans_list,_,_) -> trans_list
  |QSynchronisation (_,_,_,trans_list,_,_,_,_) -> trans_list
  | _ -> failwith "unappropriate request trans_synchronised"
;;


let get_synchro_astd1 a = match a with
  |Synchronisation (_,_,astd1,_) -> astd1
  | _ -> failwith "unappropriate request synchro_astd1"
;;


let get_synchro_astd2 a = match a with
  |Synchronisation (_,_,_,astd2) -> astd2
  | _ -> failwith "unappropriate request synchro_astd2"
;;


let get_qvar a = match a with
  |QChoice (_,v,_,_) -> v
  |QSynchronisation (_,v,_,_,_,_,_,_) -> v
  | _ -> failwith "unappropriate request get_qvar"
;;

let get_qvalues_c a = match a with
  |QChoice (_,_,val_list,_) -> val_list
  | _ -> failwith "unappropriate request get_qvalues_c"
;;

let get_qvalues_s a = match a with
  |QSynchronisation (_,_,val_list,_,_,_,_,_) -> val_list
  | _ -> failwith "unappropriate request get_qvalues_s"
;;



let get_qastd a = match a with
  |QChoice (_,_,_,astd) -> astd
  |QSynchronisation (_,_,_,_,astd,_,_,_) -> astd
  | _ -> failwith "unappropriate request get_qastd"

;;

let get_guard_pred a =match a with
  |Guard (_,pred,_) -> pred
  | _ -> failwith "unappropriate request get_guard_pred"
;;
  

let get_guard_astd a =match a with
  |Guard (_,_,astd) -> astd
  | _ -> failwith "unappropriate request get_guard_astd"
;;

let get_called_name a = match a with
  |Call (_,called,_) -> called
  | _ -> failwith "unappropriate request get_called_name"
;;

let get_called_values a = match a with 
  |Call (_,_,var_val_list) -> var_val_list 
  | _ -> failwith "unappropriate request get_called_values"
;;






let rename_astd astd_to_rename namebis = match astd_to_rename with
   |Automata (a,b,c,d,e) -> Automata (namebis,b,c,d,e)
   |Sequence (a,b,c) -> Sequence (namebis,b,c)
   |Choice (a,b,c) -> Choice (namebis,b,c)
   |Kleene (a,b) -> Kleene (namebis,b)
   |Synchronisation (a,b,c,d) -> Synchronisation (namebis,b,c,d)
   |QChoice (a,b,c,d) -> QChoice (namebis,b,c,d)
   |QSynchronisation (a,b,c,d,e,f,g,h) -> QSynchronisation (namebis,b,c,d,e,f,g,h)
   |Guard (a,b,c) -> Guard (namebis,b,c)
   |Call (a,b,c) -> Call (namebis,b,c)
   |Elem(_) -> Elem(namebis)
;;



let rec is_astd_final_in_automata astd state_name = match astd with
   | Automata (a,b,c,d::q,e) -> if d=state_name then true else (is_astd_final_in_automata (Automata (a,b,c,q,e)) state_name)
   | Automata (_,_,_,[],_)-> false
   | _ -> failwith "not an automata"
;;


let is_elem a = match a with
  | Elem(_) -> true
  | _ -> false
;;




let rec find_subastd name astd_list = match astd_list with
  |(a::b) ->
            if (get_name a)=name
                    then a
                    else (find_subastd name b )   
  |_-> failwith "sub-astd not_found"
;;






let _ASTD_astd_table_ = Hashtbl.create 5 
;;

let register a = Hashtbl.add _ASTD_astd_table_ (get_name a) a  
;;

let get_astd name = Hashtbl.find _ASTD_astd_table_ name 
;;




let rec find_transitions astd = match astd with

   |Automata (a,[],[],d,e) ->  []

   |Automata (a,[],h::c,d,e) ->let next=(automata_of a [] c d e ) in (ASTD_arrow.get_transition h)::(find_transitions next)

   |Automata (a,h::b,c,d,e) ->  (find_transitions h)@(find_transitions (automata_of a b c d e))

   |Sequence (a,b,c) -> (find_transitions b) @ (find_transitions c)

   |Choice (a,b,c) -> (find_transitions b) @ (find_transitions c)

   |Kleene (a,b) -> find_transitions b

   |Synchronisation (a,b,c,d) -> (find_transitions c)@(find_transitions d)

   |Guard (a,b,c) -> find_transitions c

   |QChoice (a,b,c,d) -> find_transitions d

   |QSynchronisation (a,b,c,d,e,f,g,h)-> find_transitions e

   |Call (a,b,c) -> (find_transitions (get_astd b)) 

   |Elem (a) -> []
;;



let rec remember_transitions astd = match astd with

   |Automata (a,b,c,d,e) ->  List.iter ASTD_arrow.register_arrow c;
                             List.iter remember_transitions b

   |Sequence (a,b,c) -> remember_transitions b ;remember_transitions c

   |Choice (a,b,c) -> remember_transitions b ; remember_transitions c

   |Kleene (a,b) -> remember_transitions b

   |Synchronisation (a,b,c,d) -> remember_transitions c ;remember_transitions d

   |Guard (a,b,c) -> remember_transitions c

   |QChoice (a,b,c,d) -> begin
                         let l= find_transitions d in 
                         ASTD_arrow.register_transitions_from_list a l;
                         remember_transitions d 
                         end

   |QSynchronisation (a,b,c,d,e,f,g,h)-> begin
                                   let l= find_transitions e in 
                                   ASTD_arrow.register_transitions_from_list a l;
                                   remember_transitions e 
                                   end
                                   

   |Call (a,b,c) -> () 

   |Elem (a) -> ()
;;









let get_data_automata astd = match astd with
  |Automata(a,b,c,d,e) -> (a,b,c,d,e)
  |_-> failwith "not appropriate"


let get_data_sequence astd = match astd with
  |Sequence(a,b,c) -> (a,b,c)
  |_-> failwith "not appropriate"

let get_data_choice astd = match astd with
  |Choice(a,b,c) -> (a,b,c)
  |_-> failwith "not appropriate"

let get_data_kleene astd = match astd with
  |Kleene(a,b) -> (a,b)
  |_-> failwith "not appropriate"

let get_data_synchronisation astd = match astd with
  |Synchronisation(a,b,c,d) -> (a,b,c,d)
  |_-> failwith "not appropriate"

let get_data_guard astd = match astd with
  |Guard(a,b,c) -> (a,b,c)
  |_-> failwith "not appropriate"

let get_data_qchoice astd = match astd with
  |QChoice(a,b,c,d) -> (a,b,c,d)
  |_-> failwith "not appropriate"

let get_data_qsynchronisation astd = match astd with
  |QSynchronisation(a,b,c,d,e,f,g,h) -> (a,b,c,d,e,f,g,h)
  |_-> failwith "not appropriate"

let get_data_call astd = match astd with
  |Call(a,b,c) -> (a,b,c)
  |_-> failwith "not appropriate"




let string_of name = name 
;;


let global_save_astd a = (register a); (remember_transitions a)
;;


let rec string_of_sons sons_list = match sons_list with
 |h::t -> let name = string_of(get_name h) in name^" "^(string_of_sons t) 
 |[] ->""
;;


let rec print astd st = match astd with
   |Automata (a,b,c,d,e) -> let s=string_of_sons b in print_endline (st^" Automata ; Name : "^a^"; Sons : "^s  );print_newline(); 
                                print_sons b st;

   |Sequence (a,b,c) -> print_endline (st^" Sequence ; Name : "^a^"; Son 1 : "^(string_of(get_name b))^"; Son 2 : "^(string_of(get_name c)));print_newline();print b (st^"   "); print c (st^"   ")

   |Choice (a,b,c) -> print_endline (st^"Choice ; Name : "^a^"; Son 1 : "^(string_of(get_name b))^"; Son 2 : "^(string_of(get_name c)));print_newline(); print b (st^"   ");print c (st^"   ")

   |Kleene (a,b) -> print_endline (st^"Kleene ; Name : "^a^"; Son : "^(string_of(get_name b)));print_newline();print b (st^"   ")

   |Synchronisation (a,b,c,d) -> print_endline (st^"Synchronisation ; Name : "^a^"; Son 1 : "^(string_of(get_name c))^"; Son 2 : "^(string_of(get_name d)));print_newline(); print c (st^"   ") ; print d (st^"   ")

   |Guard (a,b,c) -> print_endline (st^"Guard ; Name : "^a^"; Son : "^(string_of(get_name c)));print_newline();print c (st^"   ")

   |QChoice (a,b,c,d) -> print_endline (st^"QChoice ; Name : "^a^"; Var : "^ASTD_variable.string_of(b)^"; Son : "^(string_of(get_name d)));print_newline(); print d (st^"   ") 

   |QSynchronisation (a,b,c,d,e,f,g,h)-> print_endline (st^"QSynchronisation ; Name : "^a^"; Var : "^ASTD_variable.string_of(b)^"; Son : "^(string_of(get_name e)));print_newline(); print e (st^"   ") 

   |Call (a,b,c) -> print_endline (st^"Call ; Name : "^(string_of a)^"; Called : "^(string_of b));print_newline()

   |Elem (a) -> print_endline (st^"Elem ; Name : "^(string_of a));print_newline()



and print_sons astd_list start= match astd_list with
    |h::q -> print h (start^"   ");print_sons q start 
    |[]-> print_newline()
;;






