type position = string
type step = Left | Right
type side = Undef | Fst | Snd
type qchoice = Val of ASTD_term.t | ChoiceNotMade



type astd_name = string

type t = Automata_s of position * ((astd_name * t) list) * t
        |Sequence_s of step * t
        |Choice_s of side * t
        |Kleene_s of bool * t
        |Synchronisation_s of t * t
        |QChoice_s of qchoice * t
        |QSynchronisation_s  of (ASTD_term.t *t) list
        |Guard_s of bool * t
        |Call_s of bool * t
        |NotDefined
        |Elem
;;





let automata_s_of pos hist current = Automata_s (pos,hist,current);;
let sequence_s_of step current = Sequence_s (step,current);;
let choice_s_of side current = Choice_s (side,current);;
let kleene_s_of started current = Kleene_s (started,current);;
let synchronisation_s_of first second = Synchronisation_s (first,second);;
let qchoice_s_of choice current = QChoice_s (choice,current);;
let qsynchronisation_s_of list_synchronised = QSynchronisation_s (list_synchronised);;
let guard_s_of condition current = Guard_s (condition,current);;
let call_s_of called current = Call_s (called,current);;
let not_defined_state () = NotDefined;;
let elem_state () =Elem;;


let undef_choice_of () = Undef;;
let fst_choice_of () = Fst;;
let snd_choice_of () = Snd;;

let left_sequence_of () =Left;;
let right_sequence_of () =Right;;

let qchoice_notmade_of () = ChoiceNotMade


let get_pos state = match state with
 |Automata_s (a,b,c) -> a
 |_ -> failwith "not an automata"
;;


let is_automata state = match state with
 |Automata_s(_,_,_) -> true
 |_-> false


let is_qsynchro state = match state with
 |QSynchronisation_s(_) -> true
 |_-> false


let get_data_from_qsynchro state = match state with
  |QSynchronisation_s(q) -> q
  |_-> failwith "not appropriate use of get_data_from_qsynchro" 

let get_data_automata_s state = match state with
  |Automata_s(a,b,c) -> (a,b,c)
  |_-> failwith "not an automata in get_data_automata_s" 



let get_val choice =match choice with
 |Val(a) -> a 
 |_ -> failwith "not a value"
;;


let val_of a = Val (a);;



let rec init astd = match astd with
   |ASTD_astd.Automata (a,b,c,d,e) -> automata_s_of e (init_history b) (init (ASTD_astd.find_substate e b) )

   |ASTD_astd.Sequence (a,b,c) -> sequence_s_of Left (init b)

   |ASTD_astd.Choice (a,b,c) -> choice_s_of (undef_choice_of()) (not_defined_state())

   |ASTD_astd.Kleene (a,b) -> kleene_s_of false (init b)
    
   |ASTD_astd.Synchronisation (a,b,c,d) -> synchronisation_s_of  (init c)  (init d)

   |ASTD_astd.Guard (a,b,c) -> guard_s_of false (init c)

   |ASTD_astd.QChoice (a,b,c,d) -> qchoice_s_of ChoiceNotMade (init d)

   |ASTD_astd.QSynchronisation (a,b,[],d,e)->  QSynchronisation_s ([])

   |ASTD_astd.QSynchronisation (a,b,value::c,d,e)-> 
                      let s =init (ASTD_astd.qsynchronisation_of a b c d e)
                            in if is_qsynchro s then let q=get_data_from_qsynchro s
                                                       in qsynchronisation_s_of ((value,init e)::q)
                                                else failwith "incomprehensible problem"

   |ASTD_astd.Call (a,b,c) -> call_s_of false NotDefined

   |ASTD_astd.Elem (a) -> Elem



and init_history astd_list = match astd_list with
    |(ASTD_astd.Automata(name,astd_l,arrow_list,final,initial))::q -> 
                           ((name,init (ASTD_astd.Automata(name,astd_l,arrow_list,final,initial)))::(init_history q))
    |h::q-> init_history q

    |[]-> []
;;


let rec modify_h hist name new_state= match hist with
    |(a,b)::q -> if a=name then (name,new_state)::q
                             else (a,b)::(modify_h q name new_state)
    |[] -> failwith "history state not found"


let rec get_deep h_list name = match h_list with
   |(n,mem)::q-> if name=n then mem
                            else get_deep q n
   |[]-> failwith "impossible history"


let rec get_shallow h_list name = match h_list with
   |(n,mem)::q-> if n=name then let (a,b,c) = get_data_automata_s mem in a
                            else get_shallow q n
   |[]-> failwith "impossible history"

 


let goto_automata astd name h_list = match astd with
  | ASTD_astd.Automata (n,astd_list,_,_,_) -> 
          if name="H1"
             then let n2=(get_shallow h_list n )
                      in automata_s_of n2
                                    (init_history astd_list)
                                    (init (ASTD_astd.find_substate n2 (ASTD_astd.get_sub astd)))
             else if name = "H2"
                      then get_deep h_list n
                      else let new_s=(init (ASTD_astd.find_substate name (ASTD_astd.get_sub astd)))
                               in automata_s_of name h_list new_s
  | _ -> failwith "impossible transition "
;;



let string_of_bool a = if a then "true" else "false"
;;



let string_of_seq a = match a with
  |Left -> "Left"
  |Right -> "Right"

let string_of_choice a = match a with
  |Fst -> "First"
  |Snd -> "Second"
  |Undef -> "Choice not made yet"

let string_of_qchoice a=match a with
 |Val(v) -> ASTD_term.string_of v
 |ChoiceNotMade -> "Choice not made yet"
;;




let rec print state s = match state with
        |Automata_s (a,b,c) ->print_newline();
                              print_endline(s^"Automata_s ,");
                              (*print_endline(s^"//StartHistory");
                              (print_h b (s^"//"));*)
                              print_endline(s^"sub_state : "^a);
                              print c (s^"   ")
        |Sequence_s (a,b) ->print_newline();print_endline(s^"Sequence_s ,");print_endline(s^"step : "^(string_of_seq a));print b (s^"   ")
        |Choice_s (a,b) ->print_newline();print_endline(s^"Choice_s ,");print_endline(s^"step : "^(string_of_choice a));print b (s^"   ")
        |Kleene_s (a,b) ->print_newline();print_endline(s^"Kleene_s ,");print_endline(s^"started ? : "^(string_of_bool a));print b (s^"   ")
        |Synchronisation_s (a,b) ->print_newline();print_endline(s^"Synchronisation_s ,");print a (s^"   ");print b (s^"   ")
        |QChoice_s (a,b) ->print_newline();print_endline(s^"QChoice_s ,");
                                           print_endline(s^"chosen value : "^(string_of_qchoice a));print b (s^"   ")
        |QSynchronisation_s (a) -> print_newline();print_endline(s^"QSynchronisation_s ,");print_qsynch a s
        |Guard_s (a,b) ->print_newline();print_endline(s^"Guard_s ,");print_endline(s^"started ? : "^(string_of_bool a));print b (s^"   ")
        |Call_s (a,b) ->print_newline();print_endline(s^"Call_s ,");print_endline(s^"started ? : "^(string_of_bool a));print b (s^"   ")
        |NotDefined ->print_endline (s^"End of the state")
        |Elem -> print_endline(s^"Elem")
and print_qsynch l s = match l with
       |(v,a)::q ->print_newline();print_endline(s^"value : "^(ASTD_term.string_of v));print a (s^"   "); print_qsynch q s
       |[]-> print_endline ""
and print_h hist s = match hist with
  |(n1,h)::t -> print_endline(s^"n1");
               print h (s);
               print_h t s
  |[]->print_endline(s^"EndHistory")
;;



  
