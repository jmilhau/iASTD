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
        |QSynchronisation_s  of ((ASTD_transition.t * ASTD_constant.domain) list)* (ASTD_constant.domain)
                                                                              *ASTD_constant.domain * t
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
let qsynchronisation_s_of trans fin_dom not_init_dom init =  QSynchronisation_s (trans,fin_dom,not_init_dom,init);;
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
  |QSynchronisation_s(o,p,q,r) -> (o,p,q,r)
  |_-> failwith "not appropriate use of get_data_from_qsynchro" 

let get_data_automata_s state = match state with
  |Automata_s(a,b,c) -> (a,b,c)
  |_-> failwith "not an automata in get_data_automata_s" 



let get_val choice =match choice with
 |Val(a) -> a 
 |_ -> failwith "not a value"
;;


let val_of a = Val (a);;

let rec create_arrow_val_list v_list a = match a with
  |h::t ->(h,v_list)::(create_arrow_val_list v_list t)
  |[]->[]

let rec fuse lab values arrow_list v_list  = match arrow_list with
  |(label)::t -> if lab=label then (ASTD_constant.fusion v_list values,t)
                              else let (a,b)=(fuse lab values t v_list) in (a,label::b)

  |[]->(ASTD_constant.remove_domain_from v_list values,[])
;;

let rec maj_arrows v_list a b = match b with
  |(lab,values)::t-> let (c,d)=fuse lab values a v_list in (lab,c)::(maj_arrows v_list d t)
  |[]->create_arrow_val_list v_list a
;;



let rec arrow_included a a_list = match a_list with
  |h::t-> if a=h then true else arrow_included a t
  |[] -> false

let rec remove_arrow a a_list = match a_list with
  |h::t -> if a=h then t else (h::(remove_arrow a t))
  |[]-> []

let rec fusion_arrows arrow1 arrow2 = match arrow1 with
           |h::t-> if (arrow_included h arrow2) then (fusion_arrows t arrow2)
                                                else (h::(fusion_arrows t arrow2))
           |[] -> arrow2

let rec fusion_arrows_synch arrow1 arrow2 a_list = match a_list with
  |h::t -> if (arrow_included h arrow1) 
              then if (arrow_included h arrow2) 
                      then (fusion_arrows_synch arrow1 arrow2 t)
                      else (fusion_arrows_synch arrow1 (remove_arrow h arrow2) t) 
              else if (arrow_included h arrow2) 
                      then (fusion_arrows_synch (remove_arrow h arrow1) arrow2 t)
                      else (fusion_arrows_synch (remove_arrow h arrow1) (remove_arrow h arrow2) t) 

  |[]->fusion_arrows arrow1 arrow2

let rec study_comparison arg params = match (arg,params) with
 |((ASTD_term.Const a)::b,c::d)->if c = a then study_comparison b d
                                          else false
 |(a::b,c::d)-> study_comparison b d
 |([],[])-> true
 |_->failwith "parameters and arguments should have the same number" 


let correspond trans event = if ((ASTD_transition.get_label trans)=(ASTD_event.get_label event))
                                then (study_comparison (ASTD_transition.get_params trans) (ASTD_event.get_const event))
                                else false
                             
 

let rec get_val_arrow a_list event = match a_list with
  | (trans,v_list)::t -> if (correspond (trans) (event)) 
                                                     then ASTD_constant.fusion v_list (get_val_arrow t event) 
                                                     else  (get_val_arrow t event) 
  | [] -> ASTD_constant.empty_dom
;;



let rec get_labels arrows = match arrows with
  |(h1,h2)::t-> h1::(get_labels t)
  |[]->[]





let _ASTD_synch_table_ = Hashtbl.create 5 

let register_synch name value state = Hashtbl.add _ASTD_synch_table_ (name,value) state
let get_synch name value = Hashtbl.find _ASTD_synch_table_ (name,value)
let get_synch_state not_init_dom init name c = if (ASTD_constant.is_included c not_init_dom)
                                                         then Hashtbl.find _ASTD_synch_table_ (name,c)
                                                         else init


let rec init_env astd env = match astd with
   |ASTD_astd.Automata (a,b,c,d,e) -> automata_s_of e (init_history b env) (init_env (ASTD_astd.find_subastd e b) env )

   |ASTD_astd.Sequence (a,b,c) -> sequence_s_of Left (init_env b env)

   |ASTD_astd.Choice (a,b,c) -> choice_s_of (undef_choice_of()) (not_defined_state())

   |ASTD_astd.Kleene (a,b) -> kleene_s_of false (init_env b env)
    
   |ASTD_astd.Synchronisation (a,b,c,d) -> synchronisation_s_of  (init_env c env)  (init_env d env)

   |ASTD_astd.Guard (a,b,c) -> guard_s_of false (init_env c env)

   |ASTD_astd.QChoice (a,b,c,d) ->let bind_env = ASTD_environment.bind b (ASTD_term.Const(ASTD_constant.FreeConst))
                                  in qchoice_s_of ChoiceNotMade (init_env d (ASTD_environment.add_binding bind_env env)) 

   |ASTD_astd.QSynchronisation (a,b,val_list,d,e,f,g,h)-> 
               let bind_env = ASTD_environment.bind b (ASTD_term.Const(ASTD_constant.FreeConst))
               in let env2=(ASTD_environment.add_binding bind_env env)
               in let next= (init_env e env2) 
               in let (x,y)=evaluate_arrows e next env2
               in if y 
   then qsynchronisation_s_of (create_arrow_val_list val_list x) (ASTD_constant.empty_dom) (ASTD_constant.empty_dom) next
   else qsynchronisation_s_of (create_arrow_val_list val_list x) val_list (ASTD_constant.empty_dom) next

   |ASTD_astd.Call (a,b,c) -> call_s_of false NotDefined

   |ASTD_astd.Elem (a) -> Elem



and init_history astd_list env = match astd_list with
    |(ASTD_astd.Automata(name,astd_l,arrow_list,final,initial))::q -> 
                        ((name,init_env (ASTD_astd.Automata(name,astd_l,arrow_list,final,initial)) env)::(init_history q env))
    |h::q-> init_history q env

    |[]-> []


and evaluate_arrows astd state env = match (astd,state) with
   |(ASTD_astd.Automata (a,b,c,d,e),Automata_s(f,g,h)) -> begin 
                                                          let l=evaluate_arrows_automata f c env
                                                          and next_astd=(ASTD_astd.find_subastd f b)
                                                          in let (x,y)=evaluate_arrows next_astd h env
                                                          in if (ASTD_astd.is_elem next_astd)
                                                               then (l,ASTD_astd.is_astd_final_in_automata astd f)
                                                               else (l @ x,y)
                                                          end
      
   |(ASTD_astd.Sequence (a,b,c),Sequence_s (d,e)) -> if d=Left 
                                                        then let (x,y)= (evaluate_arrows b e env) 
                                                             in if y
                                                                   then let (v,w)=(evaluate_arrows c (init_env c env) env)
                                                                        in (fusion_arrows x v ,y && w)
                                                                   else (x,false)
                                                        else (evaluate_arrows c e env)
                                                     
   |(ASTD_astd.Choice (a,b,c),Choice_s (d,e)) -> if d= Fst
                                                    then (evaluate_arrows b e env)
                                                    else if d= Snd 
                                                            then (evaluate_arrows c e env)
                                                            else let (x,y)= (evaluate_arrows b (init_env b env) env)
                                                                 and (v,w)= (evaluate_arrows c (init_env c env) env)
                                                                 in (fusion_arrows x v , y || w)

   |(ASTD_astd.Kleene (a,b),Kleene_s (c,d)) -> begin 
                                               let (x,y) = (evaluate_arrows b d env)
                                               in if y then let (v,w)=(evaluate_arrows b (init_env b env) env)
                                                            in (fusion_arrows x v , true)
                                                       else (x,not c)
                                               end

   |(ASTD_astd.Synchronisation (a,b,c,d),Synchronisation_s (e,f)) -> 
                          let (x,y) = evaluate_arrows c e env 
                          and (v,w) = evaluate_arrows d f env 
                          in (fusion_arrows_synch x v b, y && w)

   |(ASTD_astd.Guard (a,b,c),Guard_s (d,e) ) ->begin 
                                                   if d 
                                                   then evaluate_arrows c e env
                                                   else if (ASTD_arrow.estimate_guard env b) 
                                                        then begin evaluate_arrows c e env end
                                                        else begin ([],false) end
                                               end

   |(ASTD_astd.QChoice (a,b,c,d),QChoice_s(e,f)) -> 
                         if e=ChoiceNotMade
                            then let bind_env = ASTD_environment.bind b (ASTD_term.Const(ASTD_constant.FreeConst))
                                 in evaluate_arrows d (init_env d env) (ASTD_environment.add_binding bind_env env)
                            else let bind_env = ASTD_environment.bind b (get_val e)
                                 in evaluate_arrows d f (ASTD_environment.add_binding bind_env env)


   |(ASTD_astd.QSynchronisation (a,b,val_list,d,e,f,g,h),QSynchronisation_s (i,j,k,l))-> 
                             begin 
                               (get_labels i,val_list=j)
                             end
   |(ASTD_astd.Call (a,b,c), Call_s (d,e))-> if d 
                                          then 
                                            evaluate_arrows (ASTD_astd.get_astd b) e (ASTD_environment.increase_call env c)
                                          else 
                                            let astd2= (ASTD_astd.get_astd b)
                                            in evaluate_arrows astd2 (init_env astd2 env) (ASTD_environment.increase_call env c)


   |_ ->([],false)

and evaluate_arrows_automata current arrows env = match arrows with
  |h::t-> if (ASTD_arrow.get_from h)=current then (ASTD_arrow.get_transition h)::(evaluate_arrows_automata current t env)
                                             else (evaluate_arrows_automata current t env)
  |[]->[]
;;



let init astd = init_env astd []



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
                                    (init_history astd_list [])
                                    (init (ASTD_astd.find_subastd n2 (ASTD_astd.get_sub astd)))
             else if name = "H2"
                      then get_deep h_list n
                      else let new_s=(init (ASTD_astd.find_subastd name (ASTD_astd.get_sub astd)))
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






let rec print state astd s = match state with
        |Automata_s (a,b,c) ->print_newline();
                              print_endline(s^"Automata_s ,"^(ASTD_astd.get_name astd));
                              (*print_endline(s^"//StartHistory");
                              (print_h astd b (s^"//"));*)
                              print_endline(s^"sub_state : "^a);
                              print c (ASTD_astd.find_subastd a (ASTD_astd.get_sub astd)) (s^"   ")
        |Sequence_s (a,b) ->print_newline();print_endline(s^"Sequence_s ,");print_endline(s^"step : "^(string_of_seq a));
               begin if a=Left then print b (ASTD_astd.get_seq_l astd) (s^"   ") 
                               else print b (ASTD_astd.get_seq_r astd) (s^"   ") 
               end
        |Choice_s (a,b) ->print_newline();print_endline(s^"Choice_s ,");print_endline(s^"step : "^(string_of_choice a));
               begin if a=Undef then print_endline (s^"No choice made")
                                        else if a=Fst then print b (ASTD_astd.get_choice1 astd) (s^"   ")
                                                      else print b (ASTD_astd.get_choice2 astd)(s^"   ")
               end
        |Kleene_s (a,b) ->print_newline();print_endline(s^"Kleene_s ,");print_endline(s^"started ? : "^(string_of_bool a)); 
                          print b (ASTD_astd.get_astd_kleene astd) (s^"   ") 
        |Synchronisation_s (a,b) ->print_newline();print_endline(s^"Synchronisation_s ,");
                                   print a (ASTD_astd.get_synchro_astd1 astd) (s^"   ");
                                   print b (ASTD_astd.get_synchro_astd2 astd) (s^"   ")
        |QChoice_s (a,b) ->print_newline();print_endline(s^"QChoice_s ,");
                                      begin 
                                      if a=ChoiceNotMade 
                                           then print_endline(s^"Value Not Chosen // Possible values: "^(string_of_qchoice a ))
                                           else print_endline(s^"chosen value : "^(string_of_qchoice a))
                                      end;print b (ASTD_astd.get_qastd astd) (s^"   ")
        |QSynchronisation_s (x,y,z,t) -> print_newline();print_endline(s^"QSynchronisation_s ,not initial values:  "^(ASTD_constant.print_dom z));(print_synch astd (s^"   ") z)
        |Guard_s (a,b) ->print_newline();print_endline(s^"Guard_s ,");print_endline(s^"started ? : "^(string_of_bool a));
                         print b (ASTD_astd.get_guard_astd astd) (s^"   ")
        |Call_s (a,b) ->print_newline();print_endline(s^"Call_s ,");print_endline(s^"started ? : "^(string_of_bool a));
                        print b (ASTD_astd.get_astd (ASTD_astd.get_called_name astd)) (s^"   ")
        |NotDefined ->print_endline (s^"End of the state")
        |Elem -> print_endline(s^"Elem")



and print_h hist astd s = match hist with
  |(n1,h)::t ->print_endline(s^"n1");
               print h (ASTD_astd.find_subastd n1 (ASTD_astd.get_sub astd)) (s);
               print_h t astd s
  |[]->print_endline(s^"EndHistory")


and print_synch astd s not_init =if ASTD_constant.is_empty_dom not_init 
                                    then print_newline ()
                                    else let (value,t)=ASTD_constant.head_tail not_init
                                         in begin 
                                            print_newline ();
                                            print_endline (s^"Value "^(ASTD_constant.string_of value));
                                            print (get_synch (ASTD_astd.get_name astd) value) (ASTD_astd.get_qastd astd) s;
                                            print_synch astd s t
                                            end
;;

  
