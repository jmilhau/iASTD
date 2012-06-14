type position = string
type step = Left | Right
type side = Undef | Fst | Snd
type qchoice = Val of ASTD_term.t | ChoiceNotMade
type astd_name = string
type called_path =astd_name list

type t = Automata_s of position * ((astd_name * t) list) * t
        |Sequence_s of step * t
        |Choice_s of side * t
        |Kleene_s of bool * t
        |Synchronisation_s of t * t
        |QChoice_s of qchoice *(ASTD_constant.domain)* (ASTD_constant.domain) * t
        |QSynchronisation_s  of (ASTD_constant.domain)* (ASTD_constant.domain) * (ASTD_constant.domain) * t
        |Guard_s of bool * t
        |Call_s of bool * t
        |NotDefined
        |Elem
;;



    let val_debug = false ;;
    let debug m = if (val_debug) 
                            then (print_endline m )
                            else begin end;;


let automata_s_of pos hist current = Automata_s (pos,hist,current);;
let sequence_s_of step current = Sequence_s (step,current);;
let choice_s_of side current = Choice_s (side,current);;
let kleene_s_of started current = Kleene_s (started,current);;
let synchronisation_s_of first second = Synchronisation_s (first,second);;
let qchoice_s_of choice final_dom unknown_dom current = QChoice_s (choice,final_dom,unknown_dom,current);;
let qsynchronisation_s_of not_fin_dom unknown_dom init_dom init =  QSynchronisation_s (not_fin_dom,unknown_dom,init_dom,init);;
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
  |QSynchronisation_s(p,q,r,s) -> (p,q,r,s)
  |_-> failwith "not appropriate use of get_data_from_qsynchro" 

let get_data_automata_s state = match state with
  |Automata_s(a,b,c) -> (a,b,c)
  |_-> failwith "not an automata in get_data_automata_s" 



let get_val choice = match choice with
 |Val(a) -> a 
 |ChoiceNotMade -> failwith "not a value"
;;


let val_of a = Val (a);;


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



(*let register_synch name value env call_path state = Hashtbl.add _ASTD_synch_table_ (name,value,env,call_path) state*) (**opt: remove state*)

let rec remove_all name value env call_path = if Hashtbl.mem _ASTD_synch_table_ (name,value,env,call_path)
			then begin 
				Hashtbl.remove _ASTD_synch_table_ (name,value,env,call_path);
				remove_all name value env call_path
				end
			else begin end

let register_synch name value env call_path state = 
                                debug ("register state in "^name^" for val "^(ASTD_constant.string_of value)^" in env "^(ASTD_environment.string_of env));
                                remove_all name value env call_path;
                                Hashtbl.add _ASTD_synch_table_ (name,value,env,call_path) state


let get_synch name value env call_path = begin 
                                        debug ("get state in "^name^" for val "^(ASTD_constant.string_of value)^" in env "^(ASTD_environment.string_of env)); 
                                        Hashtbl.find _ASTD_synch_table_ (name,value,env,call_path) 
                                         end
                        
let get_synch_state not_init_dom init name value env call_path =debug ("get_state from init or hash in "^name^" for val "^(ASTD_constant.string_of value)^" in env "^(ASTD_environment.string_of env));
                                                         if (ASTD_constant.is_included value not_init_dom)
                                                         then Hashtbl.find _ASTD_synch_table_ (name,value,env,call_path)
                                                         else init


let rec save_data to_save = match to_save with
        |((name,value,env,call_path),state,add)::t-> if add
                       then begin
                                register_synch name value env call_path state ; 
                                save_data t
                                end
                       else begin debug "remove all";
                                remove_all name value env call_path ;
                                save_data t
                                end
        |[]->begin end




let rec init astd = match astd with
   |ASTD_astd.Automata (a,b,c,d,e,f) -> automata_s_of f (init_history b) (init (ASTD_astd.find_subastd f b))

   |ASTD_astd.Sequence (a,b,c) -> sequence_s_of Left (init b)

   |ASTD_astd.Choice (a,b,c) -> choice_s_of (undef_choice_of()) (not_defined_state())

   |ASTD_astd.Kleene (a,b) -> kleene_s_of false (init b)
    
   |ASTD_astd.Synchronisation (a,b,c,d) -> synchronisation_s_of  (init c)  (init d)

   |ASTD_astd.Guard (a,b,c) -> guard_s_of false (init c)

   |ASTD_astd.QChoice (a,b,val_list,dep,d) ->
	let boxable = ASTD_astd.is_init_final d []
	in if boxable = "true"
		then qchoice_s_of ChoiceNotMade (val_list) (ASTD_constant.empty_dom) (init d)
		else if boxable = "false"
			then qchoice_s_of ChoiceNotMade (ASTD_constant.empty_dom) (ASTD_constant.empty_dom) (init d)
			else qchoice_s_of ChoiceNotMade (ASTD_constant.empty_dom) (val_list) (init d)


   |ASTD_astd.QSynchronisation (a,b,val_list,d,opt,e)-> 
	let boxable = ASTD_astd.is_init_final e []
	in if boxable = "true"
		then qsynchronisation_s_of (ASTD_constant.empty_dom) (ASTD_constant.empty_dom) (ASTD_constant.empty_dom) (init e)
		else if boxable = "false"
			then qsynchronisation_s_of (val_list) (ASTD_constant.empty_dom) (ASTD_constant.empty_dom) (init e)
			else qsynchronisation_s_of (ASTD_constant.empty_dom) (val_list) (ASTD_constant.empty_dom) (init e)

   |ASTD_astd.Call (a,b,c) -> call_s_of false NotDefined

   |ASTD_astd.Elem (a) -> Elem


and init_history astd_list = match astd_list with
    |(ASTD_astd.Automata(name,astd_l,arrow_list,s_final,d_final,initial))::q -> 
                        ((name,init (ASTD_astd.Automata(name,astd_l,arrow_list,s_final,d_final,initial)))::(init_history q ))
    |h::q-> init_history q 

    |[]-> []



(*le problème est celui de qsynch: domain=Finaux U nonFinaux U inconnu  => on a besoin de réduire le plus tôt possible toutes les valeurs
inconnues vu qu'elles devront être recalculées lors de l'évaluation de l'état final => on utilise is_init_final(astd) dans l'initialisation pour le savoir et oui -> final / non -> non final / ?-> inconnu
depuis vu que les trois ensembles forment une partition du domain, il suffit de non final et inconnu pour les représenter (on prends non final et inconnu car quand ils sont vides, l'état est final)
le dernier point est que l'évaluation des inconnus ne doit pas être perdue => il faut l'enregistrer => il faut propager les états modifiés

attention la garde : ca semble intéressant de faire pareil en passant started à true mais le peut on ?
*)

let rec is_final astd state env call_path = match state with
	|Automata_s (position,hist,sub_state) -> 
		let (name,sub_astd,arrows,shallow_final,deep_final,init)=ASTD_astd.get_data_automata astd
		in if List.mem position shallow_final 
			then begin debug ("shallow final "^name);
				(state,true)
				end
			else if List.mem position deep_final 
				then begin debug ("in deep "^name);
					let (new_state,is_final)=is_final astd sub_state env call_path
					in (Automata_s (position,hist,new_state) , is_final)
					end
				else begin debug ("not final in "^name^" the position "^position);
					(state,false)
					end

	|Sequence_s (step,sub_state) -> 
		let (name,left_astd,right_astd)=ASTD_astd.get_data_sequence astd
		in if step=Left
			then let (new_state,final1)=(is_final left_astd sub_state env call_path)
				and (_,final2)=(is_final right_astd (init right_astd) env call_path)
				in (Sequence_s (step,new_state),final1 && final2)
			else let (new_state,final)=(is_final left_astd sub_state env call_path)
				in (Sequence_s (step,new_state),final)

	|Choice_s (side,sub_state) -> 
		let (name,left_astd,right_astd)=ASTD_astd.get_data_choice astd
		in if side = Fst 
			then let (new_state,final)=(is_final left_astd sub_state env call_path)
				in (Choice_s (Fst,new_state),final)
			else if side = Snd
				then let (new_state,final)=(is_final right_astd sub_state env call_path)
					in (Choice_s (Snd,new_state),final)
				else let (_,final1)=(is_final left_astd (init left_astd) env call_path)
					and(_,final2)=(is_final right_astd (init right_astd) env call_path)
					in (state,final1||final2)

	|Kleene_s (started,sub_state) -> 
		let (name,sub_astd)=ASTD_astd.get_data_kleene astd
		in if started 
			then let (new_state,final)=(is_final sub_astd (sub_state) env call_path)
				in begin
					begin if final then debug ("is final "^name) else debug ("is'nt final "^name) end;
					(Kleene_s (started,new_state),final)
					end
			else (state,true)

	|Synchronisation_s (sub_state1,sub_state2) -> 
		let (name,synchro,sub_astd1,sub_astd2)=ASTD_astd.get_data_synchronisation astd
		in let (new_state1,final1)=(is_final sub_astd1 sub_state1 env call_path)
				and(new_state2,final2)=(is_final sub_astd2 sub_state2 env call_path)
				in (Synchronisation_s (new_state1,new_state2),final1 && final2)

	|QChoice_s (qchoice,final_dom,unknown_dom,sub_state) ->
		let (name,var,dom,dep,sub_astd)=ASTD_astd.get_data_qchoice astd
		in if qchoice==ChoiceNotMade
			then let final= ref final_dom
				and unknown= ref unknown_dom
				in begin 
					while !final=ASTD_constant.empty_dom && (!unknown)<>(ASTD_constant.empty_dom)
					do
						let (head_val,tail_val)=ASTD_constant.head_tail !unknown
						in begin unknown:=tail_val;
							let bind_env=ASTD_environment.bind var (ASTD_term.Const head_val)
							in let env2=(ASTD_environment.add_binding bind_env env)
							in let (_,sub_final)=is_final sub_astd (init sub_astd) env2 call_path
								in if sub_final
									then final:= ASTD_constant.insert (ASTD_constant.value_of head_val) !final
									else begin end
					end
				done;
				(QChoice_s  (qchoice,!final,!unknown,NotDefined),!final <>ASTD_constant.empty_dom)
				end
			else let bind_env = ASTD_environment.bind var (get_val qchoice)
				in let env2=(ASTD_environment.add_binding bind_env env)
				in let (new_state,sub_final)=is_final sub_astd (sub_state) env2 call_path
				in begin begin if sub_final then debug ("is final "^name)else debug ("is'nt final "^name) end;
					(QChoice_s  (qchoice,ASTD_constant.empty_dom,ASTD_constant.empty_dom,new_state),sub_final)
					end


	|QSynchronisation_s  (not_final_domain,unknown_domain,not_init_domain,init_state) ->
		let (name,var,dom,synchro,dep,sub_astd)=ASTD_astd.get_data_qsynchronisation astd
		and not_final= ref not_final_domain
		and unknown= ref unknown_domain
		in begin while !not_final=ASTD_constant.empty_dom && (!unknown)<>(ASTD_constant.empty_dom)
			do
				let (head_val,tail_val)=ASTD_constant.head_tail !unknown
				in begin unknown:=tail_val;
					let bind_env=ASTD_environment.bind var (ASTD_term.Const head_val)
					in let env2=(ASTD_environment.add_binding bind_env env)
					in begin if (ASTD_constant.is_included head_val not_init_domain)
						then begin let sub_state =get_synch name head_val env call_path 
							in let (new_state,sub_final)=is_final sub_astd sub_state env2 call_path
							in if sub_final
								then register_synch name head_val env call_path new_state
								else begin not_final := ASTD_constant.insert (ASTD_constant.value_of head_val) !not_final ; 
									register_synch name head_val env call_path new_state 
									end 
							end
						else begin let (_,sub_final)=is_final sub_astd init_state env2 call_path
							in if sub_final 
								then begin end
								else begin(not_final := ASTD_constant.insert (ASTD_constant.value_of head_val) !not_final)end
							end
					end
				end
			done;
			(QSynchronisation_s  (!not_final,!unknown,not_init_domain,init_state),!not_final =ASTD_constant.empty_dom)
			end

	|Guard_s (started,sub_state) ->
		let (name,predicate_list,sub_astd)=ASTD_astd.get_data_guard astd
		in if started 
			then let (new_state,final)=is_final sub_astd sub_state env call_path
				in (Guard_s (started,new_state),final)
			else let (new_state,final) = (is_final sub_astd sub_state env call_path)
				in (Guard_s (started,new_state),(ASTD_arrow.evaluate_guard env predicate_list) && final)

	|Call_s (called,sub_state) ->
		let (name,called_name,fct_vec)=ASTD_astd.get_data_call astd
		in let sub_astd = (ASTD_astd.call_astd called_name (ASTD_environment.increase_call env fct_vec))
		in if List.mem called_name call_path
			then (Call_s (called,sub_state),false)
			else if called 
				then let (new_state,final)=is_final sub_astd sub_state (ASTD_environment.increase_call env fct_vec) (called_name::call_path)
					in (Call_s (called,new_state),final)
				else let (_,final)=is_final sub_astd (init sub_astd) (ASTD_environment.increase_call env fct_vec) (name::call_path)
					in (state,final)

	|Elem -> (Elem,true)

	|NotDefined -> (NotDefined,false)


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
  | ASTD_astd.Automata (n,astd_list,_,_,_,_) -> 
          if name="H1"
             then let n2=(get_shallow h_list n )
                      in automata_s_of n2
                                    (init_history astd_list)
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






let rec print state astd s env call_path = match state with
        |Automata_s (a,b,c) ->print_newline();
                              print_endline(s^"Automata_s ,"^(ASTD_astd.get_name astd));
                              (*print_endline(s^"//StartHistory");
                              (print_h astd b (s^"//")) env call_path;*)
                              print_endline(s^"sub_state : "^a);
                              print c (ASTD_astd.find_subastd a (ASTD_astd.get_sub astd)) (s^"   ") env call_path
        |Sequence_s (a,b) ->print_newline();print_endline(s^"Sequence_s ,");print_endline(s^"step : "^(string_of_seq a));
               begin if a=Left then print b (ASTD_astd.get_seq_l astd) (s^"   ")  env call_path
                               else print b (ASTD_astd.get_seq_r astd) (s^"   ") env call_path
               end
        |Choice_s (a,b) ->print_newline();print_endline(s^"Choice_s ,");print_endline(s^"step : "^(string_of_choice a));
               begin if a=Undef then print_endline (s^"No choice made")
                                        else if a=Fst then print b (ASTD_astd.get_choice1 astd) (s^"   ") env call_path
                                                      else print b (ASTD_astd.get_choice2 astd)(s^"   ") env call_path
               end
        |Kleene_s (a,b) ->print_newline();print_endline(s^"Kleene_s ,");print_endline(s^"started ? : "^(string_of_bool a)); 
                          print b (ASTD_astd.get_astd_kleene astd) (s^"   ") env call_path
        |Synchronisation_s (a,b) ->print_newline();print_endline(s^"Synchronisation_s ,");
                                   print a (ASTD_astd.get_synchro_astd1 astd) (s^"   ") env call_path ;
                                   print b (ASTD_astd.get_synchro_astd2 astd) (s^"   ") env call_path
        |QChoice_s (a,final_dom,unknown_dom,b) ->print_newline();print_endline(s^"QChoice_s ,");
                                      begin 
					let (n,o,p,q,r)= ASTD_astd.get_data_qchoice astd
                                        in if a=ChoiceNotMade 
                                           then begin print_endline(s^"Value Not Chosen // Possible values: "^(ASTD_constant.print_dom p ));
                                                      print b (ASTD_astd.get_qastd astd) (s^"   ") env call_path
                                                end
                                           else begin print_endline(s^"chosen value : "^(string_of_qchoice a)^" for  qchoice "^o);
                                                      let bind_env = ASTD_environment.bind (ASTD_astd.get_qvar astd) (get_val a) 
                                                      in print b (ASTD_astd.get_qastd astd) (s^"   ") (ASTD_environment.add_binding bind_env env) call_path
                                                end
                                      end;
        |QSynchronisation_s (not_fin,unknown,not_init_dom,init) -> print_newline();print_endline(s^"QSynchronisation_s ,Not_Initial values:  "^(ASTD_constant.print_dom not_init_dom));
                                           (print_synch astd (s^"   ") not_init_dom env call_path)
        |Guard_s (a,b) ->print_newline();print_endline(s^"Guard_s ,");print_endline(s^"started ? : "^(string_of_bool a));
                         print b (ASTD_astd.get_guard_astd astd) (s^"   ") env call_path
        |Call_s (a,b) ->print_newline();print_endline(s^"Call_s ,");print_endline(s^"started ? : "^(string_of_bool a));
                        print b (ASTD_astd.get_astd (ASTD_astd.get_called_name astd)) (s^"   ") env ((ASTD_astd.get_name astd)::call_path)
        |NotDefined ->print_endline (s^"End of the state")
        |Elem -> print_endline(s^"Elem")



and print_h hist astd s env call_path = match hist with
  |(n1,h)::t ->print_endline(s^"n1");
               print h (ASTD_astd.find_subastd n1 (ASTD_astd.get_sub astd)) (s) env call_path;
               print_h t astd s env call_path
  |[]->print_endline(s^"EndHistory")


and print_synch astd s not_init env call_path =
	if ASTD_constant.is_empty_dom not_init 
		then print_newline ()
		else let (value,t)=ASTD_constant.head_tail not_init
			in begin 
				print_newline ();
				print_endline (s^"Value "^(ASTD_constant.string_of value)^" for qsynch "^(ASTD_astd.get_qvar astd) );
				let bind_env=ASTD_environment.bind (ASTD_astd.get_qvar astd) (ASTD_term.Const value) 
				in begin 
				print (get_synch (ASTD_astd.get_name astd) value (ASTD_environment.add_binding bind_env env) call_path)
					(ASTD_astd.get_qastd astd)
					s
					(ASTD_environment.add_binding bind_env env) call_path;
				print_endline (s^"end");
				print_synch astd s t env call_path
				end
				end
;;

  
