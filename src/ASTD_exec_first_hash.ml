type state_to_modify=ASTD_state.t
type modified_state=ASTD_state.t

type astd_name = string
type called_path = astd_name list


 
    let val_debug = ref false;;
    let debug m = if (!val_debug) 
                            then (print_endline m )
                            else begin end;;
    let debug_on () = (val_debug := true);;


let get_bdd key opt_const astd = ASTD_state.get_synch key opt_const astd


let register_bdd key opt_const state astd =  ASTD_state.register_synch key opt_const state astd

let remove_bdd key opt_const astd= ASTD_state.remove_all key opt_const astd


let save_synch_data to_save = ASTD_state.save_data to_save 


let get_synch_bdd not_init_dom init_state key opt_const astd= ASTD_state.get_synch_state not_init_dom init_state key opt_const astd


(**Init*)

let rec init astd = match astd with
   |ASTD_astd.Automata (a,b,c,d,e,f) -> ASTD_state.automata_s_of f (init_history b) (init (ASTD_astd.find_subastd f b))

   |ASTD_astd.Sequence (a,b,c) -> ASTD_state.sequence_s_of ASTD_state.Fst (init b)

   |ASTD_astd.Choice (a,b,c) -> ASTD_state.choice_s_of (ASTD_state.undef_choice_of()) (ASTD_state.not_defined_state())

   |ASTD_astd.Kleene (a,b) -> ASTD_state.kleene_s_of false (init b)
    
   |ASTD_astd.Synchronisation (a,b,c,d) -> ASTD_state.synchronisation_s_of  (init c)  (init d)

   |ASTD_astd.Guard (a,b,c) -> ASTD_state.guard_s_of false (init c)

   |ASTD_astd.QChoice (a,b,val_list,dep,d) ->
	let boxable = ASTD_astd.is_init_final d []
	in if boxable = "true"
		then ASTD_state.qchoice_s_of ASTD_state.ChoiceNotMade (val_list) (ASTD_constant.empty_dom) (init d)
		else if boxable = "false"
			then ASTD_state.qchoice_s_of ASTD_state.ChoiceNotMade (ASTD_constant.empty_dom) (ASTD_constant.empty_dom) (init d)
			else ASTD_state.qchoice_s_of ASTD_state.ChoiceNotMade (ASTD_constant.empty_dom) (val_list) (init d)


   |ASTD_astd.QSynchronisation (a,b,val_list,d,opt,e)-> 
	let boxable = ASTD_astd.is_init_final e []
	in if boxable = "true"
		then ASTD_state.qsynchronisation_s_of (ASTD_constant.empty_dom) (ASTD_constant.empty_dom) (ASTD_constant.empty_dom) (init e)
		else if boxable = "false"
			then ASTD_state.qsynchronisation_s_of (val_list) (ASTD_constant.empty_dom) (ASTD_constant.empty_dom) (init e)
			else ASTD_state.qsynchronisation_s_of (ASTD_constant.empty_dom) (val_list) (ASTD_constant.empty_dom) (init e)

   |ASTD_astd.Call (a,b,c) -> ASTD_state.call_s_of false ASTD_state.NotDefined

   |ASTD_astd.Elem (a) -> ASTD_state.Elem


and init_history astd_list = match astd_list with
	|(ASTD_astd.Elem (_))::q -> (init_history q )
    |astd::q ->  ((ASTD_astd.get_name astd,init (astd))::(init_history q ))

    |[]-> []



(*le problème est celui de qsynch: domain=Finaux U nonFinaux U inconnu  => on a besoin de réduire le plus tôt possible toutes les valeurs
inconnues vu qu'elles devront être recalculées lors de l'évaluation de l'état final => on utilise is_init_final(astd) dans l'initialisation pour le savoir et oui -> final / non -> non final / ?-> inconnu
depuis vu que les trois ensembles forment une partition du domain, il suffit de non final et inconnu pour les représenter (on prends non final et inconnu car quand ils sont vides, l'état est final)
le dernier point est que l'évaluation des inconnus ne doit pas être perdue => il faut l'enregistrer => il faut propager les états modifiés

attention la garde : ca semble intéressant de faire pareil en passant started à true mais le peut on ?
*)

(**Final states*)
let rec is_final astd state env call_path current_key= match state with
	|ASTD_state.Automata_s (position,hist,sub_state) -> 
		let (name,sub_astd,arrows,shallow_final,deep_final,init)=ASTD_astd.get_data_automata astd
		in if List.mem position shallow_final 
			then begin debug ("shallow final "^name);
				(state,true)
				end
			else if List.mem position deep_final 
				then begin debug ("in deep "^name);
					let (new_state,final)=is_final astd sub_state env call_path (current_key^"/"^position)
					in (ASTD_state.Automata_s (position,hist,new_state) , final)
					end
				else begin debug ("not final in "^name^" the position "^position);
					(state,false)
					end

	|ASTD_state.Sequence_s (step,sub_state) -> 
		let (name,first,second)=ASTD_astd.get_data_sequence astd
		in if step=ASTD_state.Fst
			then let (new_state,final1)=(is_final first sub_state env call_path (current_key^"/"^(ASTD_astd.get_name first)))
				and (_,final2)=(is_final second (init second) env call_path (current_key^"/"^(ASTD_astd.get_name second)))
				in (ASTD_state.Sequence_s (step,new_state),final1 && final2)
			else let (new_state,final)=(is_final second sub_state env call_path (current_key^"/"^(ASTD_astd.get_name second)))
				in (ASTD_state.Sequence_s (step,new_state),final)

	|ASTD_state.Choice_s (side,sub_state) -> 
		let (name,left_astd,right_astd)=ASTD_astd.get_data_choice astd
		in if side = ASTD_state.Right
			then let (new_state,final)=(is_final right_astd sub_state env call_path (current_key^"/"^(ASTD_astd.get_name right_astd)))
				in (ASTD_state.Choice_s (ASTD_state.Right,new_state),final)
			else if side = ASTD_state.Left
				then let (new_state,final)=(is_final left_astd sub_state env call_path (current_key^"/"^(ASTD_astd.get_name left_astd)))
					in (ASTD_state.Choice_s (ASTD_state.Left,new_state),final)
				else let (_,final1)=(is_final left_astd (init left_astd) env call_path (current_key^"/"^(ASTD_astd.get_name left_astd)))
 				     and(_,final2)=(is_final right_astd (init right_astd) env call_path (current_key^"/"^(ASTD_astd.get_name right_astd)))
					in (state,final1||final2)

	|ASTD_state.Kleene_s (started,sub_state) -> 
		let (name,sub_astd)=ASTD_astd.get_data_kleene astd
		in if started 
			then let (new_state,final)=(is_final sub_astd (sub_state) env call_path (current_key^"/"^(ASTD_astd.get_name sub_astd)))
				in begin
					begin if final then debug ("is final "^name) else debug ("isn't final "^name) end;
					(ASTD_state.Kleene_s (started,new_state),final)
					end
			else (state,true)

	|ASTD_state.Synchronisation_s (sub_state1,sub_state2) -> 
		let (name,synchro,sub_astd1,sub_astd2)=ASTD_astd.get_data_synchronisation astd
		in let (new_state1,final1)=(is_final sub_astd1 sub_state1 env call_path (current_key^"/"^(ASTD_astd.get_name sub_astd1)))
				and(new_state2,final2)=(is_final sub_astd2 sub_state2 env call_path (current_key^"/"^(ASTD_astd.get_name sub_astd2)))
				in (ASTD_state.Synchronisation_s (new_state1,new_state2),final1 && final2)

	|ASTD_state.QChoice_s (qchoice,final_dom,unknown_dom,sub_state) ->
		let (name,var,dom,dep,sub_astd)=ASTD_astd.get_data_qchoice astd
		in if qchoice==ASTD_state.ChoiceNotMade
			then let final= ref final_dom
				and unknown= ref unknown_dom
				in begin 
					while !final=ASTD_constant.empty_dom && (!unknown)<>(ASTD_constant.empty_dom)
					do
						let (head_val,tail_val)=ASTD_constant.head_tail !unknown
						in begin unknown:=tail_val;
							let bind_env=ASTD_environment.bind var (ASTD_term.Const head_val)
							in let env2=(ASTD_environment.add_binding bind_env env)
							in let (_,sub_final)=is_final sub_astd (init sub_astd) env2 call_path (current_key^":"^(ASTD_constant.string_of head_val)^"/"^(ASTD_astd.get_name sub_astd))
								in if sub_final
									then final:= ASTD_constant.insert (ASTD_constant.value_of head_val) !final
									else begin end
					end
				done;
				(ASTD_state.QChoice_s  (qchoice,!final,!unknown,ASTD_state.NotDefined),!final <>ASTD_constant.empty_dom)
				end
			else let bind_env = ASTD_environment.bind var (ASTD_state.get_val qchoice)
				in let env2=(ASTD_environment.add_binding bind_env env)
				in let (new_state,sub_final)=is_final sub_astd (sub_state) env2 call_path (current_key^":"^(ASTD_constant.string_of (ASTD_term.extract_constant_from_term (ASTD_state.get_val qchoice)))^"/"^(ASTD_astd.get_name sub_astd))
				in begin begin if sub_final then debug ("is final "^name)else debug ("is'nt final "^name) end;
					(ASTD_state.QChoice_s  (qchoice,ASTD_constant.empty_dom,ASTD_constant.empty_dom,new_state),sub_final)
					end


	|ASTD_state.QSynchronisation_s  (not_final_domain,unknown_domain,not_init_domain,init_state) ->
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
						then begin let sub_state =get_bdd current_key (Some head_val) sub_astd 
							in let (new_state,sub_final)=is_final sub_astd sub_state env2 call_path (current_key^":"^(ASTD_constant.string_of head_val)^"/"^(ASTD_astd.get_name sub_astd))
							in if sub_final
								then begin debug ("value final : "^(ASTD_constant.string_of head_val));
									register_bdd current_key (Some head_val) new_state sub_astd
									end
								else begin debug ("value not final : "^(ASTD_constant.string_of head_val));
									not_final := ASTD_constant.insert (ASTD_constant.value_of head_val) !not_final ; 
									register_bdd current_key (Some head_val) new_state sub_astd 
									end 
							end
						else begin let (_,sub_final)=is_final sub_astd init_state env2 call_path (current_key^":"^(ASTD_constant.string_of head_val)^"/"^(ASTD_astd.get_name sub_astd))
							in if sub_final 
								then begin end
								else begin(not_final := ASTD_constant.insert (ASTD_constant.value_of head_val) !not_final)end
							end
					end
				end
			done;
			(ASTD_state.QSynchronisation_s  (!not_final,!unknown,not_init_domain,init_state),!not_final =ASTD_constant.empty_dom)
			end

	|ASTD_state.Guard_s (started,sub_state) ->
		let (name,predicate_list,sub_astd)=ASTD_astd.get_data_guard astd
		in if started 
			then let (new_state,final)=is_final sub_astd sub_state env call_path (current_key^"/"^(ASTD_astd.get_name sub_astd))
				in (ASTD_state.Guard_s (started,new_state),final)
			else let (new_state,final) = (is_final sub_astd sub_state env call_path (current_key^"/"^(ASTD_astd.get_name sub_astd)))
				in (ASTD_state.Guard_s (started,new_state),(ASTD_arrow.evaluate_guard env predicate_list) && final)

	|ASTD_state.Call_s (called,sub_state) ->
		let (name,called_name,fct_vec)=ASTD_astd.get_data_call astd
		in let sub_astd = (ASTD_astd.call_astd called_name (ASTD_environment.increase_call env fct_vec))
		in if List.mem called_name call_path
			then (ASTD_state.Call_s (called,sub_state),false)
			else if called 
				then let (new_state,final)=is_final sub_astd sub_state (ASTD_environment.increase_call env fct_vec) (called_name::call_path) (current_key^"/"^called_name)
					in (ASTD_state.Call_s (called,new_state),final)
				else let (_,final)=is_final sub_astd (init sub_astd) (ASTD_environment.increase_call env fct_vec) (name::call_path) (current_key^"/"^called_name)
					in (state,final)

	|ASTD_state.Elem -> (ASTD_state.Elem,true)

	|ASTD_state.NotDefined -> (ASTD_state.NotDefined,false)







let rec modify_h hist name new_state= match hist with
    |(a,b)::q -> if a=name then (name,new_state)::q
                             else (a,b)::(modify_h q name new_state)
    |[] -> failwith "history state not found"


let rec get_deep h_list name = match h_list with
   |(n,mem)::q-> if name=n then mem
                            else get_deep q n
   |[]-> failwith "impossible history"


let rec get_shallow h_list name = match h_list with
   |(n,mem)::q-> if n=name then let (a,b,c) = ASTD_state.get_data_automata_s mem in a
                            else get_shallow q n
   |[]-> failwith "impossible history"

 
(**Exec*)


let goto_automata astd name h_list = match astd with
  | ASTD_astd.Automata (n,astd_list,_,_,_,_) -> 
          if name="H1"
             then let n2=(get_shallow h_list n )
                      in ASTD_state.automata_s_of n2
                                    (init_history astd_list)
                                    (init (ASTD_astd.find_subastd n2 (ASTD_astd.get_sub astd)))
             else if name = "H2"
                      then get_deep h_list n
                      else let new_s=(init (ASTD_astd.find_subastd name (ASTD_astd.get_sub astd)))
                               in ASTD_state.automata_s_of name (init_history astd_list) new_s
  | _ -> failwith "impossible transition "
;;








let rec apply_all_modifs name var astd2 env not_init current_key modifs study_state = 
	if modifs <> []
	then let (const,mod_state,to_save,kappa)= List.hd modifs
		and (_,key2,sub_astd2,state1,save,kappa2,_)=study_state
		in let (not_fin_dom,unknown_dom,not_init_dom,init)=ASTD_state.get_data_from_qsynchro state1
		in let bind_env = ASTD_environment.bind var (ASTD_term.Const const)
		in let env2=(ASTD_environment.add_binding bind_env env)
		in let value = ASTD_constant.value_of const
		in let (new_state,isfinal)=(is_final astd2 mod_state env2 [] key2)
		in begin 
			if isfinal
			then begin
				let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.insert value not_init_dom),init);
				in apply_all_modifs name var astd2 env not_init current_key (List.tl modifs) (new_study_state,key2,sub_astd2,new_study_state,((current_key,Some const),astd2,new_state,true)::(to_save@save),kappa@kappa2,true)
				end
			else begin
				let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.insert value not_init_dom),init);
				in apply_all_modifs name var astd2 env  not_init current_key (List.tl modifs) (new_study_state,key2,sub_astd2,new_study_state,((current_key,Some const),astd2,new_state,true)::(to_save@save),kappa@kappa2,true)
				end
			end

	else begin 
		 study_state
		end



let rec find_arrow arrow_list event current sub_state sub_astd env  current_key = match arrow_list with
	| ASTD_arrow.Local ( from,to_state,transition,pred,final )::a::t -> 
		if (current=from)&&((ASTD_event.get_label event) = (ASTD_transition.get_label transition))
			then if final
				then begin 
					let (new_state,final)=(is_final sub_astd sub_state env [] current_key)
					in if final
					then begin 
						if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
						then (ASTD_arrow.Local ( from,to_state,transition,pred,final ),new_state,true)
						else find_arrow (a::t) event current new_state sub_astd env current_key
						end
					else find_arrow (a::t) event current new_state sub_astd env current_key
					end
				else begin 
					if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
					then (ASTD_arrow.Local ( from,to_state,transition,pred,final ),sub_state,true)
					else find_arrow (a::t) event current sub_state sub_astd env current_key 
					end
			else find_arrow (a::t) event current sub_state sub_astd env current_key

	| ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final )::a::t ->
		if (through=current) &&((ASTD_event.get_label event) = (ASTD_transition.get_label transition)) && (ASTD_astd.is_automata sub_astd)
			then let (sub_from,hist,sub_state2) =ASTD_state.get_data_automata_s sub_state
				in if from=sub_from
					then if final
						then let (new_sub_state,final)=(is_final (ASTD_astd.find_subastd sub_from (ASTD_astd.get_sub sub_astd)) sub_state2 env [] current_key)
							in let new_state = ASTD_state.Automata_s (sub_from,hist,new_sub_state)
							in if final 
							then if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
								then (ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final ),new_state,true)
								else find_arrow (a::t) event current new_state sub_astd env current_key
							else find_arrow (a::t) event current new_state sub_astd env current_key
						else if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
							then (ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final ),sub_state,true)
							else find_arrow (a::t) event current sub_state sub_astd env current_key
					else find_arrow (a::t) event current sub_state sub_astd env current_key
			else find_arrow (a::t) event current sub_state sub_astd env current_key

	| ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final )::a::t -> 
		if (current=from)&&((ASTD_event.get_label event) = (ASTD_transition.get_label transition))
			then if final
				then let (new_state,final)= is_final sub_astd sub_state env [] current_key
					in if final
					then if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
						then (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),new_state,true)
						else find_arrow (a::t) event current new_state sub_astd env current_key
					else find_arrow (a::t) event current new_state sub_astd env current_key
				else if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
					then (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),sub_state,true)
					else find_arrow (a::t) event current sub_state sub_astd env current_key
			else find_arrow (a::t) event current sub_state sub_astd env current_key

	| ASTD_arrow.Local ( from,to_state,transition,pred,final )::[] ->
		if (current=from)&&((ASTD_event.get_label event) = (ASTD_transition.get_label transition))
			then if final
				then begin 
					let (new_state,final)=(is_final sub_astd sub_state env [] current_key)
					in if final
					then begin 
						if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
						then (ASTD_arrow.Local ( from,to_state,transition,pred,final ),new_state,true)
						else (ASTD_arrow.Local ( from,to_state,transition,pred,final ),new_state,false)
						end
					else (ASTD_arrow.Local ( from,to_state,transition,pred,final ),new_state,false)
					end
				else begin 
					if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
					then (ASTD_arrow.Local ( from,to_state,transition,pred,final ),sub_state,true)
					else (ASTD_arrow.Local ( from,to_state,transition,pred,final ),sub_state,false)
					end
			else (ASTD_arrow.Local (from,to_state,transition,pred,final ),sub_state,false)

	| ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final )::[] ->
		if (through=current) &&((ASTD_event.get_label event) = (ASTD_transition.get_label transition)) && (ASTD_astd.is_automata sub_astd)
			then let (sub_from,hist,sub_state2) =ASTD_state.get_data_automata_s sub_state
				in if from=sub_from
					then if final
						then let (new_sub_state,final)=(is_final (ASTD_astd.find_subastd sub_from (ASTD_astd.get_sub sub_astd)) sub_state2 env [] current_key)
							in let new_state = ASTD_state.Automata_s (sub_from,hist,new_sub_state)
							in if final 
							then if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
								then (ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final ),new_state,true)
								else (ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final ),new_state,false)
							else (ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final ),new_state,false)
						else if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
							then (ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final ),sub_state,true)
							else (ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final ),sub_state,false)
					else (ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final ),sub_state,false)
			else (ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final ),sub_state,false)

	| ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final )::[] -> 
		if (current=from)&&((ASTD_event.get_label event) = (ASTD_transition.get_label transition))
			then if final
				then let (new_state,final)=(is_final sub_astd sub_state env [] current_key)
					in if final
					then if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
						then (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),new_state,true)
						else (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),new_state,false)
					else (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),new_state,false)
				else if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
					then (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),sub_state,true)
					else (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),sub_state,false)
			else (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),sub_state,false)
	|[]->failwith "an automata should have at least one transition"



let rec find_corresponding_params label trans_list var param_list indeter none= match trans_list with
	|trans::tail-> if label=ASTD_transition.get_label trans
				then let params= ASTD_transition.get_params trans
					in if List.mem var params
						then (find_corresponding_params label tail var (params::param_list) indeter false)
						else (find_corresponding_params label tail var param_list true false)
				else find_corresponding_params label tail var param_list indeter none
	|[]->(param_list,indeter,none)

let rec find_value c_list params_list var c_list_save found_val= match (params_list,c_list) with
	|((head_var::var_tail)::t,head_c::c_tail)->begin debug ("find_val for "^(ASTD_term.string_of var)^" vs "^(ASTD_term.string_of head_var));
							if head_var=var 
							then let value=ASTD_term.Const head_c
								in begin
									debug ("1 value found "^(ASTD_term.string_of (ASTD_term.Const head_c)));
									if List.mem value found_val
										then begin debug "already had";
											find_value c_tail (var_tail::t) var c_list_save found_val
											end
										else begin debug "added";
											find_value c_tail (var_tail::t) var c_list_save (value::found_val)
											end
									end
							else begin debug "not equal";
								if true (**(head_var "est une constante")&&("égalité de constante et valeur")*)
								then find_value c_tail (var_tail::t) var c_list_save found_val
								else find_value c_list_save t var c_list_save found_val
								end
							end
	|([]::t,[])->find_value c_list_save t var c_list_save found_val
	|([],_)->found_val
	|_->failwith "event and transition have not the same number of arguments"








let get_values event astd2 var = debug "extract values for kappa indirect";
	let trans_list=ASTD_astd.get_sub_transitions [] astd2
	and (label,c_list)=ASTD_event.get_data event
	in let (param_trans,indeter,none)=find_corresponding_params label trans_list var [] false true
	in begin debug "params extracted" ;
		if param_trans<>[] 
		then (find_value c_list param_trans var c_list [],indeter)
		else ([],indeter && not(none))
		end



let rec find_value_indirect event astd env dep_path_list var = match dep_path_list with
	|(ASTD_optimisation.Dep_path(variable,dep,dep_path_list2))::tail-> 
			if var=variable
				then begin debug ("found dep to follow for "^var);
					let (_,var_met,_,_,_)=dep
					in let values=(List.map (find_value_indirect event astd env dep_path_list2) var_met)
					in begin debug ("found values for dep for "^var);ASTD_optimisation.get_kappa dep  values end
					end 
				else find_value_indirect event astd env tail var
	|[]-> begin debug ("try in env for "^var);
		try ASTD_environment.find_value_of env var
		with Not_found-> begin debug ("try to retrieve kappa indirect threw kappa direct for "^var);
					let (qchoice,indeter)=(get_values event astd (ASTD_term.Var var))
					in if List.length qchoice=1
						then begin debug "get val to apply "; let value_ext=List.hd qchoice in begin debug "value extracted";value_ext end end
						else failwith "impossible to kappa optimize indirect (find value)"
					end
		end









let rec active_optimisation event astd env label var opt_list = match opt_list with
	|(label2,path,variable,dep_path)::tail->if label=label2 
							then begin debug ("found_label "^label^" in active opt");
 								ASTD_state.Val(find_value_indirect event astd env [dep_path] var)
								end
							else begin
								active_optimisation event astd env label var tail
								end
	|[]->begin debug ("NOT found label "^label^" in active opt");
		ASTD_state.ChoiceNotMade
		end








let rec execute state astd event env current_key   = 
let label_list = List.map (ASTD_transition.get_label) (ASTD_astd.get_sub_transitions [] astd)
in if (not(List.mem (ASTD_event.get_label event) label_list) )
then (state,"",astd,state,[],[],false) 
else match state with 

  |ASTD_state.Automata_s (name_current,hist,state2) -> 
	begin debug ("aut exec "^(ASTD_astd.get_name astd));
	let  (name,sub_astd,arrow_list,s_final,d_final,_)=ASTD_astd.get_data_automata astd 
	in let astd2 = (ASTD_astd.find_subastd name_current sub_astd)
	in if List.mem (ASTD_event.get_label event) (List.map (ASTD_transition.get_label) (List.map ASTD_arrow.get_transition arrow_list))
		then let (arrow,new_state,found)=find_arrow arrow_list event name_current state2 (ASTD_astd.find_subastd name_current sub_astd) env (current_key^"/"^name_current)
			in if found 
				then let new_hist = begin if ASTD_state.is_automata new_state
							then (modify_h hist name_current new_state) 
							else hist
							end
					in if ASTD_arrow.is_to_sub arrow
						then let dest=ASTD_arrow.get_to arrow
							and through = ASTD_arrow.get_through arrow
							in let new_sub_astd = (ASTD_astd.find_subastd through sub_astd)
							in let new_sub_state = ASTD_state.Automata_s (through,new_hist,goto_automata new_sub_astd dest new_hist)
							in (new_sub_state,current_key,astd,new_sub_state,[],[],true)
						else let dest=ASTD_arrow.get_to arrow
							in let new_sub_astd = (ASTD_astd.find_subastd dest sub_astd)
							in let new_sub_state = 
								(ASTD_state.Automata_s (dest,new_hist,init new_sub_astd))
							in (new_sub_state,current_key,astd,new_sub_state,[],[],true)
				else let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute new_state astd2 event env (current_key^"/"^name_current )
					in (delta,key,sub_astd,ASTD_state.Automata_s (name_current,hist,mod_state),to_save,kappa,is_modified)
		else let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event env  (current_key^"/"^(ASTD_astd.get_name astd2) )
			in (delta,key,sub_astd,ASTD_state.Automata_s (name_current,hist,mod_state),to_save,kappa,is_modified)
	end


  |ASTD_state.Sequence_s (step,state2) -> 
	begin debug ("seq exec "^(ASTD_astd.get_name astd));
	let (name,first,second)=ASTD_astd.get_data_sequence astd
	in if step = ASTD_state.Fst
		then begin 
			let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute state2 first event env  (current_key^"/"^(ASTD_astd.get_name first) )
			in if is_modified 
				then begin
					(delta,
					key,
					sub_astd,
					ASTD_state.Sequence_s (step,mod_state),
					to_save,
					kappa,
					true)
					end
				else let (new_state,final)=(is_final first mod_state env [] (current_key^"/"^(ASTD_astd.get_name first) ))
					in if final
						then begin
							let (_,_,_,mod_state2,to_save2,kappa2,is_modified2)=execute ( init second) second event env  (current_key^"/"^(ASTD_astd.get_name second) )
							in if is_modified2
								then begin
									(ASTD_state.Sequence_s (ASTD_state.Snd,mod_state2),
									current_key,
									astd,
									ASTD_state.Sequence_s (ASTD_state.Snd,mod_state2),
									to_save2,
									kappa2,
									true)
									end
								else begin 
									(state,"",astd,state,[],[],false) 
									end
							end
						else (state,"",astd,state,[],[],false)
			end
		else begin
			let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute state2 second event env (current_key^"/"^(ASTD_astd.get_name second) )
			in if is_modified 
				then begin
					(delta,
					key,
					sub_astd,
					ASTD_state.Sequence_s (step,mod_state),
					to_save,
					kappa,
					true)
					end
				else begin 
					(state,"",astd,state,[],[],false) 
					end
			end
	end

  |ASTD_state.Choice_s (side,state2) -> 
	begin debug ("choice exec "^(ASTD_astd.get_name astd));
	let (name,astd1,astd2)=ASTD_astd.get_data_choice astd
	in if side = ASTD_state.Right
		then begin
			let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event env  (current_key^"/"^(ASTD_astd.get_name astd2) )
			in if is_modified
				then (delta,key,sub_astd,ASTD_state.Choice_s (side,mod_state),to_save,kappa,is_modified)
				else (state,"",astd,state,[],[],false) 
			end
		else begin
			if side=ASTD_state.Left
			then begin
				let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute state2 astd1 event env  (current_key^"/"^(ASTD_astd.get_name astd1) )
				in if is_modified
					then (delta,key,sub_astd,ASTD_state.Choice_s (side,mod_state),to_save,kappa,is_modified)
					else (state,"",astd,state,[],[],false) 
				end
			else begin
				let (_,_,_,mod_state,to_save1,kappa1,is_modified) = execute ( init astd1) astd1 event env (current_key^"/"^(ASTD_astd.get_name astd1) )
				in if is_modified
					then (ASTD_state.Choice_s (ASTD_state.Left,mod_state),current_key,astd,ASTD_state.Choice_s (ASTD_state.Left,mod_state),to_save1,kappa1,is_modified)
					else begin
						let (_,_,_,mod_state2,to_save2,kappa2,is_modified2) = execute ( init astd2) astd2 event env (current_key^"/"^(ASTD_astd.get_name astd2) ) 
						in if is_modified2
							then (ASTD_state.Choice_s (ASTD_state.Right,mod_state2),current_key,astd,ASTD_state.Choice_s(ASTD_state.Right ,mod_state2),to_save2 ,kappa2,is_modified2)
							else (state,"",astd,state,[],[],false) 
						end
				end
			end
	end

  |ASTD_state.Kleene_s (started,state2) ->  
	begin debug ("kleene exec "^(ASTD_astd.get_name astd));
	let (name,astd2)=ASTD_astd.get_data_kleene astd
	in let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event env (current_key^"/"^(ASTD_astd.get_name astd2))
	in if started 
		then if is_modified
			then begin debug ("kleene modified "^name);
				if (mod_state=( init astd2))
				then begin
					(ASTD_state.Kleene_s (false,mod_state),current_key,astd,ASTD_state.Kleene_s (false,mod_state),to_save,kappa,true)
					end
				else (delta,key,sub_astd,ASTD_state.Kleene_s (started,mod_state),to_save,kappa,is_modified)
				end
			else let (new_state,isfinal)=(is_final astd2 state2 env [] (current_key^"/"^(ASTD_astd.get_name astd2) ))
				in if isfinal
					then begin debug ("restart kleene "^name); 
						let (delta2,key2,sub_astd2,mod_state2,to_save2,kappa2,is_modified2) = execute ( init astd2) astd2 event  env  (current_key^"/"^(ASTD_astd.get_name astd2) )
						in if is_modified2 
							then (delta2,key2,sub_astd2,ASTD_state.Kleene_s (started,mod_state2),to_save2,kappa2,is_modified2)
							else (state,"",astd,state,[],[],false) 
						end
					else begin debug ("restart impossible kleene "^name);
						(state,"",astd,state,[],[],false)
						end
		else begin debug ("kleene not started "^name);
			if is_modified 
			then (ASTD_state.Kleene_s (true,mod_state),current_key,astd,ASTD_state.Kleene_s (true,mod_state),to_save,kappa,is_modified)
			else (state,"",astd,state,[],[],false)
			end
	end

  |ASTD_state.Synchronisation_s (state1,state2) -> 
	begin debug ("synch exec "^(ASTD_astd.get_name astd));
	let (name,transition_list,astd1,astd2)=ASTD_astd.get_data_synchronisation astd
	in if List.mem (ASTD_event.get_label event) transition_list
		then begin 
			let (_,_,_,mod_state,to_save1,kappa1,is_modified) = execute state1 astd1 event env  (current_key^"/"^(ASTD_astd.get_name astd1) )
			and (_,_,_,mod_state2,to_save2,kappa2,is_modified2) = execute state2 astd2 event env  (current_key^"/"^(ASTD_astd.get_name astd2) )
			in if is_modified && is_modified2
				then (ASTD_state.Synchronisation_s (mod_state,mod_state2),current_key,astd,ASTD_state.Synchronisation_s (mod_state,mod_state2),to_save1@to_save2,kappa1@kappa2,true)
				else (state,"",astd,state,[],[],false) 
			end
		else begin 
			let (delta,key1,sub_astd1,mod_state,to_save,kappa,is_modified) = execute state1 astd1 event env (current_key^"/"^(ASTD_astd.get_name astd1) )
			in if is_modified 
				then begin 
					(delta,key1,sub_astd1,ASTD_state.Synchronisation_s (mod_state,state2),to_save,kappa,true)
					end
				else begin 
					let (delta2,key2,sub_astd2,mod_state2,to_save2,kappa2,is_modified2) = execute state2 astd2 event env (current_key^"/"^(ASTD_astd.get_name astd2) )  
					in if is_modified2
						then (delta2,key2,sub_astd2,ASTD_state.Synchronisation_s (mod_state,mod_state2),to_save2,kappa2,true)
						else (state,"",astd,state,[],[],false) 
					end
			end
	end           

  |ASTD_state.Guard_s (started,state2) -> 
	begin debug ("guard exec "^(ASTD_astd.get_name astd));
	let (name,pred_list,astd2)=ASTD_astd.get_data_guard astd
	in if started 
		then let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event env (current_key^"/"^(ASTD_astd.get_name astd2) )
			in (delta,key,sub_astd,ASTD_state.Guard_s (started,mod_state),to_save,kappa,is_modified)
		else if (ASTD_arrow.evaluate_guard env pred_list)
			then let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event env (current_key^"/"^(ASTD_astd.get_name astd2) )
				in (ASTD_state.Guard_s (true,mod_state),current_key,astd,ASTD_state.Guard_s (true,mod_state),to_save,kappa,is_modified)
			else (state,"",astd,state,[],[],false)
	end



  |ASTD_state.QChoice_s (val_used,final_dom,unknown_dom,state2) -> 
	begin debug ("qchoice exec "^(ASTD_astd.get_name astd));
	let (name,var,list_val,dep,astd2)=ASTD_astd.get_data_qchoice astd
	in if val_used=ASTD_state.ChoiceNotMade
		then begin debug "trying some values";
			let (direct_val,indeterminist)=(get_values event astd2 (ASTD_term.Var var))
			 in try_qchoice dep (ASTD_event.get_label event) state2 astd2 var event env list_val direct_val indeterminist current_key
			end
					
		else begin 
			debug ("qchoice "^var^" choice using "^(ASTD_term.string_of(ASTD_state.get_val val_used))^" for the astd "^name);
				let bind_env = ASTD_environment.bind var (ASTD_state.get_val val_used)
				in let env2=(ASTD_environment.add_binding bind_env env)
				in let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event env2 (current_key^":"^(ASTD_constant.string_of (ASTD_term.extract_constant_from_term (ASTD_state.get_val val_used)))^"/"^(ASTD_astd.get_name astd2)) 	
				in (delta,key,sub_astd,
					ASTD_state.QChoice_s (val_used,final_dom,unknown_dom,mod_state),
					to_save,
					(ASTD_optimisation.automatic_gestion_of_kappa_values env (ASTD_event.get_label event) dep (ASTD_state.get_val val_used))@kappa, 					is_modified)
				
			end
	end 

  |ASTD_state.QSynchronisation_s (not_fin_dom,unknown_dom,not_init_dom,init_state) -> 
	begin debug ("qsynch exec "^(ASTD_astd.get_name astd));
	let (name,var,val_list,trans_list,opt,astd2)=ASTD_astd.get_data_qsynchronisation astd
	in try begin
                        let value2 = active_optimisation event astd env (ASTD_event.get_label event) var opt 
			in if not(value2=ASTD_state.ChoiceNotMade)
				then begin debug ("kappa indirect "^var^" "^(ASTD_term.string_of(ASTD_state.get_val value2))^" for the astd "^name);
				if (List.mem (ASTD_event.get_label event) trans_list) || (not(ASTD_constant.is_included (ASTD_term.extract_constant_from_term (ASTD_state.get_val value2)) val_list))
					then (state,"",astd,state,[],[],false)
					else begin 
						let bind_env = ASTD_environment.bind var (ASTD_state.get_val value2)
						in let env2=(ASTD_environment.add_binding bind_env env)
						and value=ASTD_term.extract_constant_from_term (ASTD_state.get_val value2)
						in let value'=ASTD_constant.value_of value
						in let get_state=get_synch_bdd not_init_dom init_state current_key (Some value) astd2  
						in let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute get_state astd2 event env2  (current_key^":"^(ASTD_constant.string_of (ASTD_term.extract_constant_from_term (ASTD_state.get_val value2)))^"/"^(ASTD_astd.get_name astd2)) 		
						in begin 
							if is_modified
							then let (new_state,isfinal)=(is_final astd2 mod_state env2 [] (current_key^":"^(ASTD_constant.string_of (ASTD_term.extract_constant_from_term (ASTD_state.get_val value2)))^"/"^(ASTD_astd.get_name astd2)) )
								in begin debug "kappa indirect !!!!!!!!!!";
									if isfinal
									then let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.insert value' not_init_dom),init_state)
										in (new_study_state,
											current_key,
											astd,
											new_study_state,
											((current_key,Some value),astd2,new_state,true)::to_save,
											kappa,
											is_modified)
	
									else let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.insert value' not_init_dom),init_state)
										in (new_study_state,
											current_key,
											astd,
											new_study_state,
											((current_key,Some value),astd2,new_state,true)::to_save,
											kappa,
											is_modified)
								end

							else begin 
							(state,current_key,astd,state,[],[],is_modified)
								end
							end
						end

					end
				else begin debug ("no optimisation indirect kappa in "^name^" for "^(ASTD_event.get_label event) );
					if List.mem (ASTD_event.get_label event) trans_list
					then let (list_modif,modif_possible)=modif_all_qsynch name astd2 event init_state not_init_dom val_list env [] var  current_key
						in if modif_possible 
							then begin 
								apply_all_modifs name var astd2 env  not_init_dom current_key list_modif (state,"",astd,state,[],[],false)
								end
							else (state,"",astd,state,[],[],false)
					else let (direct_val,indeterminist)= (get_values event astd2 (ASTD_term.Var var))
						in try_qsynch name state astd2 var event env val_list direct_val trans_list val_list indeterminist current_key
					end		
	end 

	with _ -> (state,"",astd,state,[],[],false)
	end 


  |ASTD_state.Call_s (called,state2) -> 
	begin debug ("call exec "^(ASTD_astd.get_name astd));
	let (name,called_name,fct_vec)=ASTD_astd.get_data_call astd
	in let astd2=(ASTD_astd.call_astd called_name (ASTD_environment.increase_call env fct_vec))
	in if called 
		then begin 
			let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event (ASTD_environment.increase_call env fct_vec) (current_key^"/"^(ASTD_astd.get_name astd2) )
			in (delta,key,sub_astd,ASTD_state.Call_s (called,mod_state),to_save,kappa,is_modified)
			end
		else begin 
			let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute ( init astd2) astd2 event (ASTD_environment.increase_call env fct_vec) (current_key^"/"^(ASTD_astd.get_name astd2) )
			in (ASTD_state.Call_s (true,mod_state),current_key,astd,ASTD_state.Call_s (true,mod_state),to_save,kappa,is_modified)
			end
	end


  |_ -> begin (state,"",astd,state,[],[],false) end


and try_qchoice dep label state astd var event env list_val kappa_dir_val indeterminist current_key=

if kappa_dir_val=[]
	then if indeterminist
		then if not (list_val = (ASTD_constant.empty_dom))
			then begin debug "then trying global values";
			let (head_val,tail)= ASTD_constant.head_tail list_val
			in let bind_env = ASTD_environment.bind var (ASTD_term.Const head_val)
			in let env2=(ASTD_environment.add_binding bind_env env)
			in let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute state astd event env2 (current_key^":"^(ASTD_constant.string_of head_val)^"/"^(ASTD_astd.get_name astd))
			in if is_modified
				then begin 
					(ASTD_state.QChoice_s (ASTD_state.val_of (ASTD_term.Const head_val),(ASTD_constant.empty_dom),(ASTD_constant.empty_dom),mod_state),
					current_key,
					astd,
					ASTD_state.QChoice_s (ASTD_state.val_of (ASTD_term.Const head_val),(ASTD_constant.empty_dom),(ASTD_constant.empty_dom),mod_state),
					to_save,
					(ASTD_optimisation.automatic_gestion_of_kappa_values env label dep (ASTD_term.Const head_val))@kappa,
					is_modified)
					end
				else try_qchoice dep label state astd var event env tail [] indeterminist current_key
			end
			else (state,"",astd,state,[],[],false)
		else (state,"",astd,state,[],[],false)
	else begin debug "trying first kappa values";
		if (not(ASTD_constant.is_included (ASTD_term.extract_constant_from_term (List.hd kappa_dir_val)) list_val))
		then try_qchoice dep label state astd var event env list_val (List.tl kappa_dir_val) indeterminist current_key
		else let head_val=List.hd kappa_dir_val
			in let bind_env = ASTD_environment.bind var (head_val)
			in let env2=(ASTD_environment.add_binding bind_env env)
			in let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute state astd event env2  (current_key^":"^(ASTD_constant.string_of (ASTD_term.extract_constant_from_term head_val))^"/"^(ASTD_astd.get_name astd))
			in if is_modified
				then begin debug "kappa direct !!!!!!!!!!";
					(ASTD_state.QChoice_s (ASTD_state.val_of (head_val),(ASTD_constant.empty_dom),(ASTD_constant.empty_dom),mod_state),
					current_key,
					astd,
					ASTD_state.QChoice_s (ASTD_state.val_of (head_val),(ASTD_constant.empty_dom),(ASTD_constant.empty_dom),mod_state),
					to_save,
					(ASTD_optimisation.automatic_gestion_of_kappa_values env label dep (head_val))@kappa,
					is_modified)
					end
				else try_qchoice dep label state astd var event env list_val (List.tl kappa_dir_val) indeterminist current_key
		end






and try_qsynch name state astd var event env list_val kappa_dir_val trans_list dom indeterminist current_key=
	if kappa_dir_val=[]
	then if indeterminist
		then if list_val <> (ASTD_constant.empty_dom)
			then begin debug "then try dom for qsynch";
			let (head_val,tail)= ASTD_constant.head_tail list_val
			and (not_fin_dom, unknown_dom, not_init_dom,init_state) = ASTD_state.get_data_from_qsynchro state
			in let value=ASTD_constant.value_of head_val
			in let bind_env = ASTD_environment.bind var (ASTD_term.Const head_val)
			in let env2=(ASTD_environment.add_binding bind_env env)
			in let sub_state=get_synch_bdd not_init_dom init_state current_key (Some head_val) astd   
			in let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute sub_state astd event env2   (current_key^":"^(ASTD_constant.string_of head_val)^"/"^(ASTD_astd.get_name astd))
			in if is_modified
				then let (new_state,isfinal)=(is_final astd mod_state env2 [] (current_key^":"^(ASTD_constant.string_of head_val)^"/"^(ASTD_astd.get_name astd)))
					in begin 
						if isfinal
						then let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.insert value not_init_dom),init_state)
							in (new_study_state,current_key,astd,new_study_state,((current_key,Some head_val),astd, new_state, true)::to_save,kappa,is_modified)
						else let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.insert value not_init_dom),init_state)
							in (new_study_state,current_key,astd,new_study_state,((current_key, Some head_val),astd, new_state, true)::to_save,kappa,is_modified)
						end

				else try_qsynch name state astd var event env tail [] trans_list dom indeterminist current_key
			end
			else (state,"",astd,state,[],[],false)
		else (state,"",astd,state,[],[],false)
	else begin debug " try kappa dir for qsynch ";
			let head_val=(ASTD_term.extract_constant_from_term (List.hd kappa_dir_val))
			and (not_fin_dom, unknown_dom, not_init_dom,init_state) = ASTD_state.get_data_from_qsynchro state
			in let value=ASTD_constant.value_of (head_val)
		in if (List.mem (ASTD_event.get_label event) trans_list) || (not(ASTD_constant.is_included (ASTD_term.extract_constant_from_term (List.hd kappa_dir_val)) dom))
		then try_qsynch name state astd var event env list_val (List.tl kappa_dir_val) trans_list dom indeterminist current_key
		else let bind_env = ASTD_environment.bind var (List.hd kappa_dir_val)
			in let env2=(ASTD_environment.add_binding bind_env env)
			in let sub_state=get_synch_bdd not_init_dom init_state current_key (Some head_val) astd  
			in let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute sub_state astd event env2   (current_key^":"^(ASTD_constant.string_of head_val)^"/"^(ASTD_astd.get_name astd))
			in if is_modified
				then let (new_state,isfinal)=(is_final astd mod_state env2 [] (current_key^":"^(ASTD_constant.string_of head_val)^"/"^(ASTD_astd.get_name astd)))
					in begin debug "kappa direct !!!!!!!!!!";
						if isfinal
						then let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.insert value not_init_dom),init_state)
							in (new_study_state,current_key,astd,new_study_state,((current_key, Some head_val) ,astd, new_state, true)::to_save,kappa,is_modified)
						else let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.insert value not_init_dom),init_state)
							in (new_study_state,current_key,astd,new_study_state,((current_key, Some head_val) ,astd, new_state, true)::to_save,kappa,is_modified)
						end
				else try_qsynch name state astd var event env list_val (List.tl kappa_dir_val) trans_list dom indeterminist current_key
		end


and modif_all_qsynch name astd event init_state not_init_dom value_list env modifs var current_key=
	if value_list<>ASTD_constant.empty_dom
		then let (val_used, tail)= ASTD_constant.head_tail value_list
			in let bind_env = ASTD_environment.bind var (ASTD_term.Const ( val_used))
			in let env2=(ASTD_environment.add_binding bind_env env)
			in if ASTD_constant.is_included val_used not_init_dom
				then begin 
					let state=get_bdd current_key (Some val_used) astd
					in let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute state astd event env2 (current_key^":"^(ASTD_constant.string_of val_used)^"/"^(ASTD_astd.get_name astd))
						in let modif=(val_used,mod_state,to_save,kappa)
						in if is_modified 
							then modif_all_qsynch name astd event init_state not_init_dom tail env (modif::modifs) var current_key
							else ([],false)
					end
				else begin 
					let (delta,key,sub_astd,mod_state,to_save,kappa,is_modified) = execute init_state astd event env2 (current_key^":"^(ASTD_constant.string_of val_used)^"/"^(ASTD_astd.get_name astd))
						in let modif=(val_used,mod_state,to_save,kappa)
						in if is_modified 
							then modif_all_qsynch name astd event init_state not_init_dom tail env (modif::modifs) var current_key
							else ([],false)
					end
		else begin 
			(modifs,true)
			end








(**Affichage*)
let rec print state astd s current_key= match state with
        |ASTD_state.Automata_s (a,b,c) ->print_newline();
                              print_endline(s^"Automata_s ,"^(ASTD_astd.get_name astd));
                              print_endline(s^"//StartHistory");
                              (print_h astd b (s^"//")) current_key;
                              print_endline(s^"sub_state : "^a);
                              print c (ASTD_astd.find_subastd a (ASTD_astd.get_sub astd)) (s^"   ") (current_key^"/"^a)
        |ASTD_state.Sequence_s (a,b) ->print_newline();print_endline(s^"Sequence_s ,");print_endline(s^"step : "^(ASTD_state.string_of_seq a));
               begin if a=ASTD_state.Fst 
				then print b (ASTD_astd.get_seq_l astd) (s^"   ")  (current_key^"/"^(ASTD_astd.get_name (ASTD_astd.get_seq_l astd)))
                               	else print b (ASTD_astd.get_seq_r astd) (s^"   ") (current_key^"/"^(ASTD_astd.get_name (ASTD_astd.get_seq_r astd)))
               end
        |ASTD_state.Choice_s (a,b) ->print_newline();print_endline(s^"Choice_s ,");print_endline(s^"step : "^(ASTD_state.string_of_choice a));
               begin if a=ASTD_state.Undef then print_endline (s^"No choice made")
                                        else if a=ASTD_state.Left then print b (ASTD_astd.get_choice1 astd) (s^"   ") (current_key^"/"^(ASTD_astd.get_name (ASTD_astd.get_choice1 astd)))
                                                      else print b (ASTD_astd.get_choice2 astd)(s^"   ") (current_key^"/"^(ASTD_astd.get_name (ASTD_astd.get_choice2 astd)))
               end
        |ASTD_state.Kleene_s (a,b) ->print_newline();print_endline(s^"Kleene_s ,");print_endline(s^"started ? : "^(string_of_bool a)); 
                          print b (ASTD_astd.get_astd_kleene astd) (s^"   ") (current_key^"/"^(ASTD_astd.get_name (ASTD_astd.get_astd_kleene astd)))
        |ASTD_state.Synchronisation_s (a,b) ->print_newline();print_endline(s^"Synchronisation_s ,");
                                   print a (ASTD_astd.get_synchro_astd1 astd) (s^"   ") (current_key^"/"^(ASTD_astd.get_name (ASTD_astd.get_synchro_astd1 astd))) ;
                                   print b (ASTD_astd.get_synchro_astd2 astd) (s^"   ") (current_key^"/"^(ASTD_astd.get_name (ASTD_astd.get_synchro_astd2 astd)))
        |ASTD_state.QChoice_s (a,final_dom,unknown_dom,b) ->print_newline();print_endline(s^"QChoice_s ,");
                                      begin 
					let (n,o,p,q,r)= ASTD_astd.get_data_qchoice astd
                                        in if a=ASTD_state.ChoiceNotMade 
                                           then begin print_endline(s^"Value Not Chosen // Possible values: "^(ASTD_constant.print_dom p ))
                                                end
                                           else begin print_endline(s^"chosen value : "^(ASTD_state.string_of_qchoice a)^" for  qchoice "^o);
                                                      print b (ASTD_astd.get_qastd astd) (s^"   ") (current_key^":"^(ASTD_state.string_of_qchoice a)^"/"^(ASTD_astd.get_name r))
                                                end
                                      end;
        |ASTD_state.QSynchronisation_s (not_fin,unknown,not_init_dom,init_state) -> print_newline();print_endline(s^"QSynchronisation_s ,Not_Initial values:  "^(ASTD_constant.print_dom not_init_dom));
                                           (print_synch (ASTD_astd.get_qastd astd) (s^"   ") not_init_dom (ASTD_astd.get_qvar astd) current_key)
        |ASTD_state.Guard_s (a,b) ->print_newline();print_endline(s^"Guard_s ,");print_endline(s^"started ? : "^(string_of_bool a));
                         print b (ASTD_astd.get_guard_astd astd) (s^"   ") (current_key^"/"^(ASTD_astd.get_name (ASTD_astd.get_guard_astd astd)))
        |ASTD_state.Call_s (a,b) ->print_newline();print_endline(s^"Call_s ,");print_endline(s^"started ? : "^(string_of_bool a));
                        print b (ASTD_astd.get_astd (ASTD_astd.get_called_name astd)) (s^"   ") (current_key^"/"^(ASTD_astd.get_called_name astd))
        |ASTD_state.NotDefined ->print_endline (s^"End of the state")
        |ASTD_state.Elem -> print_endline(s^"Elem")


and print_h astd hist s current_key= match hist with
  |(n1,h)::t ->print_endline(s^n1^" @ "^current_key);
               print h (ASTD_astd.find_subastd n1 (ASTD_astd.get_sub astd)) (s) (current_key^"/"^n1);
               print_h astd  t s current_key
  |[]->print_endline(s^"EndHistory")


and print_synch sub_astd s not_init var current_key =
	if ASTD_constant.is_empty_dom not_init 
		then print_newline ()
		else let (value,t)=ASTD_constant.head_tail not_init
			in begin 
				print_newline ();
				print_endline (s^"Value "^(ASTD_constant.string_of value)^" for qsynch "^var ); 
				print (get_bdd current_key (Some value) sub_astd)
					(sub_astd)
					s
					(current_key^":"^(ASTD_constant.string_of value)^"/"^(ASTD_astd.get_name sub_astd));
				print_endline (s^"end");
				print_synch sub_astd s t var current_key
				end
;;








(**Exec event list*)

let compteur=
  let n= ref 0 
      in function () -> 
                n:=!n+1;
                print_endline ("event number "^(string_of_int !n))
;;

let rec execute_event_list affichage state astd event_list = match event_list with
	|event::tail-> if affichage<>1 
			then 
			begin 
			print_endline "================================================================";
			print_endline ("Execution of : "^(ASTD_label.string_of(ASTD_event.get_label event)));
			let (delta,key,delta_astd,new_state,to_save,kappa,is_modified) = execute state astd event ASTD_environment.empty "Main"
			in begin 
				if is_modified 
				then if affichage=2
					then 
						begin compteur();
						save_synch_data to_save;
						ASTD_optimisation.apply_each_mod kappa
						end
					else
						begin 
						save_synch_data to_save;
						ASTD_optimisation.apply_each_mod kappa;
						print new_state astd "" "Main"
						end
				else begin
					print_endline ("Execution of : "^(ASTD_label.string_of(ASTD_event.get_label event))^" not possible")
					end
				end;
				begin
				if is_modified
					then execute_event_list affichage new_state astd tail
					else execute_event_list affichage state astd tail
				end
			end
			else
			begin 
			let (delta,key,delta_astd,new_state,to_save,kappa,is_modified)=execute state astd event ASTD_environment.empty "Main"
			in begin 
				if is_modified 
				then begin 
					save_synch_data to_save;
					ASTD_optimisation.apply_each_mod kappa
					end
				else begin
					print_endline ("Execution of : "^(ASTD_label.string_of(ASTD_event.get_label event))^" not possible");
					print_endline "======================================================"
					end
				end;
				execute_event_list affichage new_state astd tail
			end
	|[]->begin state end



