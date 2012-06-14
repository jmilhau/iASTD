type state_to_modify=ASTD_state.t
type modified_state=ASTD_state.t

type astd_name = string
type called_path = astd_name list


 
    let val_debug = false;;
    let debug m = if (val_debug) 
                            then (print_endline m )
                            else begin end;;


let rec apply_all_modifs name var astd2 env call_path modifs study_state= 
	if modifs <> []
	then let (const,(delta,sub_path,new_state,to_save,kappa,is_modified))= List.hd modifs
		and (_,path,state1,save,kappa2,_)=study_state
		in let (not_fin_dom,unknown_dom,not_init_dom,init)=ASTD_state.get_data_from_qsynchro state1
		in let bind_env = ASTD_environment.bind var (ASTD_term.Const const)
		in let env2=(ASTD_environment.add_binding bind_env env)
		in let value = ASTD_constant.value_of const
		in let (new_state2,isfinal)=(ASTD_state.is_final astd2 new_state env2 call_path)
		in if (new_state2=init)
			then begin 
			if isfinal
			then begin
				let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.remove value not_init_dom),init)
				in apply_all_modifs name var astd2 env call_path (List.tl modifs) (new_study_state,path,new_study_state,((name,const,env2,call_path),new_state,false)::(to_save@save),kappa@kappa2,true)
				end
			else begin
				let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.remove value not_init_dom),init)
				in apply_all_modifs name var astd2 env call_path (List.tl modifs) (new_study_state,path,new_study_state,((name,const,env2,call_path),new_state,false)::(to_save@save),kappa@kappa2,true)
				end
			end
			else begin 
			if isfinal
			then begin
				let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.insert value not_init_dom),init);
				in apply_all_modifs name var astd2 env call_path (List.tl modifs) (new_study_state,path,new_study_state,((name,const,env2,call_path),new_state,true)::(to_save@save),kappa@kappa2,true)
				end
			else begin
				let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.insert value not_init_dom),init);
				in apply_all_modifs name var astd2 env call_path (List.tl modifs) (new_study_state,path,new_study_state,((name,const,env2,call_path),new_state,true)::(to_save@save),kappa@kappa2,true)
				end
			end
	else begin 
		 study_state
		end



let rec find_arrow arrow_list event current sub_state sub_astd env call_path = match arrow_list with
	| ASTD_arrow.Local ( from,to_state,transition,pred,final )::a::t -> 
		if (current=from)&&((ASTD_event.get_label event) = (ASTD_transition.get_label transition))
			then if final
				then begin 
					let (new_state,is_final)=(ASTD_state.is_final sub_astd sub_state env call_path)
					in if is_final
					then begin 
						if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
						then (ASTD_arrow.Local ( from,to_state,transition,pred,final ),new_state,true)
						else find_arrow (a::t) event current new_state sub_astd env call_path
						end
					else find_arrow (a::t) event current new_state sub_astd env call_path
					end
				else begin 
					if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
					then (ASTD_arrow.Local ( from,to_state,transition,pred,final ),sub_state,true)
					else find_arrow (a::t) event current sub_state sub_astd env call_path
					end
			else find_arrow (a::t) event current sub_state sub_astd env call_path

	| ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final )::a::t ->
		if (through=current) &&((ASTD_event.get_label event) = (ASTD_transition.get_label transition)) && (ASTD_astd.is_automata sub_astd)
			then let (sub_from,hist,sub_state2) =ASTD_state.get_data_automata_s sub_state
				in if from=sub_from
					then if final
						then let (new_sub_state,is_final)=(ASTD_state.is_final (ASTD_astd.find_subastd sub_from (ASTD_astd.get_sub sub_astd)) sub_state2 env call_path)
							in let new_state = ASTD_state.Automata_s (sub_from,hist,new_sub_state)
							in if is_final 
							then if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
								then (ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final ),new_state,true)
								else find_arrow (a::t) event current new_state sub_astd env call_path
							else find_arrow (a::t) event current new_state sub_astd env call_path
						else if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
							then (ASTD_arrow.From_sub ( from,to_state,through,transition,pred,final ),sub_state,true)
							else find_arrow (a::t) event current sub_state sub_astd env call_path
					else find_arrow (a::t) event current sub_state sub_astd env call_path
			else find_arrow (a::t) event current sub_state sub_astd env call_path

	| ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final )::a::t -> 
		if (current=from)&&((ASTD_event.get_label event) = (ASTD_transition.get_label transition))
			then if final
				then let (new_state,is_final)= ASTD_state.is_final sub_astd sub_state env call_path
					in if is_final
					then if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
						then (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),new_state,true)
						else find_arrow (a::t) event current new_state sub_astd env call_path
					else find_arrow (a::t) event current new_state sub_astd env call_path
				else if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
					then (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),sub_state,true)
					else find_arrow (a::t) event current sub_state sub_astd env call_path
			else find_arrow (a::t) event current sub_state sub_astd env call_path

	| ASTD_arrow.Local ( from,to_state,transition,pred,final )::[] ->
		if (current=from)&&((ASTD_event.get_label event) = (ASTD_transition.get_label transition))
			then if final
				then begin 
					let (new_state,is_final)=(ASTD_state.is_final sub_astd sub_state env call_path)
					in if is_final
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
						then let (new_sub_state,is_final)=(ASTD_state.is_final (ASTD_astd.find_subastd sub_from (ASTD_astd.get_sub sub_astd)) sub_state2 env call_path)
							in let new_state = ASTD_state.Automata_s (sub_from,hist,new_sub_state)
							in if is_final 
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
				then let (new_state,is_final)=(ASTD_state.is_final sub_astd sub_state env call_path)
					in if is_final
					then if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
						then (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),new_state,true)
						else (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),new_state,false)
					else (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),new_state,false)
				else if (ASTD_arrow.valid_arrow event env (List.hd arrow_list))
					then (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),sub_state,true)
					else (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),sub_state,false)
			else (ASTD_arrow.To_sub ( from,to_state,through,transition,pred,final ),sub_state,false)
	|[]->failwith "an automata should have at least one transition"



let rec find_corresponding_params label trans_list = match trans_list with
	|trans::tail-> if label=ASTD_transition.get_label trans
				then let params= ASTD_transition.get_params trans
					in if params=[]
						then find_corresponding_params label tail
						else params::(find_corresponding_params label tail)
				else find_corresponding_params label tail
	|[]->[]

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
	in let param_trans=find_corresponding_params label trans_list 
	in begin debug "params extracted" ;
		if param_trans<>[] 
		then find_value c_list param_trans var c_list []
			
		else []
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
					let qchoice=(get_values event astd (ASTD_term.Var var))
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








let rec execute state astd event path env call_path = 
let label_list = List.map (ASTD_transition.get_label) (ASTD_astd.get_sub_transitions [] astd)
in if (not(List.mem (ASTD_event.get_label event) label_list) )
then (state,[],state,[],[],false) 
else match state with 

  |ASTD_state.Automata_s (name_current,hist,state2) -> 
	begin debug ("aut exec "^(ASTD_astd.get_name astd));
	let  (name,sub_astd,arrow_list,s_final,d_final,init)=ASTD_astd.get_data_automata astd 
	in let astd2 = (ASTD_astd.find_subastd name_current sub_astd)
	in if List.mem (ASTD_event.get_label event) (List.map (ASTD_transition.get_label) (List.map ASTD_arrow.get_transition arrow_list))
		then let (arrow,new_state,found)=find_arrow arrow_list event name_current state2 (ASTD_astd.find_subastd name_current sub_astd) env call_path
			in if found 
				then let new_hist = begin if ASTD_state.is_automata new_state
							then (ASTD_state.modify_h hist name_current new_state) 
							else hist
							end
					in if ASTD_arrow.is_to_sub arrow
						then let dest=ASTD_arrow.get_to arrow
							and through = ASTD_arrow.get_through arrow
							in let new_sub_astd = (ASTD_astd.find_subastd through sub_astd)
							in let new_sub_state = ASTD_state.Automata_s (through,new_hist,ASTD_state.goto_automata new_sub_astd dest new_hist)
							in (new_sub_state,((path)@([name])),new_sub_state,[],[],true)
						else let dest=ASTD_arrow.get_to arrow
							in let new_sub_astd = (ASTD_astd.find_subastd dest sub_astd)
							in let new_sub_state = ASTD_state.Automata_s (dest,new_hist,ASTD_state.init (new_sub_astd))
							in (new_sub_state,((path)@([name])),new_sub_state,[],[],true)
				else let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute new_state astd2 event ((path)@([name])) env call_path
					in (delta,sub_path,ASTD_state.Automata_s (name_current,hist,mod_state),to_save,kappa,is_modified)
		else let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event ((path)@([name])) env call_path
			in (delta,sub_path,ASTD_state.Automata_s (name_current,hist,mod_state),to_save,kappa,is_modified)
	end


  |ASTD_state.Sequence_s (step,state2) -> 
	begin debug ("seq exec "^(ASTD_astd.get_name astd));
	let (name,left,right)=ASTD_astd.get_data_sequence astd
	in if step = ASTD_state.Fst
		then begin 
			let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state2 left event ((path)@([name])) env call_path
			in if is_modified 
				then begin
					(delta,
					sub_path,
					ASTD_state.Sequence_s (step,mod_state),
					to_save,
					kappa,
					true)
					end
				else let (new_state,is_final)=(ASTD_state.is_final left mod_state env call_path)
					in if is_final
						then begin
							let (delta2,sub_path2,mod_state2,to_save2,kappa2,is_modified2)=execute (ASTD_state.init right) right event ((path)@([name])) env call_path
							in if is_modified2
								then begin
									(ASTD_state.Sequence_s (ASTD_state.Snd,mod_state2),
									((path)@([name])),
									ASTD_state.Sequence_s (ASTD_state.Snd,mod_state2),
									to_save,
									kappa2,
									true)
									end
								else begin 
									(state,[],ASTD_state.Sequence_s (step,new_state),[],[],false) 
									end
							end
						else (state,[],ASTD_state.Sequence_s (step,new_state),[],[],false)
			end
		else begin
			let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state2 right event ((path)@([name])) env call_path
			in if is_modified 
				then begin
					(delta,
					sub_path,
					ASTD_state.Sequence_s (step,mod_state),
					to_save,
					kappa,
					true)
					end
				else begin 
					(state,[],ASTD_state.Sequence_s (step,mod_state),[],[],false) 
					end
			end
	end

  |ASTD_state.Choice_s (side,state2) -> 
	begin debug ("choice exec "^(ASTD_astd.get_name astd));
	let (name,first,second)=ASTD_astd.get_data_choice astd
	in if side = ASTD_state.Right
		then begin
			let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state2 first event ((path)@([name])) env call_path
			in if is_modified
				then (delta,sub_path,ASTD_state.Choice_s (side,mod_state),to_save,kappa,is_modified)
				else (state,[],ASTD_state.Choice_s (side,mod_state),[],[],false) 
			end
		else begin
			if side=ASTD_state.Left
			then begin
				let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state2 second event ((path)@([name])) env call_path
				in if is_modified
					then (delta,sub_path,ASTD_state.Choice_s (side,mod_state),to_save,kappa,is_modified)
					else (state,[],ASTD_state.Choice_s (side,mod_state),[],[],false) 
				end
			else begin
				let (_,_,mod_state,to_save1,kappa1,is_modified) = execute (ASTD_state.init first) first event ((path)@([name])) env call_path
				in if is_modified
					then (ASTD_state.Choice_s (ASTD_state.Left,mod_state),((path)@([name])),ASTD_state.Choice_s (ASTD_state.Left,mod_state),to_save1,kappa1,is_modified)
					else begin
						let (_,_,mod_state2,to_save2,kappa2,is_modified2) = execute (ASTD_state.init second) second event ((path)@([name])) env call_path
						in if is_modified2
							then (ASTD_state.Choice_s (ASTD_state.Right,mod_state2),((path)@([name])),ASTD_state.Choice_s(ASTD_state.Right ,mod_state2),to_save2 ,kappa2,is_modified2)
							else (state,[],state,[],[],false) 
						end
				end
			end
	end

  |ASTD_state.Kleene_s (started,state2) ->  
	begin debug ("kleene exec "^(ASTD_astd.get_name astd));
	let (name,astd2)=ASTD_astd.get_data_kleene astd
	in let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event ((path)@([name])) env call_path
	in if started 
		then if is_modified
			then begin debug ("kleene modified "^name);
				if (mod_state=(ASTD_state.init astd2))
				then begin
					(ASTD_state.Kleene_s (false,mod_state),((path)@([name])),ASTD_state.Kleene_s (false,mod_state),to_save,kappa,true)
					end
				else (delta,sub_path,ASTD_state.Kleene_s (started,mod_state),to_save,kappa,is_modified)
				end
			else let (new_state,isfinal)=(ASTD_state.is_final astd2 state2 env call_path)
				in if isfinal
					then begin debug ("restart kleene "^name); 
						let (delta2,sub_path2,mod_state2,to_save2,kappa2,is_modified2) = execute (ASTD_state.init astd2) astd2 event ((path)@([name])) env call_path
						in if is_modified2 
							then (delta2,sub_path2,ASTD_state.Kleene_s (started,mod_state2),to_save2,kappa2,is_modified2)
							else (state,[],state,[],[],false) 
						end
					else begin debug ("restart impossible kleene "^name);
						(state,[],ASTD_state.Kleene_s (started,state2),[],[],false)
						end
		else begin debug ("kleene not started "^name);
			if is_modified 
			then (ASTD_state.Kleene_s (true,mod_state),((path)@([name])),ASTD_state.Kleene_s (true,mod_state),to_save,kappa,is_modified)
			else (state,((path)@([name])),ASTD_state.Kleene_s (false,mod_state),[],[],false)
			end
	end

  |ASTD_state.Synchronisation_s (state1,state2) -> 
	begin debug ("synch exec "^(ASTD_astd.get_name astd));
	let (name,transition_list,astd1,astd2)=ASTD_astd.get_data_synchronisation astd
	in if List.mem (ASTD_event.get_label event) transition_list
		then begin 
			let (_,_,mod_state,to_save1,kappa1,is_modified) = execute state1 astd1 event ((path)@([name])) env call_path
			and (_,_,mod_state2,to_save2,kappa2,is_modified2) = execute state2 astd2 event ((path)@([name])) env call_path
			in if is_modified && is_modified2
				then (ASTD_state.Synchronisation_s (mod_state,mod_state2),((path)@([name])),ASTD_state.Synchronisation_s (mod_state,mod_state2),to_save1@to_save2,kappa1@kappa2,true)
				else (state,[],ASTD_state.Synchronisation_s (mod_state,mod_state2),[],[],false) 
			end
		else begin 
			let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state1 astd1 event ((path)@([name])) env call_path
			in if is_modified 
				then begin 
					(delta,sub_path,ASTD_state.Synchronisation_s (mod_state,state2),to_save,kappa,true)
					end
				else begin 
					let (delta2,sub_path2,mod_state2,to_save2,kappa2,is_modified2) = execute state2 astd2 event ((path)@([name])) env call_path
					in if is_modified2
						then (delta2,sub_path2,ASTD_state.Synchronisation_s (mod_state,mod_state2),to_save2,kappa2,true)
						else (state,[],ASTD_state.Synchronisation_s (mod_state,mod_state2),[],[],false) 
					end
			end
	end           

  |ASTD_state.Guard_s (started,state2) -> 
	begin debug ("guard exec "^(ASTD_astd.get_name astd));
	let (name,pred_list,astd2)=ASTD_astd.get_data_guard astd
	in if started 
		then let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event ((path)@([name])) env call_path
			in (delta,sub_path,ASTD_state.Guard_s (started,mod_state),to_save,kappa,is_modified)
		else if (ASTD_arrow.evaluate_guard env pred_list)
			then let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event ((path)@([name])) env call_path
				in (ASTD_state.Guard_s (true,mod_state),((path)@([name])),ASTD_state.Guard_s (true,mod_state),to_save,kappa,is_modified)
			else (state,[],state,[],[],false)
	end
(** Old school
  |ASTD_state.QChoice_s (val_used,final_dom,unknown_dom,state2) -> 
	begin debug ("qchoice exec "^(ASTD_astd.get_name astd));
	let (name,var,list_val,dep,astd2)=ASTD_astd.get_data_qchoice astd
	in let qchoice = choose_value event astd2 (ASTD_term.Var var)
	in if val_used=ASTD_state.ChoiceNotMade
		then begin 
			if qchoice=ASTD_state.ChoiceNotMade
				then begin debug "qchoice not made or opt found";
					try_qchoice dep (ASTD_event.get_label event) state2 astd2 var event ((path)@([name])) env call_path list_val
					end
				else begin 
				     if (not(ASTD_constant.is_included (ASTD_term.extract_constant_from_term (ASTD_state.get_val qchoice)) list_val))
				     then
					(state,[],state,[],[],false)
				     else
					let value = (ASTD_state.get_val qchoice)
					in let bind_env = ASTD_environment.bind var value
					in let env2=(ASTD_environment.add_binding bind_env env)
					in let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event ((path)@([name])) env2 call_path
					in if is_modified 
						then begin 
							(ASTD_state.QChoice_s (qchoice,final_dom,unknown_dom,mod_state),
								((path)@([name])),
								ASTD_state.QChoice_s (qchoice,final_dom,unknown_dom,mod_state),
								to_save,
								(ASTD_optimisation.automatic_gestion_of_kappa_values env (ASTD_event.get_label event) dep value)@kappa,
								is_modified)
							end
						else (state,[],state,[],[],false)
					end
			end
		else begin 
			if (qchoice<>(ASTD_state.ChoiceNotMade))&&(not(qchoice=val_used))
			then begin debug  ("qchoice "^var^" choice impossible "^(ASTD_term.string_of(ASTD_state.get_val qchoice))^" for the astd "^name^" having "^(ASTD_term.string_of(ASTD_state.get_val val_used)));
				(state,[],state,[],[],false) end
			else begin debug ("qchoice "^var^" choice made "^(begin if qchoice=ASTD_state.ChoiceNotMade then "ChoiceNotMade" else(ASTD_term.string_of(ASTD_state.get_val qchoice))end)^" for the astd "^name);
				let bind_env = ASTD_environment.bind var (ASTD_state.get_val val_used)
				in let env2=(ASTD_environment.add_binding bind_env env)
				in let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event ((path)@([name])) env2 call_path
				in (delta,sub_path,ASTD_state.QChoice_s (val_used,final_dom,unknown_dom,mod_state),to_save,begin if qchoice=ASTD_state.ChoiceNotMade then [] else (ASTD_optimisation.automatic_gestion_of_kappa_values env (ASTD_event.get_label event) dep (ASTD_state.get_val qchoice))end@kappa,is_modified)
				end
			end
	end 

*)
(** New Kappa*)
  |ASTD_state.QChoice_s (val_used,final_dom,unknown_dom,state2) -> 
	begin debug ("qchoice exec "^(ASTD_astd.get_name astd));
	let (name,var,list_val,dep,astd2)=ASTD_astd.get_data_qchoice astd
	in if val_used=ASTD_state.ChoiceNotMade
		then begin debug "trying some values";
			try_qchoice dep (ASTD_event.get_label event) state2 astd2 var event ((path)@([name])) env call_path list_val (get_values event astd2 (ASTD_term.Var var))
			end
					
		else begin 
			debug ("qchoice "^var^" choice using "^(ASTD_term.string_of(ASTD_state.get_val val_used))^" for the astd "^name);
				let bind_env = ASTD_environment.bind var (ASTD_state.get_val val_used)
				in let env2=(ASTD_environment.add_binding bind_env env)
				in let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event ((path)@([name])) env2 call_path
				in (delta,sub_path,
					ASTD_state.QChoice_s (val_used,final_dom,unknown_dom,mod_state),
					to_save,
					(ASTD_optimisation.automatic_gestion_of_kappa_values env (ASTD_event.get_label event) dep (ASTD_state.get_val val_used))@kappa, 					is_modified)
				
			end
	end 

(** Old school
  |ASTD_state.QSynchronisation_s (not_fin_dom,unknown_dom,not_init_dom,init) -> 
	begin debug ("qsynch exec "^(ASTD_astd.get_name astd));
	let (name,var,val_list,trans_list,opt,astd2)=ASTD_astd.get_data_qsynchronisation astd
	in let value1 = choose_value event astd2 (ASTD_term.Var var)
	in if not (value1=ASTD_state.ChoiceNotMade)
		then (begin debug ("kappa direct "^var^" "^(ASTD_term.string_of(ASTD_state.get_val value1))^" for the astd "^name);
			if (List.mem (ASTD_event.get_label event) trans_list) || (not(ASTD_constant.is_included (ASTD_term.extract_constant_from_term (ASTD_state.get_val value1)) val_list))
			then begin debug ("not possible decided in kappa direct "^name) ;(state,[],state,[],[],false) end
			else begin debug ("maybe possible in kappa direct "^name);
				let bind_env = ASTD_environment.bind var (ASTD_state.get_val value1)
				in let env2=(ASTD_environment.add_binding bind_env env)
				and value=ASTD_term.extract_constant_from_term (ASTD_state.get_val value1)
				in let value'=ASTD_constant.value_of value
				in let get_state=ASTD_state.get_synch_state not_init_dom init name (value) env2 call_path
				in let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute get_state astd2 event ((path)@([name])) env2 call_path
		(*NB : pour la mise en bdd, on peut faire (sub_path - path) pour connaitre le chemin de la modif dans le sous astd *)
				in if is_modified
					then let (new_state,isfinal)=(ASTD_state.is_final astd2 mod_state env2 call_path)
						in if (new_state=init)
							then begin debug ("return to init in "^name);
					 		if isfinal
							then let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.remove value' not_init_dom),init)
								in (state,
									[],
									new_study_state,
									((name,value,env2,call_path),new_state,false)::to_save,
									kappa,
									is_modified)

							else let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.remove value' not_init_dom),init)
								in (state,
									[],
									new_study_state,
									((name,value,env2,call_path),new_state,false)::to_save,
									kappa,
									is_modified)
							end
							else begin 
							if isfinal
							then let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.insert value' not_init_dom),init)
								in (state,
									[],
									new_study_state,
									((name,value,env2,call_path),new_state,true)::to_save,
									kappa,
									is_modified)

							else let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.insert value' not_init_dom),init)
								in (state,
									[],
									new_study_state,
									((name,value,env2,call_path),new_state,true)::to_save,
									kappa,
									is_modified)
							end
					else begin 
						(state,[],state,[],[],false)
						end
					
				end
			end)
		else try begin
                        let value2 = active_optimisation event astd env (ASTD_event.get_label event) var opt 
			in if not(value2=ASTD_state.ChoiceNotMade)
				then begin debug ("kappa indirect "^var^" "^(ASTD_term.string_of(ASTD_state.get_val value2))^" for the astd "^name);
				if (List.mem (ASTD_event.get_label event) trans_list) || (not(ASTD_constant.is_included (ASTD_term.extract_constant_from_term (ASTD_state.get_val value2)) val_list))
					then (state,[],state,[],[],false)
					else begin 
						let bind_env = ASTD_environment.bind var (ASTD_state.get_val value2)
						in let env2=(ASTD_environment.add_binding bind_env env)
						and value=ASTD_term.extract_constant_from_term (ASTD_state.get_val value2)
						in let value'=ASTD_constant.value_of value
						in let get_state=ASTD_state.get_synch_state not_init_dom init name (value) env2 call_path
						in let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute get_state astd2 event ((path)@([name])) env2 call_path
				(*NB : pour la mise en bdd, on peut faire (sub_path - path) pour connaitre le chemin de la modif dans le sous astd *)		
						in begin 
							if is_modified
							then let (new_state,isfinal)=(ASTD_state.is_final astd2 mod_state env2 call_path)
								in if (new_state=init)
									then begin
									if isfinal
									then let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.remove value' not_init_dom),init)
										in (state,
											[],
											new_study_state,
											((name,value,env2,call_path),new_state,false)::to_save,
											kappa,
 											is_modified)
		
									else let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.remove value' not_init_dom),init)
										in (state,
											[],
											new_study_state,
											((name,value,env2,call_path),new_state,false)::to_save,
											kappa,
											is_modified)
									end
									else begin
									if isfinal
									then let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.insert value' not_init_dom),init)
										in (state,
											[],
											new_study_state,
											((name,value,env2,call_path),new_state,true)::to_save,
											kappa,
											is_modified)
	
									else let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.insert value' not_init_dom),init)
										in (state,
											[],
											new_study_state,
											((name,value,env2,call_path),new_state,true)::to_save,
											kappa,
											is_modified)
								end
							else begin 
							(state,[],state,[],[],is_modified)
								end
							end
						end

					end
				else begin debug ("no optimisation in "^name^" for "^(ASTD_event.get_label event) );
					if List.mem (ASTD_event.get_label event) trans_list
					then let (list_modif,modif_possible)=modif_all_qsynch name astd2 event init not_init_dom val_list env call_path [] var ((path)@([name]))
						in if modif_possible 
							then begin 
								apply_all_modifs name var astd2 env call_path list_modif (state,path,state,[],[],false)
								end
							else (state,[],state,[],[],false)
					else try_qsynch name state astd2 var event env call_path val_list
					end		
	end 
	with _ -> (state,[],state,[],[],false)
	end
*)


(**New Kappa*)
  |ASTD_state.QSynchronisation_s (not_fin_dom,unknown_dom,not_init_dom,init) -> 
	begin debug ("qsynch exec "^(ASTD_astd.get_name astd));
	let (name,var,val_list,trans_list,opt,astd2)=ASTD_astd.get_data_qsynchronisation astd
	in try begin
                        let value2 = active_optimisation event astd env (ASTD_event.get_label event) var opt 
			in if not(value2=ASTD_state.ChoiceNotMade)
				then begin debug ("kappa indirect "^var^" "^(ASTD_term.string_of(ASTD_state.get_val value2))^" for the astd "^name);
				if (List.mem (ASTD_event.get_label event) trans_list) || (not(ASTD_constant.is_included (ASTD_term.extract_constant_from_term (ASTD_state.get_val value2)) val_list))
					then (state,[],state,[],[],false)
					else begin 
						let bind_env = ASTD_environment.bind var (ASTD_state.get_val value2)
						in let env2=(ASTD_environment.add_binding bind_env env)
						and value=ASTD_term.extract_constant_from_term (ASTD_state.get_val value2)
						in let value'=ASTD_constant.value_of value
						in let get_state=ASTD_state.get_synch_state not_init_dom init name (value) env2 call_path
						in let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute get_state astd2 event ((path)@([name])) env2 call_path
				(*NB : pour la mise en bdd, on peut faire (sub_path - path) pour connaitre le chemin de la modif dans le sous astd *)		
						in begin 
							if is_modified
							then let (new_state,isfinal)=(ASTD_state.is_final astd2 mod_state env2 call_path)
								in if (new_state=init)
									then begin debug "kappa indirect !!!!!!!!!!";
									if isfinal
									then let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.remove value' not_init_dom),init)
										in (state,
											[],
											new_study_state,
											((name,value,env2,call_path),new_state,false)::to_save,
											kappa,
 											is_modified)
		
									else let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.remove value' not_init_dom),init)
										in (state,
											[],
											new_study_state,
											((name,value,env2,call_path),new_state,false)::to_save,
											kappa,
											is_modified)
									end
									else begin debug "kappa indirect !!!!!!!!!!";
									if isfinal
									then let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.insert value' not_init_dom),init)
										in (state,
											[],
											new_study_state,
											((name,value,env2,call_path),new_state,true)::to_save,
											kappa,
											is_modified)
	
									else let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value' not_fin_dom),(ASTD_constant.remove value' unknown_dom),(ASTD_constant.insert value' not_init_dom),init)
										in (state,
											[],
											new_study_state,
											((name,value,env2,call_path),new_state,true)::to_save,
											kappa,
											is_modified)
								end
							else begin 
							(state,[],state,[],[],is_modified)
								end
							end
						end

					end
				else begin debug ("no optimisation in "^name^" for "^(ASTD_event.get_label event) );
					if List.mem (ASTD_event.get_label event) trans_list
					then let (list_modif,modif_possible)=modif_all_qsynch name astd2 event init not_init_dom val_list env call_path [] var ((path)@([name]))
						in if modif_possible 
							then begin 
								apply_all_modifs name var astd2 env call_path list_modif (state,path,state,[],[],false)
								end
							else (state,[],state,[],[],false)
					else try_qsynch name state astd2 var event env call_path val_list (get_values event astd2 (ASTD_term.Var var)) 
					end		
	end 

	with _ -> (state,[],state,[],[],false)
	end 


  |ASTD_state.Call_s (called,state2) -> 
	begin debug ("call exec "^(ASTD_astd.get_name astd));
	let (name,called_name,fct_vec)=ASTD_astd.get_data_call astd
	in let astd2=(ASTD_astd.call_astd called_name (ASTD_environment.increase_call env fct_vec))
	in if called 
		then begin 
			let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state2 astd2 event ((path)@([name])) (ASTD_environment.increase_call env fct_vec) (name::call_path)
			in (delta,sub_path,ASTD_state.Call_s (called,mod_state),to_save,kappa,is_modified)
			end
		else begin 
			let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute (ASTD_state.init astd2) astd2 event ((path)@([name])) (ASTD_environment.increase_call env fct_vec) (name::call_path)
			in (ASTD_state.Call_s (true,state2),sub_path,ASTD_state.Call_s (true,mod_state),to_save,kappa,is_modified)
			end
	end


  |_ -> begin (state,[],state,[],[],false) end


and try_qchoice dep label state astd var event path env call_path list_val kappa_dir_val =

if kappa_dir_val=[]
	then if not (list_val = (ASTD_constant.empty_dom))
		then begin debug "then trying global values";
			let (head_val,tail)= ASTD_constant.head_tail list_val
			in let bind_env = ASTD_environment.bind var (ASTD_term.Const head_val)
			in let env2=(ASTD_environment.add_binding bind_env env)
			in let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state astd event path env2 call_path
			in if is_modified
				then begin 
					(ASTD_state.QChoice_s (ASTD_state.val_of (ASTD_term.Const head_val),(ASTD_constant.empty_dom),(ASTD_constant.empty_dom),mod_state),
					path,
					ASTD_state.QChoice_s (ASTD_state.val_of (ASTD_term.Const head_val),(ASTD_constant.empty_dom),(ASTD_constant.empty_dom),mod_state),
					to_save,
					(ASTD_optimisation.automatic_gestion_of_kappa_values env label dep (ASTD_term.Const head_val))@kappa,
					is_modified)
					end
				else try_qchoice dep label state astd var event path env call_path tail []
			end
		else (state,[],state,[],[],false)
	else begin debug "trying first kappa values";
			let head_val=List.hd kappa_dir_val
			in let bind_env = ASTD_environment.bind var (head_val)
			in let env2=(ASTD_environment.add_binding bind_env env)
			in let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state astd event path env2 call_path
			in if is_modified
				then begin debug "kappa direct !!!!!!!!!!";
					(ASTD_state.QChoice_s (ASTD_state.val_of (head_val),(ASTD_constant.empty_dom),(ASTD_constant.empty_dom),mod_state),
					path,
					ASTD_state.QChoice_s (ASTD_state.val_of (head_val),(ASTD_constant.empty_dom),(ASTD_constant.empty_dom),mod_state),
					to_save,
					(ASTD_optimisation.automatic_gestion_of_kappa_values env label dep (head_val))@kappa,
					is_modified)
					end
				else try_qchoice dep label state astd var event path env call_path list_val (List.tl kappa_dir_val)
		end






and try_qsynch name state astd var event env call_path list_val kappa_dir_val=
	if kappa_dir_val=[]
	then if list_val <> (ASTD_constant.empty_dom)
		then begin debug "then try dom for qsynch";
			let (head_val,tail)= ASTD_constant.head_tail list_val
			and (not_fin_dom, unknown_dom, not_init_dom,init) = ASTD_state.get_data_from_qsynchro state
			in let value=ASTD_constant.value_of head_val
			in let bind_env = ASTD_environment.bind var (ASTD_term.Const head_val)
			in let env2=(ASTD_environment.add_binding bind_env env)
			in let sub_state=ASTD_state.get_synch_state not_init_dom init name (head_val) env2 call_path
			in let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute sub_state astd event [] env2 call_path
			in if is_modified
				then let (new_state,isfinal)=(ASTD_state.is_final astd mod_state env2 call_path)
					in if (new_state=init)
						then begin 
						if isfinal
						then let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.remove value not_init_dom),init)
							in (state,[],new_study_state,((name,head_val,env2,call_path),new_state,false)::to_save,kappa,is_modified)
						else let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.remove value not_init_dom),init)
							in (state,[],new_study_state,((name,head_val,env2,call_path),new_state,false)::to_save,kappa,is_modified)
						end
						else begin 
						if isfinal
						then let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.insert value not_init_dom),init)
							in (state,[],new_study_state,((name,head_val,env2,call_path),new_state,true)::to_save,kappa,is_modified)
						else let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.insert value not_init_dom),init)
							in (state,[],new_study_state,((name,head_val,env2,call_path),new_state,true)::to_save,kappa,is_modified)
						end
				else try_qsynch name state astd var event env call_path tail []
			end
		else (state,[],state,[],[],false)
	else begin debug " try kappa dir for qsynch ";
			let head_val=(ASTD_term.extract_constant_from_term (List.hd kappa_dir_val))
			and (not_fin_dom, unknown_dom, not_init_dom,init) = ASTD_state.get_data_from_qsynchro state
			in let value=ASTD_constant.value_of (head_val)
			in let bind_env = ASTD_environment.bind var (List.hd kappa_dir_val)
			in let env2=(ASTD_environment.add_binding bind_env env)
			in let sub_state=ASTD_state.get_synch_state not_init_dom init name (head_val) env2 call_path
			in let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute sub_state astd event [] env2 call_path
			in if is_modified
				then let (new_state,isfinal)=(ASTD_state.is_final astd mod_state env2 call_path)
					in if (new_state=init)
						then begin debug "kappa direct !!!!!!!!!!";
						if isfinal
						then let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.remove value not_init_dom),init)
							in (state,[],new_study_state,((name,head_val,env2,call_path),new_state,false)::to_save,kappa,is_modified)
						else let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.remove value not_init_dom),init)
							in (state,[],new_study_state,((name,head_val,env2,call_path),new_state,false)::to_save,kappa,is_modified)
						end
						else begin debug "kappa direct !!!!!!!!!!";
						if isfinal
						then let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.remove value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.insert value not_init_dom),init)
							in (state,[],new_study_state,((name,head_val,env2,call_path),new_state,true)::to_save,kappa,is_modified)
						else let new_study_state=ASTD_state.QSynchronisation_s((ASTD_constant.insert value not_fin_dom),(ASTD_constant.remove value unknown_dom),(ASTD_constant.insert value not_init_dom),init)
							in (state,[],new_study_state,((name,head_val,env2,call_path),new_state,true)::to_save,kappa,is_modified)
						end
				else try_qsynch name state astd var event env call_path list_val (List.tl kappa_dir_val)
		end


and modif_all_qsynch name astd event init not_init_dom value_list env call_path modifs var path=
	if value_list<>ASTD_constant.empty_dom
		then let (val_used, tail)= ASTD_constant.head_tail value_list
			in let bind_env = ASTD_environment.bind var (ASTD_term.Const ( val_used))
			in let env2=(ASTD_environment.add_binding bind_env env)
			in if ASTD_constant.is_included val_used not_init_dom
				then begin 
					let state=ASTD_state.get_synch name val_used env2 call_path
					in let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute state astd event path env2 call_path
						in let modif=(val_used,(delta,sub_path,mod_state,to_save,kappa,is_modified))
						in if is_modified 
							then modif_all_qsynch name astd event init not_init_dom tail env call_path (modif::modifs) var path
							else ([],false)
					end
				else begin 
					let (delta,sub_path,mod_state,to_save,kappa,is_modified) = execute init astd event path env2 call_path
						in let modif=(val_used,(delta,sub_path,mod_state,to_save,kappa,is_modified))
						in if is_modified 
							then modif_all_qsynch name astd event init not_init_dom tail env call_path (modif::modifs) var path
							else ([],false)
					end
		else begin 
			(modifs,true)
			end



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
			let (delta,sub_path,new_state,to_save,kappa,is_modified) = execute state astd event [] ASTD_environment.empty []
			in begin 
				if is_modified 
				then if affichage=2
					then 
						begin compteur();
						ASTD_state.save_data to_save;
						ASTD_optimisation.apply_each_mod kappa
						end
					else
						begin (*faire la mise à jour de bdd ici, avec path=[]-> ne rien faire*)
						ASTD_state.save_data to_save;
						ASTD_optimisation.apply_each_mod kappa;
						ASTD_state.print new_state astd "" [] []
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
			let (delta,sub_path,new_state,to_save,kappa,is_modified) = execute state astd event [] ASTD_environment.empty []
			in begin 
				if is_modified 
				then begin 
					ASTD_state.save_data to_save;
					ASTD_optimisation.apply_each_mod kappa
					end
				else begin
					print_endline ("Execution of : "^(ASTD_label.string_of(ASTD_event.get_label event))^" not possible");
					print_endline "======================================================"
					end
				end;
				execute_event_list affichage new_state astd tail
			end
	|[]->begin print_endline "================================================================";state end



