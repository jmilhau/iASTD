type path=string list

type dependancy_path = ASTD_astd.dependancy_path




let var_to_dep_path var = ASTD_astd.Direct (var)

let dep_to_dep_path var dep sub_path = ASTD_astd.Dependant(var,dep,[],sub_path)

let is_direct path= match path with
	|ASTD_astd.Direct(_)->true
	|ASTD_astd.Dependant(_)->false

let is_dependant path= match path with
	|ASTD_astd.Direct(_)->false
	|ASTD_astd.Dependant(_)->true

let rec remove_var var var_list=match var_list with
	|variable::tail-> if variable=var 
			then (remove_var var tail)
			else variable::(remove_var var tail)
	|[]->[]

let rec remove_var_list to_remove var_list = match to_remove with
	|variable::tail-> let removed=remove_var variable var_list
				in remove_var_list tail removed
	|[]->var_list

(**vu que le call est pas 100% parfait, pas de gestion pour l'instant*)



let rec find_all_transitions called astd =match astd with
   |ASTD_astd.Automata (name,astd_list,arrow_list,final_states,init) -> 
		begin 
		let next_trans=(List.fold_left List.append [] (List.map (find_all_transitions called) astd_list))
		in let transitions=(List.map ASTD_arrow.get_transition (arrow_list))@next_trans
		in transitions
		end
      
   |ASTD_astd.Sequence (name,left_astd,right_astd) -> 
		begin
		let transitions=(find_all_transitions called left_astd)@(find_all_transitions called right_astd) 
		in transitions
		end
                                                     
   |ASTD_astd.Choice (name,first_astd,second_astd) -> 
		begin
		let transitions=(find_all_transitions called first_astd)@(find_all_transitions called second_astd) 
		in transitions
		end

   |ASTD_astd.Kleene (name,sub_astd) -> 
		begin 
		let transitions=(find_all_transitions called sub_astd)
		in transitions
		end

   |ASTD_astd.Synchronisation (name,synch_trans,sub_astd1,sub_astd2) -> 
		begin
		let transitions=(find_all_transitions called sub_astd1)@(find_all_transitions called sub_astd2)
		in transitions
		end

   |ASTD_astd.QSynchronisation (name,var,domain,synch_trans,sub_astd,users)-> 
		begin 
		let transitions=(find_all_transitions called sub_astd)
		in transitions
		end

   |ASTD_astd.QChoice (name,var,domain,sub_astd,producers)-> 
		begin 
		let transitions=(find_all_transitions called sub_astd)
		in transitions
		end

   |ASTD_astd.Guard (name,predicate_list,sub_astd)-> 
		begin 
		let transitions=(find_all_transitions called sub_astd)
		in transitions
		end

   |ASTD_astd.Call (name,called_name,affectations)->
		begin []
(*bla bla insérer l'appel de l'astd si pas déjà appelé, et le rajouter ensuite à la liste des apppelés*)
		end

   |_ ->begin []
	end
(*fct de récupération des transitions*)




let is_included label astd called = List.mem label (List.map (ASTD_transition.get_label) (find_all_transitions called astd))



(**      Part1 : récupération des transitions possiblement optimizables (kappa indiret)     *)


let rec add_quantifiers transition_list quantifiers path = match transition_list with
	|transition::tail-> (transition,quantifiers,path)::(add_quantifiers tail quantifiers path)
	|[]->[]


let rec find_dep_transitions quantifiers path astd= match astd with
   |ASTD_astd.Automata (name,astd_list,arrow_list,final_states,init) -> 
		begin 
		let next_trans=(List.fold_left List.append [] (List.map (find_dep_transitions quantifiers path) astd_list))
		in let transitions=(add_quantifiers (List.map (ASTD_arrow.get_transition) arrow_list) quantifiers path)@next_trans
		in transitions
		end
      
   |ASTD_astd.Sequence (name,left_astd,right_astd) -> 
		begin
		let transitions=(find_dep_transitions quantifiers path left_astd)@(find_dep_transitions quantifiers path right_astd) 
		in transitions
		end
                                                     
   |ASTD_astd.Choice (name,first_astd,second_astd) -> 
		begin
		let transitions=(find_dep_transitions quantifiers path first_astd)@(find_dep_transitions quantifiers path second_astd) 
		in transitions
		end

   |ASTD_astd.Kleene (name,sub_astd) -> 
		begin 
		let transitions=(find_dep_transitions quantifiers path sub_astd)
		in transitions
		end

   |ASTD_astd.Synchronisation (name,synch_trans,sub_astd1,sub_astd2) -> 
		begin
		let transitions=(find_dep_transitions quantifiers (path@["l"]) sub_astd1)
						@(find_dep_transitions quantifiers (path@["r"]) sub_astd2)
		in transitions
		end

   |ASTD_astd.QSynchronisation (name,var,domain,synch_trans,sub_astd,users)-> 
		begin 
		let transitions=(find_dep_transitions (var::quantifiers) path sub_astd)
		in transitions
		end

   |ASTD_astd.QChoice (name,var,domain,sub_astd,producers)-> 
		begin 
		let transitions=(find_dep_transitions quantifiers path sub_astd)
		in transitions
		end

   |ASTD_astd.Guard (name,predicate_list,sub_astd)-> 
		begin 
		let transitions=(find_dep_transitions quantifiers path sub_astd)
		in transitions
		end

   |ASTD_astd.Call (name,called_name,affectations)-> 
		begin []
		end

   |_ ->[]



let rec transition_fit path having quantifiers dep_var_list transition = match quantifiers with
	|(var)::tail-> if List.mem (ASTD_term.Var var) (ASTD_transition.get_params transition)
				then transition_fit path (var::having) tail dep_var_list transition
				else transition_fit path having tail (var::dep_var_list) transition
	|[]->if dep_var_list=[] 
		then [] 
		else [(transition,dep_var_list,having,path)]



let rec get_dep_transitions information_list = match information_list with
	|(transition,quantifiers,path)::tail -> ((transition_fit path [] quantifiers [] transition)@(get_dep_transitions tail))
	|[]->[]



let rec dependant_transitions astd = get_dep_transitions (find_dep_transitions [] [] astd)
(*give access to dependant transitions, that is to say to every transition that isn't kappa-direct-optimizable*)








(**    Part2 : récupération de toutes les dépendances fonctionnelles possibles  *)


let rec remove_doublons_label doub_list =match doub_list with
	|label::tail-> let clean=remove_doublons_label tail
			in if (List.mem label clean) 
				then clean
				else label::clean
	|[]->[]

let rec intersection_names list1 list2 = match list1 with
	|label::tail-> let inter=intersection_names tail list2 
			in if List.mem label inter 
				then inter
				else label::inter
	|[]->remove_doublons_label (list2)



let automata_complete_path path name = path@[name]



let rec get_dependancies quantifiers wait already_called path astd = match astd with
   |ASTD_astd.Automata (name,astd_list,arrow_list,final_states,init) -> 
		begin 
		let names=List.map ASTD_astd.get_name astd_list
		in List.fold_left List.append [] (List.map2 (get_dependancies quantifiers wait already_called) 
							(List.map (automata_complete_path path) names) 
							astd_list)
		end
      
   |ASTD_astd.Sequence (name,left_astd,right_astd) -> 
		begin
		(get_dependancies quantifiers wait already_called (path@["l_seq"]) left_astd)@(get_dependancies quantifiers wait already_called (path@["r_seq"]) right_astd)
		end
                                                     
   |ASTD_astd.Choice (name,first_astd,second_astd) -> 
		begin
		(get_dependancies quantifiers wait already_called (path@["f_choice"]) first_astd)@(get_dependancies quantifiers wait already_called (path@["s_choice"]) second_astd)
		end

   |ASTD_astd.Kleene (name,sub_astd) -> 
		begin 
		get_dependancies quantifiers wait already_called path sub_astd
		end

   |ASTD_astd.Synchronisation (name,synch_trans,sub_astd1,sub_astd2) -> 
		begin
		if List.length(remove_doublons_label (List.map ASTD_transition.get_label synch_trans))
		=List.length( intersection_names (List.map ASTD_transition.get_label synch_trans)
						 (intersection_names
							(List.map (ASTD_transition.get_label) (find_all_transitions [] sub_astd1) )
							(List.map (ASTD_transition.get_label) (find_all_transitions [] sub_astd2) )
							)
				)
		then (get_dependancies quantifiers wait already_called (path@["l_synch"]) sub_astd1)
				@(get_dependancies quantifiers wait already_called (path@["r_synch"]) sub_astd2)
		else (get_dependancies quantifiers true already_called (path@["l_synch"]) sub_astd1)
				@(get_dependancies quantifiers true already_called (path@["r_synch"]) sub_astd2)
		end

   |ASTD_astd.QSynchronisation (name,var,domain,synch_trans,sub_astd,users)-> 
		begin 
		get_dependancies (var::quantifiers) false already_called path sub_astd
		end

   |ASTD_astd.QChoice (name,var,domain,sub_astd,producers)-> 
		begin 
		if wait 
		then (get_dependancies quantifiers wait already_called path sub_astd)
		else (quantifiers,var,path)::(get_dependancies quantifiers wait already_called path sub_astd)
		end

   |ASTD_astd.Guard (name,predicate_list,sub_astd)-> 
		begin 
		get_dependancies quantifiers wait already_called path sub_astd
		end

   |ASTD_astd.Call (name,called_name,affectations)-> 
		begin []
(*bla bla récupérer le sous astd si pas déjà appelé*)
		end

   |_ ->[]
(*algo de Benito*)





(**     Part3 : récupération des informations sur les dépendances utiles à partir de gestion de graphes*)




let rec dependance_study_graph var using_list dependance_list met_dependances met_var = match met_dependances with
	|(quantifiers,dep_var,path)::tail-> 
		if List.mem var met_var 
			then (false,ASTD_astd.Direct(var))
			else if var=dep_var 
				then let (possible,sub_list)=study_list_graph quantifiers using_list dependance_list met_var
					in if possible
						then (true, dep_to_dep_path var (quantifiers,dep_var,path) sub_list)
						else dependance_study_graph var using_list dependance_list tail met_var
				else dependance_study_graph var using_list dependance_list tail met_var
	|[]->(false,ASTD_astd.Direct(var))



and study_list_graph quantifiers using_list dependance_list met_var = match quantifiers with
	|var::tail-> 
	let (possible,studied_list)=study_list_graph tail using_list dependance_list met_var 
		in if List.mem var using_list 
			then if possible 
				then (possible,(ASTD_astd.Direct(var))::studied_list)
				else (false,[])
			else if possible 
				then let (possible2,studied2) = dependance_study_graph var using_list dependance_list dependance_list met_var
					in (possible2, studied2::studied_list)
				else (false,[])
	|[]->(true,[])




let rec get_informations information_list dependance_list= match information_list with
	|(transition,dep_var_list,having_list,path)::tail-> 
			let (possible,result_study)=study_list_graph dep_var_list having_list dependance_list []
				in if possible
					then (ASTD_transition.get_label transition,result_study,path)::(get_informations tail dependance_list)
					else (get_informations tail dependance_list)
	|[]->[]





 (*passe tout à la moulinette et en ressort [(transition,[Dependant(missing_var,used_dep,prod,needed_var_list),Direct(accessible_var),...],path)] *)
let sum_up_informations astd  = 
	let graph= get_dependancies [] true [] [] astd
	and information_list= (dependant_transitions astd)
	in get_informations information_list graph







(**     Part4 : recherche de producteurs*)



(*argument: parametres d'une transition, retourne true si contient tous les quantificateurs étudiés*)
let rec study_parameters quantifiers parameters = match quantifiers with
	|(var)::tail-> if List.mem (ASTD_term.Var var) parameters
				then study_parameters tail parameters
				else false
	|[]->true




let rec study_variables probable_transitions quantifiers =match probable_transitions with
	|transition::tail-> if (study_parameters quantifiers (ASTD_transition.get_params transition))
				then let label=(ASTD_transition.get_label transition)
					and following_study=(study_variables tail quantifiers)
					in if List.mem label following_study
						then following_study
						else (label::following_study)
				else study_variables tail quantifiers
	|[]->[]



(*suivant si le type d'arrows voulu et l'état initial, retourne les transitions acceptables*)
let rec extract_corresponding_transitions arrow_list init final quantifiers = match arrow_list with
	|arrow::tail-> if (ASTD_arrow.get_from_final_state arrow)
			then if ( (final) && ((ASTD_arrow.get_from arrow)==init) )
				then let transition=ASTD_arrow.get_transition arrow
					in if (study_parameters quantifiers (ASTD_transition.get_params transition))
						then (transition)::(extract_corresponding_transitions tail init final quantifiers)
						else extract_corresponding_transitions tail init final quantifiers
				else extract_corresponding_transitions tail init final quantifiers
			else if ((ASTD_arrow.get_from arrow)=init)
				then let transition=ASTD_arrow.get_transition arrow
					in if (study_parameters quantifiers (ASTD_transition.get_params transition))
						then (transition)::(extract_corresponding_transitions tail init final quantifiers)
						else extract_corresponding_transitions tail init final quantifiers
				else extract_corresponding_transitions tail init final quantifiers
	|[]->[]



(*trouve tous les labels correspondant à des producteurs*)
let rec find_producers quantifiers astd env  = match astd with
	|ASTD_astd.Automata (name,astd_list,arrow_list,final_states,init) -> 
		begin 
		let sub_astd=(ASTD_astd.find_subastd init astd_list)
		in let final=ASTD_state.is_final sub_astd (ASTD_state.init_env sub_astd env) env
		in if (ASTD_astd.is_elem sub_astd)
			then let probable_transitions = extract_corresponding_transitions arrow_list init true quantifiers
				in let producers_label=study_variables probable_transitions quantifiers
				in producers_label
			else let probable_transitions = extract_corresponding_transitions arrow_list init final quantifiers
				in let producers_label=study_variables probable_transitions quantifiers 
				in producers_label@(find_producers quantifiers sub_astd env)
		end
      
	|ASTD_astd.Sequence (name,left_astd,right_astd) -> 
		begin
		let futur_state=ASTD_state.init_env left_astd env
		in if ASTD_state.is_final left_astd futur_state env
			then (find_producers quantifiers left_astd env)
					@(find_producers quantifiers right_astd env)
			else (find_producers quantifiers left_astd env)
		end

	|ASTD_astd.Choice (name,first_astd,second_astd) -> 
		begin
		(find_producers quantifiers first_astd env)
					@(find_producers quantifiers second_astd env)
		end

	|ASTD_astd.Kleene (name,sub_astd) -> 
		begin 
		(find_producers quantifiers sub_astd env)
		end

	|ASTD_astd.Synchronisation (name,synch_trans,sub_astd1,sub_astd2) -> 
		begin
		(find_producers quantifiers sub_astd1 env)
			@(find_producers quantifiers sub_astd2 env)
		end

	|ASTD_astd.QSynchronisation (name,var,domain,synch_trans,sub_astd,users)-> 
		begin 
		let bind_env = ASTD_environment.bind var (ASTD_term.Const(ASTD_constant.FreeConst))
		in (find_producers quantifiers sub_astd (ASTD_environment.add_binding bind_env env))
		end

	|ASTD_astd.QChoice (name,var,domain,sub_astd,producers)-> 
		begin 
		let bind_env = ASTD_environment.bind var (ASTD_term.Const(ASTD_constant.FreeConst))
		in (find_producers quantifiers sub_astd (ASTD_environment.add_binding bind_env env))
		end

	|ASTD_astd.Guard (name,predicate_list,sub_astd)-> 
		begin 
		find_producers quantifiers sub_astd env
		end

	|ASTD_astd.Call (name,called_name,affectations)->
		begin []
(*bla bla insérer l'appel de l'astd si pas déjà appelé, et le rajouter ensuite à la liste des apppelés*)
		end

	|_ ->[]






(*trouve tous les labels correspondant à des consommateurs*)
let rec find_consummers quantifiers astd env  = match astd with
	|ASTD_astd.Automata (name,astd_list,arrow_list,final_states,init) -> 
		begin 
		let sub_astd=(ASTD_astd.find_subastd init astd_list)
		in let final=ASTD_state.is_final sub_astd (ASTD_state.init_env sub_astd env) env
		in if (ASTD_astd.is_elem sub_astd)
			then let probable_transitions = extract_corresponding_transitions arrow_list init true quantifiers
				in let producers_label=study_variables probable_transitions quantifiers
				in producers_label
			else let probable_transitions = extract_corresponding_transitions arrow_list init final quantifiers
				in let producers_label=study_variables probable_transitions quantifiers 
				in producers_label@(find_consummers quantifiers sub_astd env)
		end
      
	|ASTD_astd.Sequence (name,left_astd,right_astd) -> 
		begin
		let futur_state=ASTD_state.init_env left_astd env
		in if ASTD_state.is_final left_astd futur_state env
			then (find_consummers quantifiers left_astd env)
					@(find_consummers quantifiers right_astd env)
			else (find_consummers quantifiers left_astd env)
		end

	|ASTD_astd.Choice (name,first_astd,second_astd) -> 
		begin
		(find_consummers quantifiers first_astd env)
					@(find_consummers quantifiers second_astd env)
		end

	|ASTD_astd.Kleene (name,sub_astd) -> 
		begin 
		(find_consummers quantifiers sub_astd env)
		end

	|ASTD_astd.Synchronisation (name,synch_trans,sub_astd1,sub_astd2) -> 
		begin
		(find_consummers quantifiers sub_astd1 env)
			@(find_consummers quantifiers sub_astd2 env)
		end

	|ASTD_astd.QSynchronisation (name,var,domain,synch_trans,sub_astd,users)-> 
		begin 
		let bind_env = ASTD_environment.bind var (ASTD_term.Const(ASTD_constant.FreeConst))
		in (find_consummers quantifiers sub_astd (ASTD_environment.add_binding bind_env env))
		end

	|ASTD_astd.QChoice (name,var,domain,sub_astd,producers)-> 
		begin 
		let bind_env = ASTD_environment.bind var (ASTD_term.Const(ASTD_constant.FreeConst))
		in (find_consummers quantifiers sub_astd (ASTD_environment.add_binding bind_env env))
		end

	|ASTD_astd.Guard (name,predicate_list,sub_astd)-> 
		begin 
		find_consummers quantifiers sub_astd env
		end

	|ASTD_astd.Call (name,called_name,affectations)->
		begin []
(*bla bla insérer l'appel de l'astd si pas déjà appelé, et le rajouter ensuite à la liste des apppelés*)
		end

	|_ ->[]










(**     Part5 : réinsertion des informations obtenues*)


let rec append_fusion_dep_list dep_list1 dep_list2 = match dep_list1 with
	|dep::tail-> if List.mem dep dep_list2
				then append_fusion_dep_list tail dep_list2
				else append_fusion_dep_list tail (dep::dep_list2)
	|[]->dep_list2


let rec extract_dependance_list dependance_list = match dependance_list with
	|dependance::tail->append_fusion_dep_list (extract_dependance dependance) (extract_dependance_list tail)
	|[]->[]
and extract_dependance dependance = match dependance with
	|ASTD_astd.Dependant(var,dep,prod,dep_list) -> let extracted= (extract_dependance_list dep_list)
					in if List.mem dep extracted
						then extracted
						else dep::extracted
	|ASTD_astd.Direct(_)->[]


let rec dep_fusion label path dep_list flipped = match dep_list with
	|dependance::tail-> if List.mem_assoc dependance flipped
				then let mem=List.assoc dependance flipped
					in let new_flipped = (dependance,(label,path)::mem)::(List.remove_assoc dependance flipped)
					in dep_fusion label path tail new_flipped
				else dep_fusion label path tail ((dependance,[(label,path)])::flipped )
	|[]-> flipped



(*retourne la liste des liens transition->dependance en dépendance->transition*)
let rec flip_dependances dep_list = match dep_list with
	|(label,link,path)::tail -> let flipped = flip_dependances tail
				in dep_fusion label path (extract_dependance_list link) flipped
	|[]->[]



let rec follow_path astd path unnecessary called env= match astd with
	|ASTD_astd.Automata (name,astd_list,arrow_list,final_states,init) -> 
		begin 
		let name=List.hd path
		and new_path =List.tl path
		in let new_astd =(ASTD_astd.find_subastd name (ASTD_astd.get_sub astd)) 
		in if new_path=[]
			then (new_astd,unnecessary ,called, env)
			else follow_path new_astd new_path unnecessary called env
		end
      
	|ASTD_astd.Sequence (name,left_astd,right_astd) -> 
		begin
		let new_path=List.tl path
		in if List.hd path = "l_seq"
			then if new_path=[]
				then (left_astd, unnecessary, called, env)
				else follow_path left_astd new_path unnecessary called env
			else if List.hd path = "r_seq" 
				then if new_path=[]
					then (right_astd, unnecessary, called, env)
					else follow_path right_astd new_path unnecessary called env
				else failwith "wrong path followed"
		end

	|ASTD_astd.Choice (name,first_astd,second_astd) -> 
		begin
		let new_path=List.tl path
		in if List.hd path = "f_choice"
			then if new_path=[]
				then (first_astd, unnecessary, called, env)
				else follow_path first_astd new_path unnecessary called env
			else if List.hd path = "s_choice" 
				then if new_path=[]
					then (second_astd, unnecessary, called, env)
					else follow_path second_astd new_path unnecessary called env
				else failwith "wrong path followed"
		end

	|ASTD_astd.Kleene (name,sub_astd) -> 
		begin 
		follow_path sub_astd path unnecessary called env
		end

	|ASTD_astd.Synchronisation (name,synch_trans,sub_astd1,sub_astd2) -> 
		begin
		let new_path=List.tl path
		in if List.hd path = "l_synch"
			then if new_path=[]
				then (sub_astd1, unnecessary, called, env)
				else follow_path sub_astd1 new_path unnecessary called env
			else if List.hd path = "r_synch" 
				then if new_path=[]
					then (sub_astd2, unnecessary, called, env)
					else follow_path sub_astd2 new_path unnecessary called env
				else if List.hd path = "stop"  
					then (astd, unnecessary, called, env)
					else failwith "wrong path followed"
		end

	|ASTD_astd.QSynchronisation (name,var,domain,synch_trans,sub_astd,users)-> 
		begin 
		let bind_env=ASTD_environment.bind var (ASTD_term.Const(ASTD_constant.FreeConst))
                in if users=[]
			then follow_path sub_astd path unnecessary called (ASTD_environment.add_binding bind_env env)
			else follow_path sub_astd path (var::unnecessary) called (ASTD_environment.add_binding bind_env env)
		end

	|ASTD_astd.QChoice (name,var,domain,sub_astd,producers)-> 
		begin 
		let bind_env=ASTD_environment.bind var (ASTD_term.Const(ASTD_constant.FreeConst))
                in follow_path sub_astd path unnecessary called (ASTD_environment.add_binding bind_env env)
		end

	|ASTD_astd.Guard (name,predicate_list,sub_astd)-> 
		begin 
		follow_path sub_astd path unnecessary called env
		end

	|ASTD_astd.Call (name,called_name,affectations)->
		begin (ASTD_astd.Elem("fail"),[],[],[])
(*bla bla insérer l'appel de l'astd si pas déjà appelé, et le rajouter ensuite à la liste des apppelés*)
		end

	|_ ->(ASTD_astd.Elem("fail"),[],[],[])


let rec go_find_qchoice astd to_find_var unnecessary called env= match astd with      
	|ASTD_astd.Kleene (name,sub_astd) -> 
		begin 
		go_find_qchoice sub_astd to_find_var unnecessary called env
		end

	|ASTD_astd.QSynchronisation (name,var,domain,synch_trans,sub_astd,users)-> 
		begin 
		let bind_env=ASTD_environment.bind var (ASTD_term.Const(ASTD_constant.FreeConst))
                in go_find_qchoice sub_astd to_find_var unnecessary called (ASTD_environment.add_binding bind_env env)
		end

	|ASTD_astd.QChoice (name,var,domain,sub_astd,producers)-> 
		begin 
		let bind_env=ASTD_environment.bind var (ASTD_term.Const(ASTD_constant.FreeConst))
                in if var=to_find_var 
			then (sub_astd,unnecessary, called, (ASTD_environment.add_binding bind_env env))
			else go_find_qchoice sub_astd to_find_var unnecessary called (ASTD_environment.add_binding bind_env env)
		end

	|ASTD_astd.Guard (name,predicate_list,sub_astd)-> 
		begin 
		go_find_qchoice sub_astd to_find_var unnecessary called env
		end

	|ASTD_astd.Call (name,called_name,affectations)->
		begin (ASTD_astd.Elem("fail"),[],[],[])
(*bla bla insérer l'appel de l'astd si pas déjà appelé, et le rajouter ensuite à la liste des apppelés*)
		end

	|_ ->failwith "exact qchoice not_found"




let rec compare_path path1 path2 = match (path1,path2) with
	|(indication1::tail1,indication2::tail2)->if indication1=indication2
							then indication1::(compare_path tail1 tail2)
							else if (indication1="l_synch"||indication1="r_synch")
								then ["stop"]
								else failwith "difference between paths not on a sequence"
	|([],[])->failwith "exact same path"
	|(list,[])->failwith "path not found"
	|([],list)->failwith "path not found"



let rec synchronized_prod_cons transition_label_list prod_cons_list = match prod_cons_list with
	|transition::tail -> if List.mem (transition) transition_label_list
				then synchronized_prod_cons transition_label_list tail
				else false
	|[]->true


let rec compare_prod_cons_trans_list astd trans_list dep_path producers consummers = match trans_list with
	|(label,trans_path)::tail->
		begin
		let (separation,unnecessary,_,_) = follow_path astd (compare_path dep_path trans_path) [] [] []
		in let separation_label_list=(List.map ASTD_transition.get_label (ASTD_astd.get_trans_synchronised separation))
		in if (((synchronized_prod_cons separation_label_list (producers@consummers))&&((consummers!=[])&&(producers!=[])))
			&& (List.mem label separation_label_list))
				then compare_prod_cons_trans_list astd tail dep_path producers consummers
				else label::(compare_prod_cons_trans_list astd tail dep_path producers consummers)
		end
	|[]->[]





let rec study_prod_cons_informations transition_list astd dependance_list = match dependance_list with
	|((having_list,dep_var,dep_path),trans_list)::tail->
			let (follow_astd,unnecessary,called,env)= follow_path astd dep_path [] [] []
			in let (dep_astd,all_unnecessary,_,_)=go_find_qchoice follow_astd dep_var unnecessary called env
			in let prod_list =find_producers (remove_var_list unnecessary (dep_var::having_list)) dep_astd []
			and cons_list =find_producers (dep_var::having_list) dep_astd []
			in (compare_prod_cons_trans_list astd trans_list dep_path prod_list cons_list)
				@(study_prod_cons_informations transition_list astd tail)
	|[]->[]


let rec remove_transitions_from_list informations_list to_remove_labels = match informations_list with
	|(label,dep_link,path)::tail-> if List.mem label to_remove_labels 
			then remove_transitions_from_list tail to_remove_labels
			else (label,dep_link,path)::(remove_transitions_from_list tail to_remove_labels)
	|[]->[]




(*renvoie la liste des transitions étudiées qui sont valides du point de vue des producteurs et consommateurs*)
let test_all_informations informations_list astd = 
		let to_remove = study_prod_cons_informations informations_list astd (flip_dependances informations_list)
		in remove_transitions_from_list informations_list to_remove


let rec reimplace_astd_automata astd_list name_to_find astd_to_reimplace = match (astd_list) with
  |(astd::tail) ->
            if (ASTD_astd.get_name astd)=name_to_find
                    then astd_to_reimplace::tail
                    else astd::(reimplace_astd_automata tail name_to_find astd_to_reimplace) 
  |_-> failwith "to reimplace sub-astd not_found"
;;




let rec insert_prod astd producers dep_var having_list = match astd with      
	|ASTD_astd.Kleene (name,sub_astd) -> 
		begin 
		ASTD_astd.Kleene (name,insert_prod sub_astd producers dep_var having_list)
		end

	|ASTD_astd.QSynchronisation (name,var,domain,synch_trans,sub_astd,users)-> 
		begin 
		ASTD_astd.QSynchronisation (name,var,domain,synch_trans,insert_prod sub_astd producers dep_var having_list,users)
		end

	|ASTD_astd.QChoice (name,var,domain,sub_astd,producers_list)-> 
		begin 
		if var=dep_var
			then ASTD_astd.QChoice (name,var,domain,sub_astd,((producers,having_list)::producers_list))
			else insert_prod sub_astd producers dep_var having_list
		end

	|ASTD_astd.Guard (name,predicate_list,sub_astd)-> 
		begin 
		ASTD_astd.Guard (name,predicate_list,insert_prod sub_astd producers dep_var having_list)
		end

	|ASTD_astd.Call (name,called_name,affectations)->
		begin ASTD_astd.Elem("fail")
(*bla bla insérer l'appel de l'astd si pas déjà appelé, et le rajouter ensuite à la liste des apppelés*)
		end

	|_ ->failwith "exact qchoice not_found for prod insertion"



	
let rec insert_prod_path astd path producers dep_var having_list= match astd with
	|ASTD_astd.Automata (name,astd_list,arrow_list,final_states,init) -> 
		begin 
		let name=List.hd path
		and new_path =List.tl path
		in let new_astd =(ASTD_astd.find_subastd name (ASTD_astd.get_sub astd)) 
		in if new_path=[]
			then let next= insert_prod new_astd producers dep_var having_list
				in ASTD_astd.Automata (name,reimplace_astd_automata astd_list name next,arrow_list,final_states,init)
			else let next= insert_prod_path new_astd new_path producers dep_var having_list
				in ASTD_astd.Automata (name,reimplace_astd_automata astd_list name next,arrow_list,final_states,init)
		end
      
	|ASTD_astd.Sequence (name,left_astd,right_astd) -> 
		begin
		let new_path=List.tl path
		in if List.hd path = "l_seq"
			then if new_path=[]
				then ASTD_astd.Sequence (name,insert_prod left_astd producers dep_var having_list,right_astd) 
				else ASTD_astd.Sequence (name,insert_prod_path left_astd new_path producers dep_var having_list,right_astd)
			else if List.hd path = "r_seq" 
				then if new_path=[]
					then ASTD_astd.Sequence (name,left_astd,insert_prod right_astd producers dep_var having_list) 
					else ASTD_astd.Sequence (name,left_astd,insert_prod_path right_astd new_path producers dep_var having_list)
				else failwith "wrong path followed for insertion"
		end

	|ASTD_astd.Choice (name,first_astd,second_astd) -> 
		begin
		let new_path=List.tl path
		in if List.hd path = "f_choice"
			then if new_path=[]
				then ASTD_astd.Choice (name,insert_prod first_astd producers dep_var having_list,second_astd)
				else ASTD_astd.Choice (name,insert_prod_path first_astd new_path producers dep_var having_list,second_astd)
			else if List.hd path = "s_choice" 
				then if new_path=[]
					then ASTD_astd.Choice (name,first_astd,insert_prod second_astd producers dep_var having_list)
					else ASTD_astd.Choice (name,first_astd,insert_prod_path second_astd new_path producers dep_var having_list)
				else failwith "wrong path followed for insertion"
		end

	|ASTD_astd.Kleene (name,sub_astd) -> 
		begin 
		ASTD_astd.Kleene (name,insert_prod_path sub_astd path producers dep_var having_list)
		end

	|ASTD_astd.Synchronisation (name,synch_trans,sub_astd1,sub_astd2) -> 
		begin
		let new_path=List.tl path
		in if List.hd path = "l_synch"
			then if new_path=[]
				then ASTD_astd.Synchronisation (name,synch_trans,insert_prod sub_astd1 producers dep_var having_list,sub_astd2)
				else ASTD_astd.Synchronisation (name,synch_trans,insert_prod_path sub_astd1 new_path producers dep_var having_list, sub_astd2)
			else if List.hd path = "r_synch" 
				then if new_path=[]
					then ASTD_astd.Synchronisation (name,synch_trans,sub_astd1,insert_prod sub_astd2 producers dep_var having_list)
					else ASTD_astd.Synchronisation (name,synch_trans,sub_astd1,insert_prod_path sub_astd2 new_path producers dep_var having_list)
				else failwith "wrong path followed for insertion"
		end

	|ASTD_astd.QSynchronisation (name,var,domain,synch_trans,sub_astd,users)-> 
		begin 
		ASTD_astd.QSynchronisation (name,var,domain,synch_trans,insert_prod_path sub_astd path producers dep_var having_list,users)
		end

	|ASTD_astd.QChoice (name,var,domain,sub_astd,producers_list)-> 
		begin 
		ASTD_astd.QChoice (name,var,domain,insert_prod_path sub_astd path producers dep_var having_list,producers_list)
		end

	|ASTD_astd.Guard (name,predicate_list,sub_astd)-> 
		begin 
		ASTD_astd.Guard (name,predicate_list,insert_prod_path sub_astd path producers dep_var having_list)
		end

	|ASTD_astd.Call (name,called_name,affectations)->
		begin ASTD_astd.Elem("fail")
(*bla bla insérer l'appel de l'astd si pas déjà appelé, et le rajouter ensuite à la liste des apppelés*)
		end

	|_ ->failwith "not found insertion place"




let rec find_origin_transition_automata name arrow_list arrow_current_study met_states init var_list = match arrow_current_study with
	|ASTD_arrow.Local(from_state,to_state,transition,predicate_list,from_final_state )::tail->
	if to_state=name
		then if from_state= init 
			then if (study_parameters var_list (ASTD_transition.get_params transition)) 
					then (transition)
						::(find_origin_transition_automata name arrow_list tail met_states init var_list)
					else (find_origin_transition_automata name arrow_list tail met_states init var_list)
			else if List.mem from_state met_states
				then find_origin_transition_automata name arrow_list tail met_states init var_list
				else let deeper_producers=
						find_origin_transition_automata name arrow_list arrow_list (to_state::met_states) to_state var_list
					and other_roads=find_origin_transition_automata name arrow_list arrow_current_study met_states init var_list
					in (deeper_producers)@(other_roads)
		else find_origin_transition_automata name arrow_list tail met_states init var_list
	|other_arrow::tail->find_origin_transition_automata name arrow_list tail met_states init var_list
	|[]->[]






let rec get_all_producers_for_studied_transition label var_list astd env called= match astd with
	|ASTD_astd.Automata (name,astd_list,arrow_list,final_states,init) -> 
			get_first_transition_automata arrow_list astd_list init label var_list called env
	|ASTD_astd.Kleene (name,sub_astd) -> get_all_producers_for_studied_transition label var_list sub_astd env called
	|ASTD_astd.Sequence (name,left_astd,right_astd) -> if ASTD_state.is_final astd (ASTD_state.init_env (left_astd) env) env
								then (get_all_producers_for_studied_transition label var_list right_astd env called)
								else (get_all_producers_for_studied_transition label var_list left_astd env called)
	|ASTD_astd.Choice (name,first_astd,second_astd) -> 
				if is_included label first_astd called
						then if is_included label second_astd called
							then (get_all_producers_for_studied_transition label var_list first_astd env called)
								@(get_all_producers_for_studied_transition label var_list second_astd env called)
							else get_all_producers_for_studied_transition label var_list first_astd env called
						else if is_included label second_astd called
							then get_all_producers_for_studied_transition label var_list second_astd env called
							else []
	|ASTD_astd.Synchronisation (name,synch_trans,sub_astd1,sub_astd2) -> 
						(get_all_producers_for_studied_transition label var_list sub_astd1 env called)
						@(get_all_producers_for_studied_transition label var_list sub_astd2 env called)
	|ASTD_astd.QSynchronisation (name,var,domain,synch_trans,sub_astd,users)-> []
	|ASTD_astd.QChoice (name,var,domain,sub_astd,producers_list)-> 
				let bind_env=ASTD_environment.bind var (ASTD_term.Const(ASTD_constant.FreeConst))
                                in get_all_producers_for_studied_transition label var_list sub_astd (ASTD_environment.add_binding bind_env env) called
	|ASTD_astd.Guard (name,predicate_list,sub_astd)-> get_all_producers_for_studied_transition label var_list sub_astd env called
	|ASTD_astd.Call (name,called_name,affectations)->[]
	|_->[]




and get_first_transition_automata arrow_list astd_list init studied_transition var_list called env = match astd_list with
	|astd::tail->if init=(ASTD_astd.get_name astd)
			then get_all_producers_for_studied_transition studied_transition var_list astd env called
			else if (is_included studied_transition astd called)
				then let name= ASTD_astd.get_name astd
					in let possible_producers= (find_origin_transition_automata name arrow_list arrow_list [] init var_list)
					in let deep_producers=(study_variables possible_producers var_list)
					in (deep_producers@(get_first_transition_automata arrow_list tail init studied_transition var_list called env))
				else (get_first_transition_automata arrow_list tail init studied_transition var_list called env)
	|[]->[]







(*gets every productor relative to the studied problem transition*)
let rec get_complete_dep_corresponding_to_transition label dep_informations astd = match dep_informations with
	|(ASTD_astd.Dependant(var,(var_list,dep_var,dep_path),empty_prod,dependancy_list))::tail->
		let new_path=get_complete_dep_corresponding_to_transition label dependancy_list astd
		in let (dep_astd,unnecessary,called1,env1)= follow_path astd dep_path [] [] []
		in let (deepest,unnecessary2,called2,env2)=go_find_qchoice dep_astd dep_var unnecessary called1 env1
		in let prod_list=get_all_producers_for_studied_transition label (remove_var_list unnecessary (dep_var::var_list)) deepest env2 called2
		in (ASTD_astd.Dependant(var,(var_list,dep_var,dep_path),prod_list,new_path))
					::(get_complete_dep_corresponding_to_transition label tail astd)
	|ASTD_astd.Direct(x)::tail-> (ASTD_astd.Direct(x))::(get_complete_dep_corresponding_to_transition label tail astd)
	|[]->[]


let rec actualise_informations_with_producers astd valid_informations = match valid_informations with
	|(label,dep_link,path)::tail->let actualised_link = get_complete_dep_corresponding_to_transition label dep_link astd
					in (label,(actualised_link),path)::(actualise_informations_with_producers astd tail)
	|[]->[]


let rec extract_user_information link user_var = match link with
	|(ASTD_astd.Direct x)::tail->(extract_user_information tail user_var)
	|(ASTD_astd.Dependant (var,(quantifiers,dep_var,path),prod,dep_informations))::tail->
			if var=user_var
				then (dep_informations,quantifiers,tail)
				else let (informations,quantifiers_needed,not_used)=extract_user_information tail user_var
					in (informations,quantifiers_needed,(ASTD_astd.Dependant (var,(quantifiers,dep_var,path),prod,dep_informations))
						::not_used)
	|[]->([],[],[])


	
let rec insert_one_user astd label link path = match astd with
	|ASTD_astd.Automata (name,astd_list,arrow_list,final_states,init) -> 
	begin 
	let name=List.hd path
	and new_path =List.tl path
	in let new_astd =(ASTD_astd.find_subastd name (ASTD_astd.get_sub astd)) 
	in let next= insert_one_user new_astd label link new_path
	in ASTD_astd.Automata (name,reimplace_astd_automata astd_list name next,arrow_list,final_states,init)
	end
      
	|ASTD_astd.Sequence (name,left_astd,right_astd) -> 
		begin
		let new_path=List.tl path
		in if List.hd path = "l_seq"
			then ASTD_astd.Sequence (name,insert_one_user left_astd label link new_path,right_astd)
			else if List.hd path = "r_seq" 
				then ASTD_astd.Sequence (name,left_astd,insert_one_user right_astd label link new_path)
				else failwith "wrong path followed for insertion"
		end

	|ASTD_astd.Choice (name,first_astd,second_astd) -> 
		begin
		let new_path=List.tl path
		in if List.hd path = "f_choice"
			then ASTD_astd.Choice (name,insert_one_user first_astd label link new_path,second_astd)
			else if List.hd path = "s_choice" 
				then ASTD_astd.Choice (name,first_astd,insert_one_user second_astd label link new_path)
				else failwith "wrong path followed for insertion"
		end

	|ASTD_astd.Kleene (name,sub_astd) -> 
		begin 
		ASTD_astd.Kleene (name,insert_one_user sub_astd label link path)
		end

	|ASTD_astd.Synchronisation (name,synch_trans,sub_astd1,sub_astd2) -> 
		begin
		let new_path=List.tl path
		in if List.hd path = "l_synch"
			then ASTD_astd.Synchronisation (name,synch_trans,insert_one_user sub_astd1 label link new_path, sub_astd2)
			else if List.hd path = "r_synch" 
				then ASTD_astd.Synchronisation (name,synch_trans,sub_astd1,insert_one_user sub_astd2 label link new_path)
				else failwith "wrong path followed for insertion"
		end

	|ASTD_astd.QSynchronisation (name,var,domain,synch_trans,sub_astd,users)-> 
		begin 
		let (dep_informations,quantifiers,not_to_insert)= extract_user_information link var
		in if not_to_insert=[]
			then if dep_informations=[]
				then ASTD_astd.QSynchronisation (name,var,domain,synch_trans,sub_astd,users)
				else ASTD_astd.QSynchronisation (name,var,domain,synch_trans,
									sub_astd,
									(label,quantifiers,dep_informations)::users)
			else if dep_informations=[]
				then ASTD_astd.QSynchronisation (name,var,domain,synch_trans,
								insert_one_user sub_astd label link path,
								users)
				else ASTD_astd.QSynchronisation (name,var,domain,synch_trans,
									(insert_one_user sub_astd label link path),
									(label,quantifiers,dep_informations)::users)
		end

	|ASTD_astd.QChoice (name,var,domain,sub_astd,producers_list)-> 
		begin 
		ASTD_astd.QChoice (name,var,domain,insert_one_user sub_astd label link path,producers_list)
		end

	|ASTD_astd.Guard (name,predicate_list,sub_astd)-> 
		begin 
		ASTD_astd.Guard (name,predicate_list,insert_one_user sub_astd label link path)
		end

	|ASTD_astd.Call (name,called_name,affectations)->
		begin ASTD_astd.Elem("fail")
(*bla bla insérer l'appel de l'astd si pas déjà appelé, et le rajouter ensuite à la liste des apppelés*)
		end

	|_ ->failwith "not found insertion place"




(*insère les infos relatives à la kappa indirecte au niveau des qsynch*)
let rec insert_users valid_informations astd=match valid_informations with
	|(label,link,path)::tail -> let new_astd=insert_one_user astd label link path
					in insert_users tail new_astd
	|[]->astd

(*fonction globale permettant toute l'analyse statique*)
let rec static_analysis astd = 
	let informations_list = sum_up_informations astd
	in let valid_informations= test_all_informations informations_list astd
	in let actualized_informations=  actualise_informations_with_producers astd valid_informations
	in let new_astd_prod=insert_producers astd (flip_dependances valid_informations)
	in let new_astd=insert_users actualized_informations new_astd_prod
	in if astd=new_astd
		then astd
		else static_analysis astd





(**     Part6 : gestion des infos en base de données*)






let _ASTD_dependance_table_ = Hashtbl.create 5 




let memorise_dependancy var quant_var quant_val value prod = 
			Hashtbl.add _ASTD_dependance_table_ (prod,var,quant_var,quant_val) (value,time())


let retrieve_dependancy var quant_var quant_val prod = 
			Hashtbl.find _ASTD_dependance_table_ (prod,var,quant_var,quant_val) 






let retrieve_right_dependancy var quant_var quant_val prod_list = match prod_list with
	|prod::tail->let (next_value,next_time)=retrieve_right_dependancy var quant_var quant_val tail
			in let (current_value,time)=begin 
							(try retrieve_dependancy var quant_var quant_val prod
                           				with _ ->(ASTD_constant.FreeConst,0)) 
						end
			in if next_time > time 
				then (next_value,next_time)
				else (current_value,time)
	|[]->(ASTD_constant.FreeConst,0)



(**      Part7: utilisation de l'algorithme*)


(*gets the vector of values corresponding to a vector of variables*)
let rec get_values_in_environment var_list env = match var_list with
	|var::tail->let associated_val=ASTD_term.extract_constant_from_term(ASTD_environment.find_value_of env var)
			in associated_val::(get_values_in_environment tail env)
	|[]->[]







(*gets the corresponding value for a variable *)
let rec extract_value_from_var transition_params event_params dep_var = match (transition_params,event_params) with
	|(var::tail1,term::tail2)-> if var==(ASTD_term.Var dep_var) 
					then term
					else extract_value_from_var tail1 tail2 dep_var
	|([],[])->failwith "not found"
	|_->failwith "the two lists should have the same number of elements"



(*test et enregistrement de valeur pour producteur*)
let rec productor_action transition_params dep_var event productor_list env=match productor_list with
	|(label_list,having_var)::tail->if List.mem (ASTD_event.get_label event) label_list
					then let value_vect = get_values_in_environment having_var env
						and dep_value = extract_value_from_var transition_params (ASTD_event.get_const event) dep_var
						in (memorise_dependance dep_var having_var value_vect dep_value)
					else (productor_action transition_params dep_var event tail env)
	|[]->()











let rec fuse_var_list list1 list2 = match list1 with
	|var::tail->if List.mem var list2
			then fuse_var_list tail list2
			else (fuse_var_list tail (var::list2))
	|[]->list2




let rec extract_quantifiers_from_dep dep = match dep with
	|ASTD_astd.Direct(var)-> [var]
	|ASTD_astd.Dependant(var,dep,prod,dep_path)->(extract_quantifiers_from_dep_list dep_path)

and extract_quantifiers_from_dep_list dep_list = match dep_list with
	|dep::tail-> fuse_var_list (extract_quantifiers_from_dep dep) (extract_quantifiers_from_dep_list tail)
	|[]->[]






(*recherche dans les listes d'users si c'en est un*)
let rec compare_label_in_users label_event users_list=match users_list with
	|(transition_label,quantifiers,dep_informations)::tail->
		if transition_label=label_event 
			then (true,dep_informations,quantifiers)
			else compare_label_in_users label_event tail
	|[]->(false,[],[])



(*récupère le vect de valeurs associé à une liste de variables en se servant du kappa direct*)
let rec get_value_vect quantifiers transition_params event_params= match quantifiers with
	|var::tail->(extract_value_from_var transition_params event_params var)::(get_value_vect tail transition_params event_params)
	|[]->[]


let rec extract_val_from_list searched_var quant_var quant_val = match (quant_var,quant_val) with
	|(var::tail1,value::tail2)->if var=searched_var
					then value
					else (extract_val_from_list searched_var tail1 tail2)
	|_->failwith "value not found"





let rec study_dependance dep_list quant_var quant_val = match dep_list with
	|dep::tail-> (get_val_var dep quant_var quant_val)::(study_dependance tail quant_var quant_val)
	|[]->[]

and get_val_var dep quant_var quant_val = match dep with
	|ASTD_astd.Direct(var)->(var,extract_val_from_list var quant_var quant_val)
	|ASTD_astd.Dependant(var,dependancy,prod,dep_path)->
		let (var_list,val_list) = List.split (study_dependance dep_path quant_var quant_val) 
		in let (value,time)=(retrieve_right_dependancy var var_list val_list prod)
		in if value=0 
			then failwith "not found value"
			else (var,value)




let rec get_val_vect quantifiers values_of_needed_var = match quantifiers with
	|var::tail -> (List.assoc var values_of_needed_var )::(get_val_vect tail values_of_needed_var)
	|[]->[]





(*test et recherche de valeur utile pour user*)(**NB : a besoin d'une révision pour prendre en compte les producteurs*)
let get_val_if_user dep_var event transition_params users_list env=
	let (is_user,dep_informations,quant_var) = compare_label_in_users (ASTD_event.get_label event) users_list
	in if is_user 
		then begin 
			let needed_var=(extract_quantifiers_from_dep_list dep_informations)
			in let values_of_needed_var = (get_value_vect needed_var transition_params (ASTD_event.get_const event))	
			in let var_val_assoc=study_dependance dep_informations needed_var values_of_needed_var
			in let quant_val=get_val_vect quant_var var_val_assoc
			in (true,(retrieve_dependance dep_var quant_var quant_val))
			end
		else begin 
			(false,ASTD_constant.FreeConst)
			end




