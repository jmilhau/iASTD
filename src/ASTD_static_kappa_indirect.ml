

    let val_debug = ref true;;
    let debug m = if (!val_debug) 
                            then (print_endline m )
                            else begin end;;
    let debug_on () = (val_debug := true);;


let rec is_included list1 list2 = match list1 with
	|h::t->if List.mem h list2
		then is_included t list2
		else false
	|[]-> true

let rec intersection list1 list2 = match list1 with
	|h::t->if List.mem h list2
		then h::(intersection t list2)
		else intersection t list2
	|[]-> []

let rec union list1 list2 = match list1 with
	|h::t->if List.mem h list2
		then begin debug "already here"; (union t list2)end
		else begin debug "add" ; h::(union t list2) end
	|[]-> list2

let rec remove list1 list2 = match list2 with
	|h::t->if List.mem h list1
		then (remove list1 t)
		else h::(remove list1 t)
	|[]-> []

let rec epurate list1 = match list1 with
	|h::t-> let pur= epurate t
		in if List.mem h pur
			then pur
			else h::pur
	|[]->[]



let extract_transition_labels astd = 
	epurate (List.map (ASTD_transition.get_label) (ASTD_astd.get_sub_transitions [] astd))


 
let rec get_non_kappa_in_arrow_list arrows var_met path= match arrows with
	|arrow::t->let params= ASTD_transition.get_params (ASTD_arrow.get_transition arrow)
			in if (is_included (ASTD_term.parameters_of_variables var_met) params)
				then (get_non_kappa_in_arrow_list t var_met path)
				else (arrow,path, List.map (ASTD_term.string_of) (remove (params) (ASTD_term.parameters_of_variables var_met)))::(get_non_kappa_in_arrow_list t var_met path)
	|[]->[]



let rec get_var_from_vect vect = match vect with
	|(var,term)::tail-> var::(get_var_from_vect tail)
	|[]->[]


let rec mix_with vect var_met = epurate (var_met@(get_var_from_vect vect))


let rec extract_non_kappa_direct removed var_met call_path path astd  = 
begin debug ("extract non kappa : "^(string_of_int (List.length var_met))^" var met in"^(ASTD_astd.get_name astd));
match astd with
	|ASTD_astd.Automata (name,sub_astd,arrows,s_final,d_final,init) -> 
		let sub_arrows=List.concat (List.map (extract_non_kappa_direct removed var_met call_path (path@[name])) sub_astd)
		in let present =(get_non_kappa_in_arrow_list arrows var_met (path@[name]))
		in begin debug ((string_of_int (List.length present))^" non kappa in "^name);
			present@sub_arrows
			end

	|ASTD_astd.Sequence (name,left,right) -> 
		(extract_non_kappa_direct removed var_met call_path (path@[name]) left)@(extract_non_kappa_direct removed var_met call_path (path@[name]) right)

	|ASTD_astd.Choice (name,left,right) -> 
		(extract_non_kappa_direct removed var_met call_path (path@[name]) left)@(extract_non_kappa_direct removed var_met call_path (path@[name]) right)

	|ASTD_astd.Kleene (name,sub) -> 
		(extract_non_kappa_direct removed var_met call_path (path@[name]) sub)

    
	|ASTD_astd.Synchronisation (name,delta,left,right) -> 
		(extract_non_kappa_direct removed var_met call_path (path@[name]) left)@(extract_non_kappa_direct removed var_met call_path (path@[name]) right)

	|ASTD_astd.Guard (name,pred,sub) ->
		(extract_non_kappa_direct removed var_met call_path (path@[name]) sub)

	|ASTD_astd.QChoice (name,var,val_list,dep,sub) ->
		(extract_non_kappa_direct (var::removed) var_met call_path (path@[name]) sub)

	|ASTD_astd.QSynchronisation (name,var,val_list,delta,opt,sub )-> 
		(extract_non_kappa_direct removed (var::var_met) call_path (path@[name]) sub)

	|ASTD_astd.Call (name,called_name,vect) ->
		if List.mem name call_path
		then []
		else (extract_non_kappa_direct removed (remove removed (mix_with vect var_met)) (name::call_path) (path@[name]) (ASTD_astd.get_astd called_name))

	|ASTD_astd.Elem (a) ->  []
end



let rec extract_df var_met wait call_path path astd  = 
begin (*debug ("extract df from "^(ASTD_astd.get_name astd));*)
match astd with
	|ASTD_astd.Automata (name,sub_astd,arrows,s_final,d_final,init) -> 

                List.fold_left union [] (List.map (extract_df var_met wait call_path (path@[name])) sub_astd)

	|ASTD_astd.Sequence (name,left,right) -> 
		union (extract_df var_met wait call_path (path@[name]) left) (extract_df var_met wait call_path (path@[name]) right)

	|ASTD_astd.Choice (name,left,right) -> 
		union (extract_df var_met wait call_path (path@[name]) left) (extract_df var_met wait call_path (path@[name]) right)

	|ASTD_astd.Kleene (name,sub_astd) -> 
		(extract_df var_met wait call_path (path@[name]) sub_astd)
    
	|ASTD_astd.Synchronisation (name,delta,left,right) -> 
		if is_included (intersection (extract_transition_labels left) (extract_transition_labels right)) delta
			then union (extract_df var_met wait call_path (path@[name]) left) (extract_df var_met wait call_path (path@[name]) right)
			else union (extract_df var_met true call_path (path@[name]) left) (extract_df var_met true call_path (path@[name]) right)

	|ASTD_astd.Guard (name,pred,sub) ->
		extract_df var_met wait call_path (path@[name]) sub

	|ASTD_astd.QChoice (name,var,val_list,dep,sub) ->
		if wait
			then extract_df var_met wait call_path (path@[name]) sub
			else ((path@[name]),var_met,var,[],[])::(extract_df var_met wait call_path (path@[name]) sub)


	|ASTD_astd.QSynchronisation (name,var,val_list,delta,opt,sub )-> 
		if is_included (extract_transition_labels sub) delta
			then extract_df var_met wait call_path (path@[name]) sub
			else extract_df (var::var_met) false call_path (path@[name]) sub

	|ASTD_astd.Call (name,called_name,vect) ->
		if List.mem name call_path
			then []
			else extract_df (mix_with vect var_met) wait (name::call_path) (path@[name]) (ASTD_astd.get_astd called_name)

	|ASTD_astd.Elem (a) -> []
end

let rec find_astd astd path = 
	if (List.hd path)!=(ASTD_astd.get_name astd)
	then failwith "mistake of matching in find_path"
	else let sub_path = List.tl path
		in if sub_path=[]
			then astd
			else match astd with
				|ASTD_astd.Automata (name,sub_astd,arrows,s_final,d_final,init) ->
					find_astd (ASTD_astd.find_subastd (List.hd(sub_path)) sub_astd) sub_path

				|ASTD_astd.Sequence (name,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = (List.hd sub_path)
						then find_astd left (List.tl path)
						else find_astd right (List.tl path)

				|ASTD_astd.Choice (name,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = (List.hd sub_path)
						then find_astd left (List.tl path)
						else find_astd right (List.tl path)

				|ASTD_astd.Kleene (name,sub) ->
					find_astd sub sub_path
    
				|ASTD_astd.Synchronisation (name,delta,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = (List.hd sub_path)
						then find_astd left (List.tl path)
						else find_astd right (List.tl path)

				|ASTD_astd.Guard (name,pred,sub) ->
					find_astd sub sub_path

				|ASTD_astd.QChoice (name,var,val_list,dep,sub) ->
					find_astd sub sub_path

				|ASTD_astd.QSynchronisation (name,var,val_list,delta,opt,sub )->
					find_astd sub sub_path

				|ASTD_astd.Call (name,called_name,vect) ->
					find_astd (ASTD_astd.get_astd called_name) sub_path

				|ASTD_astd.Elem (a) ->failwith "find_astd: impossible to go deeper than elem"






let rec find_split astd path1 path2 =
	let (head1,head2,sub_path1,sub_path2) = (List.hd path1,List.hd path2,List.tl path1,List.tl path2)
	in begin debug ("find split : "^head1^" vs "^head2);
		if ((head1)!=(ASTD_astd.get_name astd))||((head2)!=(ASTD_astd.get_name astd))
		then ([],false)
		else if head1=head2
			then if (sub_path1=[])||(sub_path2=[])
				then failwith "mistake in find_split: end of path"
				else if (List.hd sub_path1)!=(List.hd sub_path2)
 					then if ASTD_astd.is_synchro astd 
						then begin debug ((string_of_int (List.length (ASTD_astd.get_trans_synchronised astd)))
									^" trans in split "^(ASTD_astd.get_name astd));
							(ASTD_astd.get_trans_synchronised astd,true) 
							end
						else ([],false)
					else match astd with
				|ASTD_astd.Automata (name,sub_astd,arrows,s_final,d_final,init) ->
					find_split (ASTD_astd.find_subastd (head1) sub_astd) sub_path1 sub_path2

				|ASTD_astd.Sequence (name,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = head1
						then find_split left sub_path1 sub_path2
						else find_split right sub_path1 sub_path2

				|ASTD_astd.Choice (name,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = head1
						then find_split left sub_path1 sub_path2
						else find_split right sub_path1 sub_path2

				|ASTD_astd.Kleene (name,sub) ->
					find_split sub sub_path1 sub_path2
    
				|ASTD_astd.Synchronisation (name,delta,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = head1
						then find_split left sub_path1 sub_path2
						else find_split right sub_path1 sub_path2

				|ASTD_astd.Guard (name,pred,sub) ->
					find_split sub sub_path1 sub_path2

				|ASTD_astd.QChoice (name,var,val_list,dep,sub) ->
					find_split sub sub_path1 sub_path2

				|ASTD_astd.QSynchronisation (name,var,val_list,delta,opt,sub )->
					find_split sub sub_path1 sub_path2

				|ASTD_astd.Call (name,called_name,vect) ->
					find_split (ASTD_astd.get_astd called_name) sub_path1 sub_path2

				|ASTD_astd.Elem (a) ->failwith "find_astd: impossible to go deeper than elem"

			else failwith "mistake in find_split: different path"

	end


let rec extract_unessential_var opti label = match opti with
	|(trans_label,path,var,dep_dep)::t->
		if trans_label = label 
			then var::(extract_unessential_var t label)
			else (extract_unessential_var t label)
	|[]->[]

let rec find_prod arrows init_name init opti_list var_list sub_produced= match arrows with
	|arrow::t->
		let trans=ASTD_arrow.get_transition arrow
		in let (essential_var) = (remove (extract_unessential_var opti_list (ASTD_transition.get_label trans)) var_list)
		in if (ASTD_arrow.get_from arrow)=init_name
			then if ((ASTD_astd.is_init_final init [])!="false")||(not (ASTD_arrow.get_from_final_state arrow))
				&& (is_included (ASTD_term.parameters_of_variables essential_var) (ASTD_transition.get_params trans) )
				then let (prod,produced)=(find_prod t init_name init opti_list var_list sub_produced)
					in((ASTD_transition.get_label trans)::prod,produced)
				else if sub_produced
					then find_prod t init_name init opti_list var_list sub_produced
					else ([],false)
			else find_prod t init_name init opti_list var_list sub_produced
	|[]->([],true)

let rec get_from_sub name arrows = match arrows with
	|(ASTD_arrow.From_sub(from,dest,through,trans,pred,final))::t->
		if through=name
			then (ASTD_arrow.From_sub(from,dest,through,trans,pred,final))::(get_from_sub name t)
			else get_from_sub name t
	|arrow::t-> get_from_sub name t
	|[]->[]

let rec get_to_sub name arrows = match arrows with
	|(ASTD_arrow.To_sub(from,dest,through,trans,pred,final))::t->
		if through=name
			then (ASTD_arrow.To_sub(from,dest,through,trans,pred,final))::(get_to_sub name t)
			else get_to_sub name t
	|arrow::t-> get_to_sub name t
	|[]->[]

let rec get_arrow_from name arrows = match arrows with
	|arrow::t-> if (ASTD_arrow.get_from arrow) = name
			then arrow::(get_arrow_from name t)
			else (get_arrow_from name t)
	|[]->[]


let rec get_arrow_to name arrows = match arrows with
	|arrow::t-> if (ASTD_arrow.get_to arrow) = name
			then arrow::(get_arrow_to name t)
			else (get_arrow_to name t)
	|[]->[]

let rec get_producers var_met var call_path opti_list from_sub astd=match astd with
	|ASTD_astd.Automata (name,sub_astd,arrows,s_final,d_final,init) ->
		let init_astd=ASTD_astd.find_subastd init sub_astd
		in let (sub_prod,sub_produced)=(get_producers var_met var call_path opti_list (get_from_sub init arrows) init_astd)
		in let (prod,produced)= find_prod (arrows@from_sub) init init_astd opti_list (var::var_met) sub_produced
		in (union sub_prod prod,produced)

	|ASTD_astd.Sequence (name,left,right) ->
		if (ASTD_astd.is_init_final left [])="false"
		then let (prod_l,always_l)=(get_producers var_met var call_path opti_list [] left) 
			and (prod_r,always_r)=(get_producers var_met var call_path opti_list [] right) 
			in (union prod_l prod_r,always_l && always_r) 
		else get_producers var_met var call_path opti_list [] left

	|ASTD_astd.Choice (name,left,right) ->
		let (prod_l,always_l)=(get_producers var_met var call_path opti_list [] left) 
		and (prod_r,always_r)=(get_producers var_met var call_path opti_list [] right) 
		in (union prod_l prod_r,always_l && always_r) 


	|ASTD_astd.Kleene (name,sub) ->
		get_producers var_met var call_path opti_list [] sub
    
	|ASTD_astd.Synchronisation (name,delta,left,right) ->
		let (prod_l,always_l)=(get_producers var_met var call_path opti_list [] left) 
		and (prod_r,always_r)=(get_producers var_met var call_path opti_list [] right) 
		in (union prod_l prod_r,always_l && always_r) 

	|ASTD_astd.Guard (name,pred,sub) ->
		get_producers var_met var call_path opti_list [] sub

	|ASTD_astd.QChoice (name,var,val_list,dep,sub) ->
		get_producers var_met var call_path opti_list [] sub

	|ASTD_astd.QSynchronisation (name,var,val_list,delta,opt,sub )->
		get_producers var_met var call_path opti_list [] sub

	|ASTD_astd.Call (name,called_name,vect) ->
		if List.mem name call_path
			then ([],false)
			else get_producers var_met var (name::call_path) opti_list [] ((ASTD_astd.get_astd called_name))

	|ASTD_astd.Elem (a) ->([],false)




let rec split_all_possible missing_found = match missing_found with
	|(dependency_path,dep_list,possible)::t-> if possible
					then let (dep,dep_l,poss)= split_all_possible t
						in if poss 
							then (dependency_path::dep,dep_list@dep_l,true)
							else ([],[],false)
					else ([],[],false)
	|[]->([],[],true)



let rec find_user_astd astd path var_to_find = 
	if (List.hd path)!=(ASTD_astd.get_name astd)
	then failwith "mistake of matching in find_user_astd"
	else let sub_path = List.tl path
		in if sub_path=[]
			then failwith " find_user_astd : not found before end"
			else match astd with
				|ASTD_astd.Automata (name,sub_astd,arrows,s_final,d_final,init) ->
					find_user_astd (ASTD_astd.find_subastd (List.hd(sub_path)) sub_astd) sub_path var_to_find

				|ASTD_astd.Sequence (name,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = (List.hd sub_path)
						then find_user_astd left sub_path var_to_find
						else find_user_astd right sub_path var_to_find

				|ASTD_astd.Choice (name,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = (List.hd sub_path)
						then find_user_astd left sub_path var_to_find
						else find_user_astd right sub_path var_to_find

				|ASTD_astd.Kleene (name,sub) ->
					find_user_astd sub sub_path var_to_find
    
				|ASTD_astd.Synchronisation (name,delta,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = (List.hd sub_path)
						then find_user_astd left sub_path var_to_find
						else find_user_astd right sub_path var_to_find

				|ASTD_astd.Guard (name,pred,sub) ->
					find_user_astd sub sub_path var_to_find

				|ASTD_astd.QChoice (name,var,val_list,dep,sub) ->
					find_user_astd sub sub_path var_to_find

				|ASTD_astd.QSynchronisation (name,var,val_list,delta,opt,sub )-> begin 
					if List.mem var var_to_find
					then if (remove [var] var_to_find)=[]
						then begin print_endline (ASTD_astd.get_name sub);sub end
						else begin find_user_astd sub sub_path (remove [var] var_to_find) end
					else find_user_astd sub sub_path var_to_find end

				|ASTD_astd.Call (name,called_name,vect) ->
					find_user_astd (ASTD_astd.get_astd called_name) sub_path var_to_find

				|ASTD_astd.Elem (a) ->failwith " find_user_astd: impossible to go deeper than elem"


let rec test_each_arrow_with_consummers cons final arrows = match arrows with
	|arrow::t-> if List.mem (ASTD_transition.get_label (ASTD_arrow.get_transition arrow)) cons
			then if List.mem (ASTD_arrow.get_to arrow) final
				then test_each_arrow_with_consummers cons final t
				else false
			else test_each_arrow_with_consummers cons final t
	|[]->true




let rec extract_astd_list final_list sub_astd_list = match final_list with
	|name::tail->(ASTD_astd.find_subastd name sub_astd_list)::(extract_astd_list tail sub_astd_list)
	|[]->[]


let rec exist_from_sub_arrow name arrows=match arrows with
	|(ASTD_arrow.From_sub(from,dest,through,trans,pred,final))::t->
		if through=name
			then true
			else exist_from_sub_arrow name t
	|arrow::t-> exist_from_sub_arrow name t
	|[]-> false

let rec exist_from name arrows=match arrows with
	|arrow::t->if (ASTD_arrow.get_from arrow)=name
			then true
			else exist_from name t
	|[]-> false

let rec get_labels arrows = match arrows with
	|arrow::t-> let sub= get_labels t 
			and label = ASTD_transition.get_label (ASTD_arrow.get_transition arrow)
			in if List.mem label sub
				then sub
				else label::sub
	|[]->[]

let rec get_consummers astd call_path f_sub t_sub wait =match astd with
	|ASTD_astd.Automata (name,sub_astd,arrows,s_final,d_final,init) -> 
		let (cons_shallow,final_shallow,is_consummed_shallow)=get_consummers_sf s_final sub_astd arrows f_sub t_sub
		and (cons_deep,final_deep,is_consummed_deep)=get_consummers_df d_final sub_astd call_path arrows f_sub t_sub
			in if (is_consummed_shallow && is_consummed_deep)
				then (union cons_shallow cons_deep, union final_shallow final_deep, true)
				else ([],[],false)

	|ASTD_astd.Sequence (name,left,right) -> 
		let (cons_r,final_r,consummed_r)=get_consummers right call_path [] [] false
		in if (ASTD_astd.is_init_final right [])!="false"
			then let (cons_l,final_l,consummed_l)=get_consummers left call_path [] [] false
				in if (consummed_l && consummed_r)
					then (union cons_l cons_r, union final_l final_r,true)
					else ([],[],false)
			else if consummed_r
				then (cons_r,final_r,true)
				else ([],[],false) 

	|ASTD_astd.Choice (name,left,right) -> 
		let (cons_l,final_l,consummed_l)=get_consummers left call_path [] [] false
		and (cons_r,final_r,consummed_r)=get_consummers right call_path [] [] false
		in if (consummed_l && consummed_r)
			then (union cons_l cons_r, union final_l final_r,true)
			else ([],[],false) 

	|ASTD_astd.Kleene (name,sub) ->
		if wait 
			then get_consummers sub call_path [] [] wait
    			else ([],[],false) 

	|ASTD_astd.Synchronisation (name,delta,left,right) -> 
		let (cons_l,final_l,consummed_l)=get_consummers left call_path [] [] false
		and (cons_r,final_r,consummed_r)=get_consummers right call_path [] [] false
		in if (consummed_l && consummed_r)
			then (union cons_l cons_r, union final_l final_r,true)
			else ([],[],false) 

	|ASTD_astd.Guard (name,pred,sub) -> get_consummers sub call_path [] [] wait

	|ASTD_astd.QChoice (name,var,val_list,dep,sub) -> get_consummers sub call_path [] [] wait

	|ASTD_astd.QSynchronisation (name,var,val_list,delta,opt,sub )-> get_consummers sub call_path [] [] wait

	|ASTD_astd.Call (name,called_name,vect) ->
		if List.mem name call_path
			then ([],[],false) 
			else get_consummers (ASTD_astd.get_astd called_name) (name::call_path) [] [] wait

	|ASTD_astd.Elem (a) -> ([],[],true) 

and get_consummers_df deep_final astd_list call_path arrows f_sub t_sub = match deep_final with
	|name::tail-> let astd=(ASTD_astd.find_subastd name astd_list)
			in if (exist_from name (arrows@f_sub))
			then ([],[],false)
			else let (cons_sub,final_sub,is_consummed_sub)=get_consummers astd call_path (get_from_sub name arrows) (get_to_sub name arrows) false
				and (cons,final,is_consummed)=get_consummers_df tail astd_list call_path arrows f_sub t_sub
				in if is_consummed_sub
					then if (ASTD_astd.is_init_final astd [])!="false"
						then (cons_sub@(cons@(get_labels(get_arrow_to name(arrows@t_sub)))),
							name::(final_sub @ final) , 
							is_consummed)
						else (cons_sub @ cons,
							final_sub @ final, 
							is_consummed)
					else ([],[],false)
	|[]->([],[],true) 

and get_consummers_sf shallow_final astd_list arrows f_sub t_sub = match shallow_final with 
	|name::tail-> let astd=(ASTD_astd.find_subastd name astd_list)
			in if (((ASTD_astd.get_sub_transitions [] astd)!=[])||(exist_from_sub_arrow name arrows)||(exist_from name (arrows@f_sub)) )
			then ([],[],false)
			else let (cons,final,consummed)= get_consummers_sf tail astd_list arrows f_sub t_sub
				in (cons@(get_labels ((get_to_sub name arrows)@(get_arrow_to name (arrows@t_sub)))),
					name::(final@(ASTD_astd.get_sub_names [] astd)),
					consummed)
	|[]->([],[],true) 

let rec remove_prod arrows prod = match arrows with
	|arrow::t->if List.mem (ASTD_transition.get_label (ASTD_arrow.get_transition arrow)) prod
			then (remove_prod t prod)
			else arrow::(remove_prod t prod)
	|[]-> []

let rec filter_final_trans arrows= match arrows with
	|arrow::t->if ASTD_arrow.get_from_final_state arrow
			then arrow::(filter_final_trans t)
			else (filter_final_trans t)
	|[]->[]


let rec is_always_produced astd prod call_path from_sub = match astd with
	|ASTD_astd.Automata (name,sub_astd,arrows,s_final,d_final,init) ->
		let first=(get_arrow_from init (arrows@from_sub))
		in let non_prod_first = remove_prod first prod
		in if (filter_final_trans non_prod_first)=[]
			then true
			else if (is_always_produced (ASTD_astd.find_subastd init sub_astd) prod call_path (get_from_sub init arrows)) 
				&&((ASTD_astd.is_init_final (ASTD_astd.find_subastd init sub_astd) [])="false")
				then true
				else false

	|ASTD_astd.Sequence (name,left,right) -> if (ASTD_astd.is_init_final right [])!="false"
		then (is_always_produced left prod call_path [])&&(is_always_produced right prod call_path [])
		else (is_always_produced left prod call_path [])

	|ASTD_astd.Choice (name,left,right) -> 
		(is_always_produced left prod call_path [])&&(is_always_produced right prod call_path [])

	|ASTD_astd.Kleene (name,sub) -> is_always_produced sub prod call_path []

	|ASTD_astd.Synchronisation (name,delta,left,right) -> 
		(is_always_produced left prod call_path [])&&(is_always_produced right prod call_path [])

	|ASTD_astd.Guard (name,pred,sub) -> is_always_produced sub prod call_path []

	|ASTD_astd.QChoice (name,var,val_list,dep,sub) -> is_always_produced sub prod call_path []

	|ASTD_astd.QSynchronisation (name,var,val_list,delta,opt,sub )-> is_always_produced sub prod call_path []

	|ASTD_astd.Call (name,called_name,vect) ->
		if List.mem name call_path
			then false 
			else is_always_produced (ASTD_astd.get_astd called_name) prod (name::call_path) []

	|ASTD_astd.Elem (a) -> false 



let rec find_from_sub arrows = match arrows with
	|arrow::t -> if ASTD_arrow.is_from_sub arrow
			then arrow::(find_from_sub t)
			else find_from_sub t
	|[]->[]

let rec remove_from_sub arrows = match arrows with
	|arrow::t -> if ASTD_arrow.is_from_sub arrow
			then (remove_from_sub t)
			else arrow::(remove_from_sub t)
	|[]->[]

let rec find_label label arrows = match arrows with
	|arrow::t -> if (ASTD_transition.get_label(ASTD_arrow.get_transition arrow)) = label
			then arrow::(find_label label t)
			else find_label label t
	|[]->[]

let rec get_sub_clues label astd_list = match astd_list with
	|astd::t-> if List.mem label (extract_transition_labels astd)
			then (ASTD_astd.get_name astd)::(get_sub_clues label t)
			else get_sub_clues label t
	|[]->[]

let rec extract_with_dest_from_list arrows dest_list = match arrows with
	|arrow::t->if List.mem (ASTD_arrow.get_to arrow) dest_list
			then arrow::(extract_with_dest_from_list t dest_list)
			else extract_with_dest_from_list t dest_list
	|[]->[]


let rec find_arrow_using_from_dest_origin_list arrows dest origin= match arrows with
	|arrow::t -> if (List.mem (ASTD_arrow.get_to arrow) dest)&&(List.mem (ASTD_arrow.get_from arrow) origin)
			then arrow::(find_arrow_using_from_dest_origin_list t dest origin)
			else (find_arrow_using_from_dest_origin_list t dest origin)
	|[] -> []

let rec find_arrow_using_from_dest_through_list arrows dest through= match arrows with
	|arrow::t -> if (List.mem (ASTD_arrow.get_to arrow) dest)&&(List.mem (ASTD_arrow.get_through arrow) through)
			then arrow::(find_arrow_using_from_dest_through_list t dest through)
			else (find_arrow_using_from_dest_through_list t dest through)
	|[] -> []

let rec get_first_trans astd fst need met used trans= 
	let (name,sub_astd,arrows,s_final,d_final,init)= ASTD_astd.get_data_automata astd
	in if (need=[init])||(need=[])
		then (fst,met)
		else let b=List.hd need
		in begin debug ("first_trans_from "^b);
			if b=init
			then get_first_trans astd fst ((List.tl need)@[init]) met used trans
			else let sub= epurate (List.map (ASTD_arrow.get_from) (extract_with_dest_from_list (get_from_sub init arrows) met))
				in if sub!=[]
					then begin debug "first_trans1";
						let b_astd= (ASTD_astd.find_subastd init sub_astd)
						in let (_,sub_met)=get_first_trans b_astd [] sub [ASTD_astd.get_init b_astd] [] []
						in let new_used= union (find_arrow_using_from_dest_origin_list 
										(remove trans arrows) 
										(union [b] sub_met) 
										(List.map (ASTD_astd.get_name) sub_astd))
									(find_arrow_using_from_dest_through_list 
										(find_from_sub (remove trans arrows)) 
										([b]) 
										(List.map (ASTD_astd.get_name) sub_astd))
							in get_first_trans 
								astd 
								(union fst (extract_with_dest_from_list 
										(get_arrow_from init arrows) 
										(union [b] sub_met)))
								(remove
									[b]
									(union need (union  (intersection 
											(List.map (ASTD_astd.get_name) sub_astd) 
											(List.map (ASTD_arrow.get_from) (new_used))) 	
										  (intersection 
											(List.map (ASTD_astd.get_name) sub_astd) 												(List.map (ASTD_arrow.get_through) (find_from_sub new_used))))))
								(union [name] met)
								new_used 
								(union trans new_used)
							end
					else begin debug "first_trans2";
						let new_used= union (find_arrow_using_from_dest_origin_list 
									(remove trans arrows) 
									([b]) 
									(List.map (ASTD_astd.get_name) sub_astd))
								(find_arrow_using_from_dest_through_list 
									(find_from_sub (remove trans arrows)) 
									([b]) 
									(List.map (ASTD_astd.get_name) sub_astd))
						in get_first_trans 
							astd 
							(union fst (extract_with_dest_from_list (get_arrow_from init arrows) [b]))
							(remove
								[b]
								(union need (union  (intersection 
										(List.map (ASTD_astd.get_name) sub_astd) 
										(List.map (ASTD_arrow.get_from) (new_used))) 
									  (intersection 
										(List.map (ASTD_astd.get_name) sub_astd) 											(List.map (ASTD_arrow.get_through) (find_from_sub new_used ))) )))
							(union [name] met)
							new_used 
							(union trans new_used)
						end
			end

let rec is_produced astd prod label call_path from_sub = 
begin
debug ("is produced en cours in "^(ASTD_astd.get_name astd));
match astd with
	|ASTD_astd.Automata (name,sub_astd,arrows,s_final,d_final,init) -> 
		let init_astd=(ASTD_astd.find_subastd init sub_astd)
		and from_sub_init= get_from_sub init arrows
		in let o= union(List.map (ASTD_arrow.get_from) (find_label label (union (remove_from_sub arrows) from_sub_init)))
				(union (List.map (ASTD_arrow.get_through) (find_label label (find_from_sub arrows))) 
				(get_sub_clues label sub_astd))
		in let (first,met)=get_first_trans astd [] o [init] [] []
		in let first_trans=(find_label label (get_arrow_from init (arrows@from_sub)))@(remove_prod (first) prod)
		in if ((filter_final_trans first_trans)!=[])
				&&(not(is_always_produced init_astd prod call_path from_sub) 
					||((ASTD_astd.is_init_final init_astd [])!="false"))

			then false
			else if List.mem label (extract_transition_labels init_astd)
				then is_produced init_astd prod label call_path from_sub_init
				else true
	|ASTD_astd.Sequence (name,left,right) -> 
		if List.mem label (extract_transition_labels right)
			then if (ASTD_astd.is_init_final right [])!="false"
				then (is_always_produced left prod call_path from_sub)&&(is_produced right prod label call_path from_sub)
				else is_always_produced left prod call_path from_sub
			else if List.mem label (extract_transition_labels left)
				then is_produced left prod label call_path from_sub
				else failwith "is_produced: absent from seq"

	|ASTD_astd.Choice (name,left,right) -> 
		if List.mem label (extract_transition_labels left)
			then if List.mem label (extract_transition_labels right)
				then (is_produced left prod label call_path [])&&(is_produced right prod label call_path [])
				else is_produced left prod label call_path []
			else if List.mem label (extract_transition_labels right)
				then is_produced right prod label call_path []
				else failwith "is_produced: absent from choice"

	|ASTD_astd.Kleene (name,sub) -> is_produced sub prod label call_path []

	|ASTD_astd.Synchronisation (name,delta,left,right) -> 
		if List.mem label (extract_transition_labels left)
			then if List.mem label (extract_transition_labels right)
				then (is_produced left prod label call_path [])&&(is_produced right prod label call_path [])
				else (is_produced left prod label call_path [])&&(is_always_produced right prod call_path from_sub)
			else if List.mem label (extract_transition_labels right)
				then (is_produced right prod label call_path [])&&(is_always_produced left prod call_path from_sub)
				else failwith "is_produced: absent from synch"

	|ASTD_astd.Guard (name,pred,sub) -> is_produced sub prod label call_path []

	|ASTD_astd.QChoice (name,var,val_list,dep,sub) -> is_produced sub prod label call_path []

	|ASTD_astd.QSynchronisation (name,var,val_list,delta,opt,sub )-> is_produced sub prod label call_path []

	|ASTD_astd.Call (name,called_name,vect) ->
		if List.mem name call_path
			then false 
			else is_produced (ASTD_astd.get_astd called_name) prod label (name::call_path) []

	|ASTD_astd.Elem (a) -> false 

end

let rec is_consummed astd = 
	let (cons,final,consummed)=get_consummers astd [] [] [] true
	in if (test_each_arrow_with_consummers cons final (ASTD_astd.get_sub_arrows [] astd))&&(consummed)
		then begin debug ("consummed "^(ASTD_astd.get_name astd));(cons,true) end
		else begin debug ("not consummed "^(ASTD_astd.get_name astd));([],false) end



let extract_producers var_met var call_path opti_list from_sub astd = 
	let (prod,produced)=get_producers var_met var call_path opti_list from_sub astd
	in if produced
		then begin debug ((ASTD_astd.get_name astd)^" is produced"); (prod,produced) end
		else begin debug ((ASTD_astd.get_name astd)^" is not produced"); (prod,produced) end


let rec update_producers_cons astd dep_list opti_list = match dep_list with
	|(path,var_met,var,_,_)::tail-> begin debug "update_prod_cons";
					let sub_astd = find_astd astd path
					in let (prod,produced) = extract_producers var_met var [] opti_list [] sub_astd
					and (cons,consummed)= is_consummed sub_astd
					in if produced && consummed
						then begin debug ("produced and consummed "^(ASTD_astd.get_name sub_astd));
							(path,var_met,var,prod,cons)::(update_producers_cons astd tail opti_list)
							end
						else begin debug ("not produced or not consummed "^(ASTD_astd.get_name sub_astd));
							update_producers_cons astd tail opti_list
							end
					end
	|[]->begin debug "end of update_prod_cons"; [] end 

let match_user_creator used_var arrow var_met var prod cons_creator dep_path trans_path astd =
	let (delta,splitted)=find_split astd dep_path trans_path
	and label=(ASTD_transition.get_label(ASTD_arrow.get_transition arrow))
	in if splitted 
		then let user_astd= find_user_astd astd trans_path (var::used_var)
			in let(cons_user,consummed_user)= is_consummed user_astd
				and produced=(is_produced user_astd prod label [] [])
				in let test = (is_included cons_creator cons_user)
				in begin (begin if test then debug "produced" else debug ("not produced "^(List.hd cons_creator)^" vs "^(List.hd cons_user)) end);
					if produced
					&&(List.mem label delta)
					&&(is_included prod delta)
					&&(is_included cons_creator cons_user)
					&&(is_included cons_creator delta)
					&&(consummed_user)

					then begin debug ("valid dep with label "^label);true end
					else begin debug ("unvalid dep with label "^label);false end
					end
		else begin debug "impossible to find a good split" ;false end



let rec extract_dependency_path dependencies var_usable save_dep arrow astd trans_path var = match dependencies with
	|(path,var_met,dep_var,prod,cons)::t->
		if (match_user_creator var_met arrow var_met var prod cons path trans_path astd)&&(var=dep_var)
			then let missing=remove var_usable var_met
				in let missing_found=List.map (extract_dependency_path save_dep var_usable save_dep arrow astd trans_path) missing
				in let (dep_dep,dep_list,possible)=split_all_possible missing_found
				in if possible
					then (ASTD_optimisation.Dep_path(var,(path,var_met,dep_var,prod,cons),dep_dep),(path,var_met,dep_var,prod,cons)::dep_list,true)
					else extract_dependency_path t var_usable save_dep arrow astd trans_path var
			else begin debug ("not_matched dependency with the label "^(ASTD_transition.get_label(ASTD_arrow.get_transition arrow))^" or var "^dep_var^" is not "^var);
				extract_dependency_path t var_usable save_dep arrow astd trans_path var
				end
	|[]->let empty=([],[],var,[],[])
		in (ASTD_optimisation.Dep_path(var,empty,[]),[],false)



let rec extract_var params =match params with
	|(ASTD_term.Var a)::t-> a::(extract_var t)
	|h::t-> extract_var t
	|[]->[]



let rec find_one_opt arrow path astd dependencies opt_list var = match opt_list with
	|(opt_label,opt_path,opt_var,dep_dep)::t-> 
		if (path=opt_path)&&((ASTD_transition.get_label (ASTD_arrow.get_transition arrow))=opt_label)&&(var=opt_var)
			then begin debug ("find one opt / already optimised "^opt_label^" var "^opt_var) ;
					([(opt_label,opt_path,opt_var,dep_dep)],[]) end
			else find_one_opt arrow path astd dependencies t var
	|[]->begin debug ("find one opt / looking if "^(ASTD_transition.get_label (ASTD_arrow.get_transition arrow))^" is optimizable for var "^var);
				let (dep_dep,dep_list,optimizable)=extract_dependency_path 
								dependencies 	
								(extract_var (ASTD_transition.get_params (ASTD_arrow.get_transition arrow))) 
								dependencies
								arrow
								astd
								path
								var
				in if (optimizable)
				then  begin debug ("find one opt /optimizable "^(ASTD_transition.get_label (ASTD_arrow.get_transition arrow)));
					([((ASTD_transition.get_label (ASTD_arrow.get_transition arrow)),path,var,dep_dep)],dep_list) end
				else begin debug ("find one opt /non optimizable "^(ASTD_transition.get_label (ASTD_arrow.get_transition arrow)));
					([],[]) end
				
				end

let rec filter_opt opt_dep_list opt dep = match opt_dep_list with
	|(sub_opt,dep_opt)::tail-> begin filter_opt tail (union sub_opt opt) (union dep_opt dep) end
	|[]->(opt,dep)


let rec retrieve_optimisations astd dependencies opt_list dep_list non_kappa opt dep = match non_kappa with
	|(arrow,path,missing_var)::tail-> begin debug ("retrieve optimisations for "^(ASTD_transition.get_label (ASTD_arrow.get_transition arrow))^" missing : "^(string_of_int (List.length missing_var)));
					let find=(List.map (find_one_opt arrow path astd dependencies opt_list) missing_var)
					in let (new_opt,new_dep)=filter_opt find opt dep
					in begin debug ("total opt :"^(string_of_int(List.length new_opt))^"/ but new :"^(string_of_int(List.length find))^"/ vs old :"^(string_of_int(List.length opt)));
                                           	retrieve_optimisations astd dependencies opt_list dep_list tail new_opt new_dep
						end
					end
							
	|[]->(opt,dep)



let rec static_analysis astd = 
	let non_kappa = extract_non_kappa_direct [] [] [] [] astd
	in let opt_list = ref []
	and dep_list = ref []
	and temp = ref []
	and wait = ref true
	in begin
		while (!wait)||((not !wait)&&( !opt_list != !temp))
		do 
		debug ("%%%%%%%%%%%%%%%%static loop start length: "^(string_of_int(List.length !opt_list)));
		temp:= !opt_list;
		wait:=false;
		let df=(extract_df [] true [] [] astd)
		in begin debug ("DF extracted : "^(string_of_int (List.length df))^"/Non Kappa extracted : "^(string_of_int (List.length non_kappa)));
			let dependencies = (update_producers_cons astd (df) (!opt_list))
			in let (opt,dep)= retrieve_optimisations astd dependencies !opt_list !dep_list non_kappa [] []
			in begin debug ("static loop restart "^(string_of_int(List.length (!opt_list))));
				debug ("static loop new "^(string_of_int(List.length (opt))));
				debug ("static loop end "^(string_of_int(List.length (union opt !opt_list))));
				opt_list:= union opt !opt_list;
				dep_list:= union dep !dep_list
				end
			end
		done;
		debug ("end_static: "^(string_of_int (List.length !opt_list))^" opt found");
		((!opt_list),(!dep_list))
		
		end

(**split                                      *)



let rec register_opt path opt astd = 
	if (List.hd path)!=(ASTD_astd.get_name astd)
	then failwith "mistake of matching in register opt"
	else let sub_path = List.tl path
		in if sub_path=[]
			then failwith "register_opt end of path"
			else match astd with
				|ASTD_astd.Automata (name,sub_astd,arrows,s_final,d_final,init) ->
					let sub=register_opt sub_path opt (ASTD_astd.find_subastd (List.hd(sub_path)) sub_astd) 
					in ASTD_astd.Automata (name,
						ASTD_astd.replace_sub_astd sub (List.hd(sub_path)) sub_astd,
						arrows, 
						s_final, 
						d_final, 
						init)

				|ASTD_astd.Sequence (name,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = (List.hd sub_path)
						then ASTD_astd.Sequence (name,register_opt sub_path opt left ,right)
						else ASTD_astd.Sequence (name,left,register_opt sub_path opt right)

				|ASTD_astd.Choice (name,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = (List.hd sub_path)
						then ASTD_astd.Choice (name,register_opt sub_path opt left,right)
						else ASTD_astd.Choice (name,left,register_opt sub_path opt right)

				|ASTD_astd.Kleene (name,sub) ->
					ASTD_astd.Kleene (name,register_opt sub_path opt sub)
    
				|ASTD_astd.Synchronisation (name,delta,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = (List.hd sub_path)
						then ASTD_astd.Synchronisation (name,
										delta,
										register_opt sub_path opt left,
										right)
						else ASTD_astd.Synchronisation (name,
										delta,
										left,
										register_opt sub_path opt right)

				|ASTD_astd.Guard (name,pred,sub) ->
					ASTD_astd.Guard (name,pred,register_opt sub_path opt sub)

				|ASTD_astd.QChoice (name,var,val_list,opt_list,sub) ->
					ASTD_astd.QChoice (name,
								var,
								val_list,
								opt_list,
								register_opt sub_path opt sub)

				|ASTD_astd.QSynchronisation (name,var,val_list,delta,opt_list,sub )->
					let (_,_,variable,_)=opt
					in if var=variable
						then begin debug ("opt registered in "^name);
							ASTD_astd.QSynchronisation (name,
										var,
										val_list,
										delta,
										opt::opt_list,
										sub)
							end
						else ASTD_astd.QSynchronisation (name,
										var,
										val_list,
										delta,
										opt_list,
										register_opt sub_path opt sub)

				|ASTD_astd.Call (name,called_name,vect) ->
					let sub_astd=register_opt sub_path opt (ASTD_astd.get_astd called_name)
					in begin let (astd,dom_var_link)= ASTD_astd.get_call_astd called_name
						in begin 
							ASTD_astd.global_save_astd sub_astd dom_var_link;
							ASTD_astd.Call (name,called_name,vect)
							end
						end

				|ASTD_astd.Elem (a) ->failwith "register opt: impossible to go deeper than elem"






let rec register_dep path dep astd = 
	if (List.hd path)!=(ASTD_astd.get_name astd)
	then failwith "mistake of matching in register dep"
	else let sub_path = List.tl path
		in if sub_path=[]
			then if ASTD_astd.is_qchoice astd
				then let (name,var,val_list,dep_list,sub)=ASTD_astd.get_data_qchoice astd
					in let (_,_,variable,_,_)=dep
					in if var!=variable
						then failwith "error in dep path"
						else begin debug ("dep registered in "^name);
							ASTD_astd.QChoice (name,
									var,
									val_list,
									dep::dep_list,
									sub)
							end

				else failwith "register dep end of path"
			else match astd with
				|ASTD_astd.Automata (name,sub_astd,arrows,s_final,d_final,init) ->
					let sub=register_dep sub_path dep (ASTD_astd.find_subastd (List.hd(sub_path)) sub_astd) 
					in ASTD_astd.Automata (name,
						ASTD_astd.replace_sub_astd sub (List.hd(sub_path)) sub_astd,
						arrows, 
						s_final, 
						d_final, 
						init)

				|ASTD_astd.Sequence (name,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = (List.hd sub_path)
						then ASTD_astd.Sequence (name,register_dep sub_path dep left ,right)
						else ASTD_astd.Sequence (name,left,register_dep sub_path dep right)

				|ASTD_astd.Choice (name,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = (List.hd sub_path)
						then ASTD_astd.Choice (name,register_dep sub_path dep left,right)
						else ASTD_astd.Choice (name,left,register_dep sub_path dep right)

				|ASTD_astd.Kleene (name,sub) ->
					ASTD_astd.Kleene (name,register_dep sub_path dep sub)
    
				|ASTD_astd.Synchronisation (name,delta,left,right) ->
					let left_name= ASTD_astd.get_name left
					in if left_name = (List.hd sub_path)
						then ASTD_astd.Synchronisation (name,
										delta,
										register_dep sub_path dep left,
										right)
						else ASTD_astd.Synchronisation (name,
										delta,
										left,
										register_dep sub_path dep right)

				|ASTD_astd.Guard (name,pred,sub) ->
					ASTD_astd.Guard (name,pred,register_dep sub_path dep sub)

				|ASTD_astd.QChoice (name,var,val_list,dep_list,sub) ->
					let (_,_,variable,_,_)=dep
					in if var!=variable
						then begin
							ASTD_astd.QChoice (name,
									var,
									val_list,
									dep_list,
									register_dep sub_path dep sub)
							end
						else begin debug ("dep registered in "^name);
							ASTD_astd.QChoice (name,
									var,
									val_list,
									dep::dep_list,
									sub)
							end

				|ASTD_astd.QSynchronisation (name,var,val_list,delta,opt,sub )->
					ASTD_astd.QSynchronisation (name,
									var,
									val_list,
									delta,
									opt,
									register_dep sub_path dep sub)

				|ASTD_astd.Call (name,called_name,vect) ->
					let sub_astd=register_dep sub_path dep (ASTD_astd.get_astd called_name)
					in let (_,var_dom_link)=ASTD_astd.get_call_astd called_name
					in begin ASTD_astd.global_save_astd sub_astd var_dom_link;
						ASTD_astd.Call (name,called_name,vect)
						end

				|ASTD_astd.Elem (a) ->failwith "register dep: impossible to go deeper than elem"





let rec register_in_user opt_list astd = match opt_list with
	|opt::tail ->let (_,path,_,_)=opt in register_in_user tail (register_opt path opt astd)
	|[]->astd

let rec register_in_creator dep_list astd = match dep_list with
	|dep::tail -> let (path,_,_,_,_)=dep in register_in_creator tail (register_dep path dep astd)
	|[]->astd

let rec register_static opt_list dep_list astd = 
	let new_astd = register_in_user opt_list astd
	in register_in_creator dep_list new_astd




