type t = Possibility of (ASTD_state.t * ASTD_arrow.t) | Mult of t list | Synch of t list
;;


let is_mult a = match a with
|Mult b -> true
|_ -> false
;;


let get_poss_data a = match a with
 |Possibility (a,b) -> (a,b)
 |_->failwith "get poss data not appropriate"
;;


let get_state_data a = match a with
 |Possibility (a,b) -> a
 |_->failwith "get state data not appropriate"
;;


let get_mult_data a = match a with
 |Mult (a) -> a
 |_->failwith "get mult date not appropriate"
;;



let get_synch_data a = match a with
 |Synch (a) -> a
 |_->failwith "get synch data not appropriate"
;;


let rec cons_mult a b = match (a,b) with
|(Mult(h::q),Mult s) -> let c = get_mult_data (cons_mult (Mult q) (Mult s)) 
                          in (Mult (h::c))
|(Mult([]),Mult s) -> Mult s
|_-> failwith "cons mult not appropriate"
;;


let is_synch a = match a with
|Synch b -> true
|_ -> false
;;


let rec cons_synch a b = match (a,b) with
|(Synch(h::q),Synch s) -> let c = get_synch_data (cons_synch (Synch q) (Synch s)) 
                          in (Synch (h::c))
|(Synch([]),Synch s) -> b
|_-> failwith "cons synch not appropriate"
;;

let cons a b = match (a,b) with
 |(Mult c, Mult d) -> cons_mult a b
 |(Mult c, Synch d) -> cons_mult a (Mult[Synch d])
 |(Synch c, Mult d) -> cons_mult (Mult[Synch c]) b
 |(Synch c, Synch d) -> cons_synch a b
 |_-> failwith "cons not appropriate"
;;

let rec find_m a b = match b with
 |Mult (c::d)-> (a=c)||(find_m a (Mult d))
 |(Mult [])->false
 |_->failwith ("not mult for find")
;;



let rec clear_cons a b = match (a) with
 |(Mult (h::t))-> if (find_m h b) then (clear_cons (Mult t) b)
                                else cons (Mult([h])) (clear_cons (Mult t) b)
 |(Mult ([]))-> b
 |_ -> cons a b
;;





let rec create_possibilities environment event arrow_list = match arrow_list with
  |arrow::q -> if (ASTD_arrow.valid_arrow event environment arrow) 
                        then begin 
                             clear_cons (Mult([Possibility(ASTD_state.NotDefined,arrow)])) (create_possibilities environment event q)  
                             end
                        else begin  
                             (create_possibilities environment event q) 
                             end
             
  |[]->Mult[] 
;;





let rec complete_synch_side side seq = 
match seq with
    | Mult (a::q) -> clear_cons (Mult[complete_synch_side side a]) (complete_synch_side side (Mult q))
    | Synch (a::q) -> clear_cons (Synch [complete_synch_side side a]) (complete_synch_side side (Synch q))
    | Mult[]->Mult[]
    | Synch[]->Synch[]
    | Possibility(state,arrow) -> if side then Possibility((ASTD_state.synchronisation_s_of ASTD_state.NotDefined state) ,arrow)
                               else Possibility((ASTD_state.synchronisation_s_of state ASTD_state.NotDefined) ,arrow)
;;







let rec complete_synch_poss seq = match seq with
   |Synch [Mult(h::t);c] -> let mem = get_synch_data (complete_synch_poss (Synch [Mult t;c])) 
                            in let a = (get_mult_data (List.hd mem))
                               and b = List.hd (List.tl mem)
                                   in Synch [Mult((complete_synch_side false h)::a);b]
   |Synch [Mult[];Mult(h::t)] -> let mem = get_synch_data (complete_synch_poss (Synch ([Mult [];Mult t]))) 
                                 in let a = (List.hd mem)
                                    and b = get_mult_data(List.hd (List.tl mem))
                                                  in Synch [a;Mult((complete_synch_side true h)::b)]
   |Synch [Mult []; Mult []] -> Synch [Mult []; Mult []]
   |_-> failwith "complete_synch not appropriate"
;;




let complete_single_possibilities state seq = match state with
              |ASTD_state.Automata_s (state_name,h,_) -> let (old_state,arrow)=get_poss_data seq 
                                                                in Possibility((ASTD_state.automata_s_of state_name h old_state),arrow)
              |ASTD_state.Sequence_s (step,_) ->let (old_state,arrow)=get_poss_data seq 
                                                                in Possibility(ASTD_state.sequence_s_of step old_state,arrow)
              |ASTD_state.Choice_s (side,_) ->let (old_state,arrow)=get_poss_data seq 
                                                                in Possibility(ASTD_state.choice_s_of side old_state,arrow)
              |ASTD_state.Kleene_s (started,_) ->let (old_state,arrow)=get_poss_data seq 
                                                                in Possibility(ASTD_state.kleene_s_of started old_state,arrow)
              |ASTD_state.Guard_s (started,_) -> let (old_state,arrow)=get_poss_data seq 
                                                                in Possibility(ASTD_state.guard_s_of started old_state,arrow)
              |ASTD_state.QChoice_s (val_used,_) -> let (old_state,arrow)=get_poss_data seq 
                                                                in Possibility(ASTD_state.qchoice_s_of val_used old_state,arrow)
              |ASTD_state.Call_s (started,_) -> let (old_state,arrow)=get_poss_data seq 
                                                                in Possibility(ASTD_state.call_s_of started old_state,arrow)

              |_ -> failwith "complete single, not appropriate to use not_defined or elem"
;;



let rec complete_possibilities state sequence = 
begin
  match sequence with
  |Mult (seq::q) -> (clear_cons (Mult([complete_possibilities state seq])) (complete_possibilities state (Mult q )))
  |Mult[] -> Mult[]
  |Synch (seq::q) -> (clear_cons (Synch([complete_possibilities state seq])) (complete_possibilities state (Synch q)))
  |Synch[] -> Synch[]
  |a -> (complete_single_possibilities state a)
end
;;



let no_possibilities a= match a with
  |Mult ([])-> true
  |_->false
;;

let rec choice_is p_list number = match p_list with
      |a::q -> if number=0 then a else choice_is q (number-1)
      |[] -> failwith "list too short for a rand choice"
;;



let choose_next p_list = match p_list with
      |a::[] -> a
      |[] -> failwith "impossible event: no possibilities"
      | _ -> let l= List.length p_list in (choice_is p_list (Random.int l))
;;



let rec possible l = match l with
  |Mult([])->false
  |Mult(a::q)-> (possible a) || (possible (Mult q))
  |Possibility(a,b)->true
  |Synch([])->false
  |Synch(a::q)->(possible a) || (possible (Mult q))
;;



let rec never_empty l = match l with
  |Synch(a::q)->(possible a) && (never_empty (Mult q))
  |Synch([])->true
  |_-> failwith "should be called with synch"
;;
  
     


let rec possible_evolutions astd state event environment = match state with
  |ASTD_state.Automata_s (a,b,c) -> 

                         begin print_endline("Aut : Poss from "^a^" in astd "^(ASTD_astd.get_name astd)^" ?");
                         let (a2,b2,c2,d2,e2)=ASTD_astd.get_data_automata astd
                         in let l1=(ASTD_arrow.get a event false)
                         in begin 
                          let astd2 = (ASTD_astd.find_substate a b2)     
                         in let (l2,final)= possible_evolutions astd2 c event environment
                         in begin 
                            let list_poss1 =complete_possibilities
                                                        (ASTD_state.automata_s_of a b ASTD_state.NotDefined ) 
                                                        (create_possibilities environment event l1)
                         in let list_poss2 = complete_possibilities state l2 
                         in let list_poss=(clear_cons list_poss1 list_poss2)
                         in let is_f =(ASTD_astd.is_state_final_automata astd a)
                         in begin 
                                let boolean=(final||((ASTD_astd.isElem astd2)&&(is_f)))
                                in if (boolean)
                                          then  begin 
                                                let l3=(ASTD_arrow.get a event true)
                                                in 
                                                let list_poss3 = complete_possibilities
                                                                         (ASTD_state.automata_s_of a b ASTD_state.NotDefined ) 
                                                                         (create_possibilities environment event l3)
                                                in let list_poss_fin=clear_cons list_poss list_poss3
                                                       in (list_poss_fin, true) 
                                                end
                                          else  begin 
                                                if (ASTD_astd.isElem astd2)
                                                        then begin 
                                                             let l3=(ASTD_arrow.get a event true) 
                                                             in begin
                                                                let list_poss3=complete_possibilities
                                                                          (ASTD_state.automata_s_of a b ASTD_state.NotDefined ) 
                                                                          (create_possibilities environment event l3)
                                                                in begin 
                                                                        ((clear_cons list_poss list_poss3),false)
                                                                   end
                                                                end
                                                             end
                                                        else  (list_poss,false) 
                                               end
                             end     
                             end
                             end
                             end

  |ASTD_state.Sequence_s (step,state2) -> 
                     let (name,left,right)=ASTD_astd.get_data_sequence astd
                     in if step = ASTD_state.Left 
                        then let (l1,final_left) = possible_evolutions left state2 event environment
                             in let list_poss1=complete_possibilities state l1
                             in if final_left 
                                      then begin 
                                           let (l2,finalbis)= 
                                                     possible_evolutions right (ASTD_state.init right) event environment
                                           in let list_poss2=
                                                      complete_possibilities (ASTD_state.sequence_s_of ASTD_state.Right 
                                                                             (ASTD_state.init right)) l2
                                           in let list_poss=clear_cons list_poss1 list_poss2 
                                           in (list_poss,finalbis)
                                           end
                                      else  (list_poss1,false)
                           else let (l1,final_right)=possible_evolutions right state2 event environment
                                in let list_poss1=complete_possibilities state l1
                                in (list_poss1,final_right)



  |ASTD_state.Choice_s (side,state2) -> 
          let (name,first,second)=ASTD_astd.get_data_choice astd
          in begin 
             if side = ASTD_state.Fst
                then let (l1,final_fst)=possible_evolutions first state2 event environment
                     in let list_poss1=complete_possibilities state l1
                           in (list_poss1,final_fst)
                   else  begin 
                         if side = ASTD_state.Snd
                           then  let (l1,final_snd)=possible_evolutions second state2 event environment
                                 in begin 
                                    let list_poss1=complete_possibilities state l1
                                    in (list_poss1,final_snd)
                                    end
                           else let (l1,final_fst)=possible_evolutions first (ASTD_state.init first) event environment
                                and (l2,final_snd)=possible_evolutions second (ASTD_state.init second) event environment
                                in let list_poss1=complete_possibilities (ASTD_state.choice_s_of ASTD_state.Fst (ASTD_state.NotDefined)) l1
                                and list_poss2=complete_possibilities (ASTD_state.choice_s_of ASTD_state.Snd (ASTD_state.NotDefined)) l2
                                in (clear_cons list_poss1 list_poss2,(final_fst || final_snd))
                         end
             end


  |ASTD_state.Kleene_s (started,state2) -> 
                        begin let (name,astd2)=ASTD_astd.get_data_kleene astd
                               in let (l1,final)=possible_evolutions astd2 state2 event environment
                               in let list_poss1=complete_possibilities (ASTD_state.kleene_s_of true state2) l1
                               in let boolean= ((final)||(not(started))) 
                               in if boolean  
                                     then begin
                                          let (l2,f2) = possible_evolutions astd2 (ASTD_state.init astd2) event environment
                                          in let list_poss2 = complete_possibilities (ASTD_state.kleene_s_of true state2) l2
                                          in ((clear_cons list_poss1 list_poss2),true)
                                          end
                                     else begin
                                          (list_poss1,false)
                                          end
                        end

  |ASTD_state.Synchronisation_s (state1,state2) -> 
                      let (name,transition_list,astd1,astd2)=ASTD_astd.get_data_synchronisation astd
                      in let (la,fa)= possible_evolutions astd1 state1 event environment
                         and (lb,fb)= possible_evolutions astd2 state2 event environment
                      in if ASTD_transition.is_included (ASTD_event.get_label event) transition_list
                               then if (possible la) && (possible lb)
                                        then (Mult[Synch [Mult[la];Mult[lb]]],fa && fb)
                                        else (Mult([]),fa && fb)  
                               else (Synch [la;lb],fa && fb)
                      

  |ASTD_state.Guard_s (started,state2) -> 
                let (name,pred_list,astd2)=ASTD_astd.get_data_guard astd
                in let l=(ASTD_arrow.get name event false)
                in let list_poss1=create_possibilities environment event l
                in if started 
                      then let (l2,f2)=possible_evolutions astd2 state2 event environment 
                           in let list_poss2=complete_possibilities state l2
                           in if f2 
                                 then let l3=(ASTD_arrow.get name event true)
                                      in let list_poss3=create_possibilities environment event l3
                                      in (clear_cons (clear_cons list_poss1 list_poss2) list_poss3,true)
                                 else (clear_cons list_poss1 list_poss2,false)
                      else if (ASTD_arrow.evaluate_guard environment pred_list) 
                              then let (l2,f2)=possible_evolutions astd2 state2 event environment 
                                   in let list_poss2=complete_possibilities (ASTD_state.guard_s_of true state2) l2
                                   in if f2 
                                         then let l3=(ASTD_arrow.get name event true)
                                              in let list_poss3=create_possibilities environment event l3
                                                 in (clear_cons (clear_cons list_poss1 list_poss2) list_poss3,true)
                                         else (clear_cons list_poss1 list_poss2,false)
                              else (list_poss1,false)






  |ASTD_state.QChoice_s (val_used,state2) ->
                let (name,var,list_val,astd2)=ASTD_astd.get_data_qchoice astd
                in if val_used=ASTD_state.ChoiceNotMade
                      then let (list_poss,f)= q_poss astd2 event var [] list_val environment true
                           in (list_poss,f)
                       else let bind_env = ASTD_environment.bind var (ASTD_state.get_val val_used)
                            in let (l,f)=
                                     possible_evolutions astd2 state2 event (ASTD_environment.add_binding bind_env environment)
                            in let list_poss = complete_possibilities state l
                                   in (list_poss,f)



  |ASTD_state.QSynchronisation_s (state_list) -> 
                let (name,var,val_list,trans_list,astd2)=ASTD_astd.get_data_qsynchronisation astd
                in let (l2,f2)= q_poss astd2 event var state_list val_list environment false
                in if (ASTD_transition.is_included (ASTD_event.get_label event) trans_list)
                   then if (never_empty l2)
                           then let list_poss= l2
                                in (Mult[list_poss],f2)
                           else (Mult[],f2)
                   else let list_poss = l2
                        in (Mult[list_poss],f2)



  |ASTD_state.Call_s (called,state2) -> 
               let (name,called_name,fct_vec)=ASTD_astd.get_data_call astd
               in let astd2=(ASTD_astd.get_astd called_name)
               in let l=(ASTD_arrow.get name event false)
               in let list_poss1=create_possibilities environment event l
               in if called 
                     then let (l2,final)=possible_evolutions astd2
                                                            state2 
                                                            event 
                                                            (ASTD_environment.increase_call environment fct_vec)
                          in let list_poss2=complete_possibilities (ASTD_state.call_s_of called state2) l2
                          in if final then let l3=(ASTD_arrow.get name event false)
                                           in let list_poss3=create_possibilities environment event l3
                                           in let list_poss = clear_cons (clear_cons list_poss1 list_poss2) list_poss3
                                              in (list_poss,true)
                                      else (clear_cons list_poss1 list_poss2,false)
                     else let (l2,final)=possible_evolutions astd2
                                                            (ASTD_state.init astd2) 
                                                            event 
                                                            (ASTD_environment.increase_call environment fct_vec)
                          in let list_poss2=complete_possibilities (ASTD_state.call_s_of true state2) l2
                          in if final then let l3=ASTD_arrow.get name event false
                                           in let list_poss3=create_possibilities environment event l3
                                           in let list_poss=clear_cons (clear_cons list_poss1 list_poss2) list_poss3
                                              in (list_poss,final)
                                  else (clear_cons list_poss1 list_poss2,false)

  |_ -> (Mult[],false)

and q_poss astd event var state_list list_val environment kind = 
    if kind 
       then match list_val with
            | h::t -> let bind_env=ASTD_environment.bind var h
                      in let (a,b)= q_poss astd event var state_list t environment true
                         and (c,d)=possible_evolutions astd (ASTD_state.init astd) event (ASTD_environment.add_binding bind_env environment)
                      in (clear_cons (complete_possibilities (ASTD_state.qchoice_s_of (ASTD_state.Val h) ASTD_state.NotDefined) c) a,b||d) 
            | []-> (Mult[],false) 


       else match (state_list,list_val) with
            |((v,state)::b,(c::d))-> 
                     if v=c then begin 
                               let bind_env =ASTD_environment.bind var c 
                               in let (l2,f2)=(possible_evolutions astd state event (ASTD_environment.add_binding bind_env environment))
                                  and (list_poss,final)= q_poss astd event var b d environment false
                               in (clear_cons (Synch [Mult[l2]]) (list_poss),f2 && final)
                                 end
                            else failwith "mistake"
            |([],[])-> (Synch [],true)
            |_-> failwith "mistake"

;;
 

let rec print a s= match a with
  |Possibility(a,b)-> print_endline (s^"POSSIBILITY");(ASTD_state.print a s);print_endline(s^"END OF POSSIBILITY")
  |Mult([])->print_endline (s^"end of mult")
  |Synch([])->print_endline (s^"end of synch")
  |Mult(a::q)->print_endline(s^"Mult");(print a (s^"  "));(print (Mult q) s)
  |Synch(a::q)->print_endline(s^"Synch");(print a (s^"  "));(print (Synch q) s)




