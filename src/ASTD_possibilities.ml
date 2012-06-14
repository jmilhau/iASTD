type t = Possibility of (ASTD_state.t * ASTD_arrow.t) | Mult of t list | Synch of (ASTD_term.t*t) list
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

let rec synch_to_mult a = match a with
 |Synch(a::t)->let b=(get_mult_data(synch_to_mult (Synch(t))  )) in Mult((Synch([a]))::b)
 |Synch([])->Mult([])
 |_->failwith "not appropriate use of synch_to_mult"
;;


let rec possible l = match l with
  |Mult([])->false
  |Mult(a::q)-> (possible a) || (possible (Mult q))
  |Possibility(a,b)->true
  |Synch([])->false
  |Synch((v,a)::q)->(possible a) || (possible (Synch q))
;;



let rec never_empty l = match l with
  |Synch((v,a)::q)->(possible a) && (never_empty (Synch q))
  |Synch([])->true
  |_-> failwith "should be called with synch"
;;




let rec clear_cons_poss a b = match (a) with
 |(Mult ((Mult [])::t) ) -> clear_cons_poss (Mult(t)) b
 |(Mult (h::t))-> if (find_m h b) then (clear_cons_poss (Mult t) b)
                                  else if possible h then cons (Mult([h])) (clear_cons_poss (Mult t) b)
                                                     else (clear_cons_poss (Mult t) b)
 |(Mult ([]))-> b
 |_ -> cons a b
;;

let clear_cons a b = if possible b then clear_cons_poss a b
                                   else a

;;

let rec print a astd s= match a with
  |Possibility(a,b)-> print_endline (s^"POSSIBILITY");(ASTD_state.print a astd s);print_endline(s^"END OF POSSIBILITY")
  |Mult([])->print_endline (s^"end of mult")
  |Synch([])->print_endline (s^"end of synch")
  |Mult(a::q)->print_endline(s^"Mult");(print a astd (s^"  "));(print (Mult q) astd s)
  |Synch((b,c)::q)->print_endline(s^"Synch");(print c astd (s^"  "));(print (Synch q) astd s)








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
    | Synch ((a,b)::q) -> clear_cons (Synch [(a,complete_synch_side side b)]) (complete_synch_side side (Synch q))
    | Mult[]->Mult[]
    | Synch[]->Synch[]
    | Possibility(state,arrow) -> if side then Possibility((ASTD_state.synchronisation_s_of ASTD_state.NotDefined state) ,arrow)
                               else Possibility((ASTD_state.synchronisation_s_of state ASTD_state.NotDefined) ,arrow)
;;







let rec complete_synch_poss seq = match seq with
   |Synch [(v1,Mult(h::t));(v2,c)] -> let mem = get_synch_data (complete_synch_poss (Synch [(v1,Mult t);(v2,c)])) 
                            in let (v,l1)=(List.hd mem) and (vbis,l2)=(List.hd (List.tl mem))
                            in let (a) = (get_mult_data (l1))
                               and (b) = l2
                                   in Synch [(v1,Mult((complete_synch_side false h)::a));(v2,b)]
   |Synch [(v1,Mult[]);(v2,Mult(h::t))] -> let mem = get_synch_data (complete_synch_poss (Synch ([(v1,Mult []);(v2,Mult t)]))) 
                                 in let (v,l1)=(List.hd mem) and (vbis,l2)=(List.hd (List.tl mem))
                                 in let a = (l1)
                                    and b = get_mult_data(l2)
                                                  in Synch [(v1,a);(v2,Mult((complete_synch_side true h)::b))]
   |Synch [(v1,Mult []); (v2,Mult [])] -> Synch [(v1,Mult []);(v2, Mult [])]
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
  |Synch ((v,seq)::q) -> (clear_cons (Synch([(v,complete_possibilities state seq)])) (complete_possibilities state (Synch q)))
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




  


let rec possible_evolutions astd state event environment = match state with

  |ASTD_state.Automata_s (a,b,c) -> 
                         begin 
                         let (a2,b2,c2,d2,e2)=ASTD_astd.get_data_automata astd
                         in let l1=(ASTD_arrow.get a event false)
                         in begin 
                          let astd2 = (ASTD_astd.find_subastd a b2)     
                         in let (l2,final)= possible_evolutions astd2 c event environment
                         in begin 
                            let list_poss1 =complete_possibilities
                                                        (ASTD_state.automata_s_of a b ASTD_state.NotDefined ) 
                                                        (create_possibilities environment event l1)
                         in let list_poss2 = complete_possibilities state l2 
                         in let list_poss=(clear_cons list_poss1 list_poss2)
                         in let is_f =(ASTD_astd.is_astd_final_in_automata astd a)
                         in begin 
                                let boolean=(final||((ASTD_astd.is_elem astd2)&&(is_f)))
                                in if (boolean)
                                          then  begin
                                                let l3=(ASTD_arrow.get a event true)
                                                in 
                                                let list_poss3 = complete_possibilities
                                                                         (ASTD_state.automata_s_of a b ASTD_state.NotDefined ) 
                                                                         (create_possibilities environment event l3)
                                                in let list_poss_fin=clear_cons list_poss list_poss3
                                                       in (Mult[list_poss_fin], true) 
                                                end
                                          else  begin
                                                        (Mult[list_poss],false) 
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
                                           in (Mult[list_poss],finalbis)
                                           end
                                      else  (Mult[list_poss1],false)
                           else let (l1,final_right)=possible_evolutions right state2 event environment
                                in let list_poss1=complete_possibilities state l1
                                in (Mult[list_poss1],final_right)



  |ASTD_state.Choice_s (side,state2) -> 
          let (name,first,second)=ASTD_astd.get_data_choice astd
          in begin 
             if side = ASTD_state.Fst
                then let (l1,final_fst)=possible_evolutions first state2 event environment
                     in let list_poss1=complete_possibilities state l1
                           in (Mult[list_poss1],final_fst)
                   else  begin 
                         if side = ASTD_state.Snd
                           then  let (l1,final_snd)=possible_evolutions second state2 event environment
                                 in begin 
                                    let list_poss1=complete_possibilities state l1
                                    in (Mult[list_poss1],final_snd)
                                    end
                           else 
                             let (l1,final_fst)=possible_evolutions first (ASTD_state.init first) event environment
                             in let (l2,final_snd)=possible_evolutions second (ASTD_state.init second) event environment
                             in let list_poss1=complete_possibilities (ASTD_state.choice_s_of ASTD_state.Fst (ASTD_state.NotDefined)) l1
                             and list_poss2=complete_possibilities (ASTD_state.choice_s_of ASTD_state.Snd (ASTD_state.NotDefined)) l2
                             in begin (Mult[clear_cons list_poss1 list_poss2],(final_fst || final_snd)) end
                                
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
                                          in begin
                                              ((Mult[clear_cons list_poss1 list_poss2]),true)
                                             end
                                          end
                                     else begin
                                          (Mult[list_poss1],false)
                                          end
                        end

  |ASTD_state.Synchronisation_s (state1,state2) -> 
                      let (name,transition_list,astd1,astd2)=ASTD_astd.get_data_synchronisation astd
                      in let (la,fa)= possible_evolutions astd1 state1 event environment
                         and (lb,fb)= possible_evolutions astd2 state2 event environment
                      in if ASTD_transition.is_included (ASTD_event.get_label event) transition_list
                               then if (possible la) && (possible lb)
                                        then (Mult[Synch [((ASTD_term.Const(ASTD_constant.Integer 1)),Mult[la]);
                                                          ((ASTD_term.Const(ASTD_constant.Integer 2)),Mult[lb])]],
                                              fa && fb)
                                        else (Mult([]),fa && fb)  
                               else (clear_cons (Mult[Synch [((ASTD_term.Const(ASTD_constant.Integer 1)),Mult[la])]])
                                                (Mult[Synch [((ASTD_term.Const(ASTD_constant.Integer 2)),Mult[lb])]]),
                                              fa && fb)
                      

  |ASTD_state.Guard_s (started,state2) -> 
                let (name,pred_list,astd2)=ASTD_astd.get_data_guard astd
                in begin   
                if started 
                      then let (l,f)=possible_evolutions astd2 state2 event environment 
                           in let list_poss=complete_possibilities state l
                           in (Mult[list_poss],f)
                      else let boolean= (ASTD_arrow.evaluate_guard environment pred_list)
                           in if boolean  
                              then begin 
                                   let (l,f)=possible_evolutions astd2 state2 event environment 
                                   in begin 
                                       let list_poss=complete_possibilities (ASTD_state.guard_s_of true state2) l   
                                        in (Mult[list_poss],f) 
                                      end 
                                   end
                              else begin 
                                   (Mult[],false) 
                                   end
                    end





  |ASTD_state.QChoice_s (val_used,state2) -> 

              let (name,var,list_val,astd2)=ASTD_astd.get_data_qchoice astd
                in begin 
                      if val_used=ASTD_state.ChoiceNotMade
                      then begin 
                           let (label,c_list)=ASTD_event.get_data event
                           in let params=ASTD_arrow.get_transition_params name label
                           in if params=[]
                              then begin 
                                   let (list_poss,f)= kappa_direct_q_poss_c astd2 event [] [] var list_val environment 
                                   in (Mult[list_poss],f)
                                   end
                              else begin 
                                   let (list_poss,f)= kappa_direct_q_poss_c astd2 event params c_list var list_val environment 
                                   in begin (Mult[list_poss],f) end
                                   end
                            end
                       else begin 
                            let bind_env = ASTD_environment.bind var (ASTD_state.get_val val_used)
                            in let (l,f)=
                                     possible_evolutions astd2 state2 event (ASTD_environment.add_binding bind_env environment)
                            in let list_poss = complete_possibilities state l
                                   in (Mult[list_poss],f)
                       end
                    end 
             



  |ASTD_state.QSynchronisation_s (trans,fin_dom,not_init_dom,init) -> 
    let (name,var,val_list,trans_list,astd2,prod,users,cons)=ASTD_astd.get_data_qsynchronisation astd
    in begin 
              let (label,c_list)=ASTD_event.get_data event 
              in let params=ASTD_arrow.get_transition_params name label
              in if params=[] 
                     then let l2= kappa_direct_q_poss_s 
                                astd2 event trans_list label [] [] var val_list environment trans name not_init_dom init
                          in (Mult[l2],ASTD_constant.is_empty_dom fin_dom)
                     else let l2= kappa_direct_q_poss_s 
                                astd2 event trans_list label params c_list var val_list environment trans name not_init_dom init
                          in (Mult[l2],ASTD_constant.is_empty_dom fin_dom)
           end


  |ASTD_state.Call_s (called,state2) -> 
               let (name,called_name,fct_vec)=ASTD_astd.get_data_call astd
               in let astd2=(ASTD_astd.get_astd called_name)
               in if called 
                     then begin 
                          let (l,final)=possible_evolutions astd2
                                                            state2 
                                                            event 
                                                            (ASTD_environment.increase_call environment fct_vec)
                          in let list_poss=complete_possibilities (ASTD_state.call_s_of called state2) l
                          in (Mult[list_poss],final)
                          end
                     else begin 
                          let (l,final)=possible_evolutions astd2
                                                            (ASTD_state.init astd2) 
                                                            event 
                                                            (ASTD_environment.increase_call environment fct_vec)
                          in let list_poss=complete_possibilities (ASTD_state.call_s_of true state2) l
                          in (Mult[list_poss],false)
                          end

  |_ -> (Mult[],false)

and q_poss_c astd event var list_val environment = 
            if ASTD_constant.is_empty_dom list_val 
            then (Mult[])
            else begin
                      let (x,y)=ASTD_constant.head_tail list_val
                      in let bind_env=ASTD_environment.bind var (ASTD_term.Const x)
                      in let a = q_poss_c astd event var y environment
                      in let (c,d)=possible_evolutions astd 
                                                       (ASTD_state.init astd) 
                                                       event 
                                                       (ASTD_environment.add_binding bind_env environment)
          in (  clear_cons (complete_possibilities 
                                   (ASTD_state.qchoice_s_of (ASTD_state.Val(ASTD_term.Const x)) ASTD_state.NotDefined) 
                                   c) 
                           a)
                 end



and kappa_direct_q_poss_c astd event params c_list var list_val environment = match (params,c_list) with
         |((a::b)::x,h::t)-> begin 
                        if a=(ASTD_term.Var var)
                        then
                           if (ASTD_constant.is_included h list_val)
                             then begin 
                               let bind_env=ASTD_environment.bind var (ASTD_term.Const h)
                               in let (c,d)=possible_evolutions astd 
                                                               (ASTD_state.init astd) 
                                                               event 
                                                               (ASTD_environment.add_binding bind_env environment)
                               in let list_poss=(ASTD_state.qchoice_s_of 
                                                     (ASTD_state.Val(ASTD_term.Const h)) 
                                                     ASTD_state.NotDefined)
                               in (Mult[complete_possibilities list_poss c],d)
                                  end
                             else begin 
                                       (ASTD_constant.print h);
                                   (Mult[],false)
                                  end
                        else kappa_direct_q_poss_c astd event (b::x) t var list_val environment
          
                        end
         |([]::x,[])->begin
                      if x=[]
                      then kappa_direct_q_poss_c astd event x ([]) var list_val environment
                      else kappa_direct_q_poss_c astd event x (ASTD_event.get_const event) var list_val environment
                      end
         |([],[])-> begin 
         let bind_env = ASTD_environment.bind var (ASTD_term.Const(ASTD_constant.FreeConst))
         in let (l,f)=ASTD_state.evaluate_arrows astd (ASTD_state.init astd) (ASTD_environment.add_binding bind_env environment)
         in if (ASTD_transition.is_included (ASTD_event.get_label event) l)
               then (q_poss_c astd event var list_val environment,f)
               else (Mult[],f)
                     end
         |_->failwith "kappa_c, the event has an incorrect number of parameters" 


and kappa_direct_q_poss_s astd2 event trans_list label params c_list var list_val environment trans name not_init_dom init= 
    match (params,c_list) with
         |((a::b)::x,h::t)-> begin 
                        if a=(ASTD_term.Var var)
                        then if (ASTD_transition.is_included label trans_list)
                                then begin 
                                        (Mult[])
                                     end
                                else if (ASTD_constant.is_included h list_val)
                                        then begin 
                                             let state=(ASTD_state.get_synch_state not_init_dom init name (h))
                                             in begin
                                             let bind_env=ASTD_environment.bind var (ASTD_term.Const h)
                                             in let (c,d)=possible_evolutions astd2 
                                                                            state
                                                                            event 
                                                                            (ASTD_environment.add_binding bind_env environment)
                                             in if (possible c) then begin
                                                                           (Synch[((ASTD_term.Const h),c)]) end
                                                                else begin 
                                                                           (Synch[((ASTD_term.Const h),c)]) end
                                                end
                                             end 
                                        else begin 
                                               (Mult[])
                                             end
                        else begin 
               kappa_direct_q_poss_s astd2 event trans_list label (b::x) t var list_val environment trans name not_init_dom init
                        end
                        end
         |([]::x,[])-> 
                   if x=[]
                   then kappa_direct_q_poss_s astd2 event trans_list label (x) ([]) var list_val environment trans name not_init_dom init
                   else kappa_direct_q_poss_s astd2 event trans_list label (x) (ASTD_event.get_const event) var list_val environment trans name not_init_dom init

         |([],[])->begin 
                       let a= ASTD_state.get_val_arrow trans event
                       in if (ASTD_transition.is_included label trans_list) 
                          then begin 
                               if a=list_val then begin 
                                                   let poss= q_poss_s astd2 event var a environment name not_init_dom init
                                                   in if(never_empty poss) then poss
                                                                           else (Mult[])
                                                  end
                                             else (Mult[])
                               end
                          else begin 
                               let x= (q_poss_s astd2 event var a environment name not_init_dom init)
                               in (synch_to_mult x)
                               end
                   end 
         |_->failwith "kappa_s, the event has an incorrect number of parameters" 



and  q_poss_s astd event var list_val environment name not_init_dom init=
           if ASTD_constant.is_empty_dom list_val 
                    then (Synch [])
                    else begin
                               let (c,d)=ASTD_constant.head_tail list_val 
                               in begin 
                                  let bind_env =ASTD_environment.bind var (ASTD_term.Const(c))
                                  in let (l2,f2)=possible_evolutions astd 
                                                                     (ASTD_state.get_synch_state not_init_dom init name c)
                                                                     event
                                                                     (ASTD_environment.add_binding bind_env environment)
                                     and (list_poss)= q_poss_s astd event var d environment name not_init_dom init
                                  in (clear_cons (Synch [((ASTD_term.Const(c)),Mult[l2])]) (list_poss))
                                  end
                         end

;;
 


