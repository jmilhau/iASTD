type path = (ASTD_term.t) list

type chosen_possibility =ASTD_state.t
type state_to_modify=ASTD_state.t
type modified_state=ASTD_state.t

let rec string_of_path a = match a with
  |h::t-> (ASTD_term.string_of h)^(string_of_path t)
  |[]->""
;;


let rec apply_local astd arrow state2 state pos env = 
    let from=ASTD_arrow.get_from arrow
    in match (state,state2) with

        |(ASTD_state.Automata_s(_,h,s),ASTD_state.Automata_s(name_next,h_new,next_state)) -> 
            begin 
              if from=name_next
                 then let dest=ASTD_arrow.get_to arrow 
                          in begin 
                                   if (ASTD_state.is_automata s) 
                                         then let h2 = ASTD_state.modify_h h from s
                                                    in
                                                    ASTD_state.automata_s_of dest
                                                    h2
                                                    (ASTD_state.init (ASTD_astd.find_subastd dest (ASTD_astd.get_sub astd)))
                                         else       ASTD_state.automata_s_of dest
                                                    h
                                                    (ASTD_state.init (ASTD_astd.find_subastd dest (ASTD_astd.get_sub astd)))
                             end
                 else ASTD_state.automata_s_of name_next
                                              h
                          (apply_local (ASTD_astd.find_subastd name_next (ASTD_astd.get_sub astd)) arrow next_state s pos env)
            end                           

        |(ASTD_state.Sequence_s (ASTD_state.Left,s),ASTD_state.Sequence_s (ASTD_state.Right,next_state))->
                       let astd2=(ASTD_astd.get_seq_r astd)
              in ASTD_state.sequence_s_of ASTD_state.Right (apply_local astd2 arrow next_state (ASTD_state.init astd2) pos env)

        |(ASTD_state.Sequence_s (_,s),ASTD_state.Sequence_s (ASTD_state.Right,next_state))-> 
                   ASTD_state.sequence_s_of ASTD_state.Right (apply_local (ASTD_astd.get_seq_r astd) arrow next_state s pos env)

        
        |(ASTD_state.Sequence_s (_,s),ASTD_state.Sequence_s (ASTD_state.Left,next_state))-> 
                       
                   ASTD_state.sequence_s_of ASTD_state.Left (apply_local (ASTD_astd.get_seq_l astd) arrow next_state s pos env)
        


        |(ASTD_state.Choice_s (ASTD_state.Undef,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) ->
                       let astd2= (ASTD_astd.get_choice1 astd)
                  in ASTD_state.choice_s_of ASTD_state.Fst (apply_local astd2 arrow next_state (ASTD_state.init astd2) pos env)

        |(ASTD_state.Choice_s (ASTD_state.Snd,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) ->
                       let astd2= (ASTD_astd.get_choice1 astd)
                  in ASTD_state.choice_s_of ASTD_state.Fst (apply_local astd2 arrow next_state (ASTD_state.init astd2) pos env)


        |(ASTD_state.Choice_s (ASTD_state.Fst,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) -> 
                     ASTD_state.choice_s_of ASTD_state.Fst (apply_local (ASTD_astd.get_choice1 astd) arrow next_state s pos env)

        |(ASTD_state.Choice_s (ASTD_state.Fst,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) ->
                       let astd2= (ASTD_astd.get_choice2 astd);
                   in ASTD_state.choice_s_of ASTD_state.Snd (apply_local astd2 arrow next_state (ASTD_state.init astd2) pos env)



        |(ASTD_state.Choice_s (ASTD_state.Undef,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) ->
                       let astd2= (ASTD_astd.get_choice2 astd);
                   in ASTD_state.choice_s_of ASTD_state.Snd (apply_local astd2 arrow next_state (ASTD_state.init astd2) pos env)


        |(ASTD_state.Choice_s (ASTD_state.Snd,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) -> 
                     ASTD_state.choice_s_of ASTD_state.Snd (apply_local (ASTD_astd.get_choice2 astd) arrow next_state s pos env)

        |(_,ASTD_state.Choice_s (ASTD_state.Undef,next_state)) -> failwith "impossible choice state in execution"



        |(ASTD_state.Kleene_s (_,s),ASTD_state.Kleene_s (true,next_state)) ->  
                       ASTD_state.kleene_s_of true (apply_local (ASTD_astd.get_astd_kleene astd) arrow next_state s pos env)

        |(_,ASTD_state.Kleene_s (false,next_state)) -> failwith "impossible kleene state in execution"

        |(ASTD_state.Synchronisation_s (s1,s2),a) ->  
            begin 
                  if (List.hd pos)=(ASTD_term.Const(ASTD_constant.Integer 1))
                     then begin 
                                ASTD_state.synchronisation_s_of 
                                           (apply_local (ASTD_astd.get_synchro_astd1 astd) arrow a s1 (List.tl pos) env)
                                            s2
                          end
                     else if (List.hd pos)=(ASTD_term.Const(ASTD_constant.Integer 2))
                             then begin 
                                        ASTD_state.synchronisation_s_of s1
                                                  (apply_local (ASTD_astd.get_synchro_astd2 astd) arrow a s2 (List.tl pos) env)
                                  end                             
                             else failwith "impossible synchro state in execution" 
            end

        |(ASTD_state.Guard_s(_,s),ASTD_state.Guard_s(true,next_state)) ->  
                       ASTD_state.guard_s_of true (apply_local (ASTD_astd.get_guard_astd astd) arrow next_state s pos env)

        |(_,ASTD_state.Guard_s(false,next_state)) -> failwith "impossible guard state in execution"

        |(_,ASTD_state.QChoice_s(ASTD_state.ChoiceNotMade,_))-> failwith "impossible qchoice execution"

        |(ASTD_state.QChoice_s(_,s),ASTD_state.QChoice_s(val_used,next_state)) -> 
               let (a,b,c,d)=ASTD_astd.get_data_qchoice astd 
               in let bind_env = ASTD_environment.bind b (ASTD_term.Const(ASTD_constant.FreeConst))
               in let env2=(ASTD_environment.add_binding bind_env env)
                 in ASTD_state.qchoice_s_of val_used (apply_local d arrow next_state s pos env2)

        |(ASTD_state.QSynchronisation_s (trans,fin_dom,not_init_dom,init),s) ->
               let (name,b,c,d,e,f,g,h)=ASTD_astd.get_data_qsynchronisation astd 
               in let bind_env = ASTD_environment.bind b (ASTD_term.Const(ASTD_constant.FreeConst))
               in let env2=(ASTD_environment.add_binding bind_env env)
               in let a=(ASTD_term.extract_constant_from_term (List.hd pos))
               in let v=ASTD_constant.value_of a
               in begin 
                  let new_s=apply_local e arrow s (ASTD_state.get_synch_state not_init_dom init name a) (List.tl pos) env2
                  in begin ASTD_state.register_synch name a new_s;
                     let not_init_dom2 = ASTD_constant.insert v not_init_dom
                     in let (arrow_list,boolean)= ASTD_state.evaluate_arrows e new_s env2   
                     in let l2=ASTD_state.maj_arrows (ASTD_constant.create_dom_from_val v) arrow_list trans
                     in begin 
                        if boolean  
                        then ASTD_state.qsynchronisation_s_of l2 (ASTD_constant.remove v fin_dom) not_init_dom2 init
                        else ASTD_state.qsynchronisation_s_of l2 (ASTD_constant.insert v fin_dom) not_init_dom2 init
                        end
                     end
                  end

        |(_,ASTD_state.Call_s (false,state)) -> failwith "impossible call execution"

        |(ASTD_state.Call_s(false,_),ASTD_state.Call_s (true,next_state)) -> 
                      let astd2=ASTD_astd.get_astd (ASTD_astd.get_called_name astd)
                      in let (a,b,c)=ASTD_astd.get_data_call astd
            in ASTD_state.call_s_of true (apply_local astd2 arrow next_state (ASTD_state.init astd2) pos  
                                                                       (ASTD_environment.increase_call env c))
 
        |(ASTD_state.Call_s(true,s),ASTD_state.Call_s (true,next_state)) -> 
                      let astd2=ASTD_astd.get_astd (ASTD_astd.get_called_name astd)
                      in let (a,b,c)=ASTD_astd.get_data_call astd
                      in ASTD_state.call_s_of true (apply_local astd2 arrow next_state s pos (ASTD_environment.increase_call env c))

        |(a,ASTD_state.NotDefined)->if a=ASTD_state.NotDefined then failwith "impossible execution: position undefined" 
                                                               else if (a=ASTD_state.Elem)
                                                                 then failwith "impossible execution: notDef elem"
                                                                 else apply_local astd arrow a a pos env

        |(a,ASTD_state.Elem)->if a=ASTD_state.NotDefined then failwith "impossible execution: position undefined" 
                                                               else if (a=ASTD_state.Elem)
                                                                 then failwith "impossible execution: notDef elem"
                                                                 else apply_local astd arrow a a pos env


        |_-> failwith "impossible execution"




let rec apply_tsub astd arrow state2 state pos env = 
    let from=ASTD_arrow.get_from arrow
    in match (state,state2) with

        |(ASTD_state.Automata_s(n,h,s),ASTD_state.Automata_s(name_next,h_new,next_state)) ->
            begin 
              if from=name_next
                 then begin 
                    let middle=ASTD_arrow.get_through arrow and dest=ASTD_arrow.get_to arrow
                    in let next_st = ASTD_state.goto_automata (ASTD_astd.find_subastd middle (ASTD_astd.get_sub astd)) dest h;
                      in begin 
                             if ASTD_state.is_automata s 
                             then let h2=ASTD_state.modify_h h from s
                                  in if (dest !="H1") && (dest!= "H2")
                                         then (ASTD_state.automata_s_of middle
                                                                        h2
                                                                        next_st )
                                         else (ASTD_state.automata_s_of middle
                                                                        h2
                                                                        next_st )

                             else  if (dest !="H1") && (dest!= "H2")
                                      then (ASTD_state.automata_s_of middle
                                                                     h
                                                                     next_st )
                                      else (ASTD_state.automata_s_of middle
                                                                     h
                                                                     next_st )
                         end
                      end

                 else begin 
                        ASTD_state.automata_s_of name_next
                                    h
                            (apply_tsub (ASTD_astd.find_subastd name_next (ASTD_astd.get_sub astd)) arrow next_state s pos env)
                      end
            end

        |(ASTD_state.Sequence_s (ASTD_state.Left,s),ASTD_state.Sequence_s (ASTD_state.Right,next_state))-> 
                       let astd2=(ASTD_astd.get_seq_r astd)
               in ASTD_state.sequence_s_of ASTD_state.Right (apply_tsub astd2 arrow next_state (ASTD_state.init astd2) pos env)


        |(ASTD_state.Sequence_s (_,s),ASTD_state.Sequence_s (ASTD_state.Right,next_state)) -> 
                   ASTD_state.sequence_s_of ASTD_state.Right (apply_tsub (ASTD_astd.get_seq_r astd) arrow next_state s pos env)

        |(ASTD_state.Sequence_s (_,s),ASTD_state.Sequence_s (ASTD_state.Left,next_state)) -> 
                   ASTD_state.sequence_s_of ASTD_state.Left (apply_tsub (ASTD_astd.get_seq_l astd) arrow next_state s pos env)
        

        |(ASTD_state.Choice_s (ASTD_state.Snd,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) ->
                       let astd2= (ASTD_astd.get_choice1 astd);
                   in ASTD_state.choice_s_of ASTD_state.Fst (apply_tsub astd2 arrow next_state (ASTD_state.init astd2) pos env)


        |(ASTD_state.Choice_s (ASTD_state.Undef,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) ->
                       let astd2= (ASTD_astd.get_choice1 astd);
                   in ASTD_state.choice_s_of ASTD_state.Fst (apply_tsub astd2 arrow next_state (ASTD_state.init astd2) pos env)

        |(ASTD_state.Choice_s (ASTD_state.Fst,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) -> 
                   ASTD_state.choice_s_of ASTD_state.Fst (apply_tsub (ASTD_astd.get_choice1 astd) arrow next_state s pos env)


        |(ASTD_state.Choice_s (ASTD_state.Fst,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) ->
                       let astd2= (ASTD_astd.get_choice2 astd);
                   in ASTD_state.choice_s_of ASTD_state.Fst (apply_tsub astd2 arrow next_state (ASTD_state.init astd2) pos env)


        |(ASTD_state.Choice_s (ASTD_state.Undef,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) ->
                       let astd2= (ASTD_astd.get_choice2 astd);
                   in ASTD_state.choice_s_of ASTD_state.Fst (apply_tsub astd2 arrow next_state (ASTD_state.init astd2) pos env)

        |(ASTD_state.Choice_s (ASTD_state.Snd,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) -> 
                  ASTD_state.choice_s_of ASTD_state.Snd (apply_tsub (ASTD_astd.get_choice2 astd) arrow next_state s pos env)

        |(_,ASTD_state.Choice_s (ASTD_state.Undef,next_state)) -> failwith "impossible choice state in execution"

        |(ASTD_state.Kleene_s (_,s),ASTD_state.Kleene_s (true,next_state)) -> 
                   ASTD_state.kleene_s_of true (apply_tsub (ASTD_astd.get_astd_kleene astd) arrow next_state s pos env)

        |(_,ASTD_state.Kleene_s (false,next_state)) -> failwith "impossible kleene state in execution"

        |(ASTD_state.Synchronisation_s (s1,s2),a) -> 
                  if (List.hd pos)=(ASTD_term.Const(ASTD_constant.Integer 1))
                     then ASTD_state.synchronisation_s_of 
                                           (apply_tsub (ASTD_astd.get_synchro_astd1 astd) arrow a s1 (List.tl pos) env)
                                            s2
                     else if (List.hd pos)=(ASTD_term.Const(ASTD_constant.Integer 2))
                             then ASTD_state.synchronisation_s_of s1
                                                    (apply_tsub (ASTD_astd.get_synchro_astd2 astd) arrow a s2 (List.tl pos) env)
                                                               
                             else failwith "impossible synchro state in execution" 

        |(ASTD_state.Guard_s(_,s),ASTD_state.Guard_s(true,next_state)) -> 
                       ASTD_state.guard_s_of true (apply_tsub (ASTD_astd.get_guard_astd astd) arrow next_state s pos env)

        |(_,ASTD_state.Guard_s(false,next_state)) -> failwith "impossible guard state in execution"

        |(_,ASTD_state.QChoice_s(ASTD_state.ChoiceNotMade,_))-> failwith "impossible qchoice execution"

        |(ASTD_state.QChoice_s(_,s),ASTD_state.QChoice_s(val_used,next_state)) -> 
               let (a,b,c,d)=ASTD_astd.get_data_qchoice astd 
               in let bind_env = ASTD_environment.bind b (ASTD_term.Const(ASTD_constant.FreeConst))
               in let env2=(ASTD_environment.add_binding bind_env env)
                    in  ASTD_state.qchoice_s_of val_used (apply_tsub d arrow next_state s pos env2)




        |(ASTD_state.QSynchronisation_s (trans,fin_dom,not_init_dom,init),s) ->
               let (name,b,c,d,e,f,g,h)=ASTD_astd.get_data_qsynchronisation astd 
               in let bind_env = ASTD_environment.bind b (ASTD_term.Const(ASTD_constant.FreeConst))
               in let env2=(ASTD_environment.add_binding bind_env env)
               in let a=(ASTD_term.extract_constant_from_term (List.hd pos))
               in let v=ASTD_constant.value_of a
               in begin 
                  let new_s=apply_tsub e arrow s (ASTD_state.get_synch_state not_init_dom init name a) (List.tl pos) env2
                  in begin ASTD_state.register_synch name a new_s;
                     let not_init_dom2 = ASTD_constant.insert v not_init_dom
                     in let (arrow_list,boolean)= ASTD_state.evaluate_arrows e new_s env2   
                     in let l2=ASTD_state.maj_arrows (ASTD_constant.create_dom_from_val v) arrow_list trans
                     in begin 
                        if boolean  
                        then ASTD_state.qsynchronisation_s_of l2 (ASTD_constant.remove v fin_dom) not_init_dom2 init
                        else ASTD_state.qsynchronisation_s_of l2 (ASTD_constant.insert v fin_dom) not_init_dom2 init
                        end
                     end
                  end

        |(_,ASTD_state.Call_s (false,state)) -> failwith "impossible call execution"

        |(ASTD_state.Call_s(false,_),ASTD_state.Call_s (true,next_state)) -> 
                      let astd2=ASTD_astd.get_astd (ASTD_astd.get_called_name astd);
                      in let (a,b,c)=ASTD_astd.get_data_call astd
         in ASTD_state.call_s_of true (apply_tsub astd2 arrow next_state (ASTD_state.init astd2) pos 
                                 (ASTD_environment.increase_call env c))


        |(ASTD_state.Call_s(true,s),ASTD_state.Call_s (true,next_state)) -> 
                      let astd2=ASTD_astd.get_astd (ASTD_astd.get_called_name astd);
                      in let (a,b,c)=ASTD_astd.get_data_call astd
                  in ASTD_state.call_s_of true (apply_tsub astd2 arrow next_state s pos (ASTD_environment.increase_call env c))

        |(a,ASTD_state.NotDefined)->if a=ASTD_state.NotDefined 
                                     then failwith "impossible execution: position undefined" 
                                     else if (a=ASTD_state.Elem)
                                         then failwith "impossible execution: notDef elem"
                                         else apply_tsub astd arrow a a pos env

        |(a,ASTD_state.Elem)->if a=ASTD_state.NotDefined then failwith "impossible execution: position undefined" 
                                                         else if (a=ASTD_state.Elem)
                                                             then failwith "impossible execution: notDef elem"
                                                             else apply_tsub astd arrow a a pos env

        |_-> failwith "impossible execution"







let rec apply_fsub astd arrow state2 state pos env = 
    let middle=ASTD_arrow.get_through arrow
    in match (state,state2) with
        |(ASTD_state.Automata_s(n,h,s),ASTD_state.Automata_s(name_next,h_new,next_state)) ->
                   begin
              if middle=name_next
                 then begin  
                      let dest=ASTD_arrow.get_to arrow
                      in ASTD_state.automata_s_of dest
                                                  (ASTD_state.modify_h h middle s)
                                                  (ASTD_state.init (ASTD_astd.find_subastd dest (ASTD_astd.get_sub astd))) 
                      end
                 else begin 
                      ASTD_state.automata_s_of name_next
                                               h
                                               (apply_fsub (ASTD_astd.find_subastd name_next (ASTD_astd.get_sub astd))
                                                           arrow 
                                                           next_state 
                                                           s 
                                                           pos env)
                      end
        end

        |(ASTD_state.Sequence_s (ASTD_state.Left,s),ASTD_state.Sequence_s (ASTD_state.Right,next_state))-> 
                       let astd2=(ASTD_astd.get_seq_r astd)
               in ASTD_state.sequence_s_of ASTD_state.Right (apply_fsub astd2 arrow next_state (ASTD_state.init astd2) pos env)


        |(ASTD_state.Sequence_s (_,s),ASTD_state.Sequence_s (ASTD_state.Right,next_state)) -> 
                 ASTD_state.sequence_s_of ASTD_state.Right (apply_fsub (ASTD_astd.get_seq_r astd) arrow next_state s pos env)

        |(ASTD_state.Sequence_s (_,s),ASTD_state.Sequence_s (ASTD_state.Left,next_state)) -> 
                 ASTD_state.sequence_s_of ASTD_state.Left (apply_fsub (ASTD_astd.get_seq_l astd) arrow next_state s pos env)
 
        |(ASTD_state.Choice_s (ASTD_state.Snd,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) ->
                       let astd2= (ASTD_astd.get_choice1 astd);
                in ASTD_state.choice_s_of ASTD_state.Fst (apply_fsub astd2 arrow next_state (ASTD_state.init astd2) pos env)

        |(ASTD_state.Choice_s (ASTD_state.Undef,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) ->
                       let astd2= (ASTD_astd.get_choice1 astd);
                in ASTD_state.choice_s_of ASTD_state.Fst (apply_fsub astd2 arrow next_state (ASTD_state.init astd2) pos env)
       
        |(ASTD_state.Choice_s (ASTD_state.Fst,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) -> 
                  ASTD_state.choice_s_of ASTD_state.Fst (apply_fsub (ASTD_astd.get_choice1 astd) arrow next_state s pos env)

        |(ASTD_state.Choice_s (ASTD_state.Fst,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) ->
                       let astd2= (ASTD_astd.get_choice2 astd);
                in ASTD_state.choice_s_of ASTD_state.Fst (apply_fsub astd2 arrow next_state (ASTD_state.init astd2) pos env)


        |(ASTD_state.Choice_s (ASTD_state.Undef,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) ->
                       let astd2= (ASTD_astd.get_choice2 astd);
                 in ASTD_state.choice_s_of ASTD_state.Fst (apply_fsub astd2 arrow next_state (ASTD_state.init astd2) pos env)

        |(ASTD_state.Choice_s (ASTD_state.Snd,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) -> 
                  ASTD_state.choice_s_of ASTD_state.Snd (apply_fsub (ASTD_astd.get_choice2 astd) arrow next_state s pos env)

        |(_,ASTD_state.Choice_s (ASTD_state.Undef,next_state)) -> failwith "impossible choice state in execution"

        |(ASTD_state.Kleene_s (_,s),ASTD_state.Kleene_s (true,next_state)) -> 
                 ASTD_state.kleene_s_of true (apply_fsub (ASTD_astd.get_astd_kleene astd) arrow next_state s pos env)

        |(_,ASTD_state.Kleene_s (false,next_state)) -> failwith "impossible kleene state in execution"

        |(ASTD_state.Synchronisation_s (s1,s2),a) -> 
                  if (List.hd pos)=(ASTD_term.Const(ASTD_constant.Integer 1))
                     then ASTD_state.synchronisation_s_of 
                                           (apply_fsub (ASTD_astd.get_synchro_astd1 astd) arrow a s1 (List.tl pos) env)
                                            s2
                     else if (List.hd pos)=(ASTD_term.Const(ASTD_constant.Integer 2))
                             then ASTD_state.synchronisation_s_of s1
                                                    (apply_fsub (ASTD_astd.get_synchro_astd2 astd) arrow a s2 (List.tl pos) env)
                                                               
                             else failwith "impossible synchro state in execution" 

        |(ASTD_state.Guard_s(_,s),ASTD_state.Guard_s(true,next_state)) -> 
                       ASTD_state.guard_s_of true (apply_fsub (ASTD_astd.get_guard_astd astd) arrow next_state s pos env)

        |(_,ASTD_state.Guard_s(false,next_state)) -> failwith "impossible guard state in execution"

        |(_,ASTD_state.QChoice_s(ASTD_state.ChoiceNotMade,next_state))-> failwith "impossible qchoice execution"

        |(ASTD_state.QChoice_s(_,s),ASTD_state.QChoice_s(val_used,next_state)) -> 
               let (a,b,c,d)=ASTD_astd.get_data_qchoice astd 
               in let bind_env = ASTD_environment.bind b (ASTD_term.Const(ASTD_constant.FreeConst))
               in let env2=(ASTD_environment.add_binding bind_env env)
                  in  ASTD_state.qchoice_s_of val_used (apply_fsub d arrow next_state s pos env2)


        |(ASTD_state.QSynchronisation_s (trans,fin_dom,not_init_dom,init),s) ->
               let (name,b,c,d,e,f,g,h)=ASTD_astd.get_data_qsynchronisation astd 
               in let bind_env = ASTD_environment.bind b (ASTD_term.Const(ASTD_constant.FreeConst))
               in let env2=(ASTD_environment.add_binding bind_env env)
               in let a=(ASTD_term.extract_constant_from_term(List.hd pos))
               in let v=ASTD_constant.value_of a
               in begin 
                  let new_s=apply_fsub e arrow s (ASTD_state.get_synch_state not_init_dom init name a) (List.tl pos) env2
                  in begin ASTD_state.register_synch name a new_s;
                     let not_init_dom2 = ASTD_constant.insert v not_init_dom
                     in let (arrow_list,boolean)= ASTD_state.evaluate_arrows e new_s env2   
                     in let l2=ASTD_state.maj_arrows (ASTD_constant.create_dom_from_val v) arrow_list trans
                     in begin 
                        if boolean  
                        then ASTD_state.qsynchronisation_s_of l2 (ASTD_constant.remove v fin_dom) not_init_dom2 init
                        else ASTD_state.qsynchronisation_s_of l2 (ASTD_constant.insert v fin_dom) not_init_dom2 init
                        end
                     end
                  end
                      
        |(_,ASTD_state.Call_s (false,state)) -> failwith "impossible call execution"

        |(ASTD_state.Call_s(false,_),ASTD_state.Call_s (true,next_state)) -> 
                   let astd2=ASTD_astd.get_astd (ASTD_astd.get_called_name astd)
                   in let (a,b,c)=ASTD_astd.get_data_call astd
                   in ASTD_state.call_s_of true (apply_fsub astd2 arrow next_state (ASTD_state.init astd2) pos 
                                                                                     (ASTD_environment.increase_call env c))

        |(ASTD_state.Call_s(true,s),ASTD_state.Call_s (true,next_state)) -> 
                   let astd2=ASTD_astd.get_astd (ASTD_astd.get_called_name astd)
                   in let (a,b,c)=ASTD_astd.get_data_call astd
                   in ASTD_state.call_s_of true (apply_fsub astd2 arrow next_state s pos (ASTD_environment.increase_call env c))



        |(something_else,ASTD_state.NotDefined)->if (something_else=ASTD_state.NotDefined) 
                                    then failwith "impossible execution: notDef" 
                                    else begin 
                                         if (something_else=ASTD_state.Elem)
                                         then failwith "impossible execution: elem"
                                         else apply_fsub astd arrow something_else something_else pos env
                                         end
        |(something_else,ASTD_state.Elem)->if (something_else=ASTD_state.NotDefined) 
                                    then failwith "impossible execution: elem notDef" 
                                    else begin 
                                         if (something_else=ASTD_state.Elem)
                                         then failwith "impossible execution: elem elem"
                                         else apply_fsub astd arrow something_else something_else pos env
                                         end
        |_-> failwith "impossible execution"







let apply astd arrow state state2 l = begin 
   match arrow with
     | ASTD_arrow.Local(_,_,_,_,_ ) -> apply_local astd arrow state2 state l []
     | ASTD_arrow.From_sub (_,_,_,_,_,_ ) -> apply_fsub astd arrow state2 state l []
     | ASTD_arrow.To_sub (_,_,_,_,_,_ ) -> apply_tsub astd arrow state2 state l []

                                      end
;;



let rec execute_possibilities astd state list_poss path1 = match (list_poss) with
                           | ASTD_possibilities.Mult(a) -> begin 
                                                    if a=[] 
                                         then begin state end
                                         else execute_possibilities astd state (ASTD_possibilities.choose_next a) path1
                                                           end
                           | ASTD_possibilities.Possibility(state2,arrow) -> 
            begin print_newline () ;
                  print_endline ("   ...execution en cours de "^(ASTD_transition.get_label(ASTD_arrow.get_transition arrow)));
                   apply astd arrow state state2 path1
            end
                           | ASTD_possibilities.Synch (a) ->synchronize astd state (list_poss) path1 



and synchronize astd state synchro_list path1 = match synchro_list with

   |ASTD_possibilities.Synch ((v,a)::q) -> 
                    let state2 = synchronize astd state (ASTD_possibilities.Synch q) path1 
                    in if (ASTD_possibilities.no_possibilities a) 
                                               then begin 
                                                         state2
                                                    end 
                                               else begin 
                                                        (execute_possibilities astd state2 a (path1@[v]))  
                                                    end

   |ASTD_possibilities.Synch [] -> state

   |_ -> failwith "unappropriate synchronise"
;;





let execute astd state event  =
               let (list_poss,f)=ASTD_possibilities.possible_evolutions astd  state event []
                 in begin 
                        (*  print_endline "=========Start Possibilities" ; 
                          ASTD_possibilities.print list_poss astd "" [] ; 
                          print_endline "=========End Possibilities" ;  *)
                          
                        if (ASTD_possibilities.possible list_poss)
                             then execute_possibilities astd state list_poss [] 
                             else failwith "Not Possible"
                    end
;;






let rec exec_sequence astd state event_list = match event_list with
   | h::t -> print_newline();
             print_endline ("=============================================================");
             print_endline ("=============================================================");
             print_endline ("=============================================================");
             print_endline ("Execution of : "^(ASTD_label.string_of(ASTD_event.get_label h)));
             let s2= execute astd state h
             in print_newline () ;
                print_endline "=========NewState" ; 
                print_newline () ;
                ASTD_state.print s2 astd "";
                print_newline () ;
                print_endline "=========End NewState" ;       
                exec_sequence astd s2 t
   | [] -> print_newline () ;
;;      

