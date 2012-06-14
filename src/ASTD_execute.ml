type path = int list



let string_of_val a = match a with
  |0->"zero"
  |1->"un"
  |2->"deux"
  |3->"trois"
  |_->"plus"
;;


let rec string_of_list a = match a with
  |h::t-> (string_of_int h)^(string_of_list t)
  |[]->""
;;


let rec apply_local astd arrow state2 state pos = 
    let from=ASTD_arrow.get_from arrow
    in match (state,state2) with

        |(ASTD_state.Automata_s(_,h,s),ASTD_state.Automata_s(name_next,h_new,next_state)) -> 

              if from=name_next
                 then let dest=ASTD_arrow.get_to arrow 
                          in begin 
                                   if (ASTD_state.is_automata s) 
                                         then let h2 = ASTD_state.modify_h h from s
                                                    in
                                                    ASTD_state.automata_s_of dest
                                                    h2
                                                    (ASTD_state.init (ASTD_astd.find_substate dest (ASTD_astd.get_sub astd)))
                                         else       ASTD_state.automata_s_of dest
                                                    h
                                                    (ASTD_state.init (ASTD_astd.find_substate dest (ASTD_astd.get_sub astd)))
                             end
                 else ASTD_state.automata_s_of name_next
                                              h
                                            (apply_local (ASTD_astd.find_substate name_next (ASTD_astd.get_sub astd)) arrow next_state s pos)
                                            

        |(ASTD_state.Sequence_s (ASTD_state.Left,s),ASTD_state.Sequence_s (ASTD_state.Right,next_state))->
                       let astd2=(ASTD_astd.get_seq_r astd)
                       in ASTD_state.sequence_s_of ASTD_state.Right (apply_local astd2 arrow next_state (ASTD_state.init astd2) pos)

        |(ASTD_state.Sequence_s (_,s),ASTD_state.Sequence_s (ASTD_state.Right,next_state))-> 
                       ASTD_state.sequence_s_of ASTD_state.Right (apply_local (ASTD_astd.get_seq_r astd) arrow next_state s pos)

        
        |(ASTD_state.Sequence_s (_,s),ASTD_state.Sequence_s (ASTD_state.Left,next_state))-> 
                       
                       ASTD_state.sequence_s_of ASTD_state.Left (apply_local (ASTD_astd.get_seq_l astd) arrow next_state s pos)
        


        |(ASTD_state.Choice_s (ASTD_state.Undef,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) ->
                       let astd2= (ASTD_astd.get_choice1 astd)
                       in ASTD_state.choice_s_of ASTD_state.Fst (apply_local astd2 arrow next_state (ASTD_state.init astd2) pos)

        |(ASTD_state.Choice_s (ASTD_state.Snd,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) ->
                       let astd2= (ASTD_astd.get_choice1 astd)
                       in ASTD_state.choice_s_of ASTD_state.Fst (apply_local astd2 arrow next_state (ASTD_state.init astd2) pos)


        |(ASTD_state.Choice_s (ASTD_state.Fst,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) -> 
                       ASTD_state.choice_s_of ASTD_state.Fst (apply_local (ASTD_astd.get_choice1 astd) arrow next_state s pos)

        |(ASTD_state.Choice_s (ASTD_state.Fst,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) ->
                       let astd2= (ASTD_astd.get_choice2 astd);
                       in ASTD_state.choice_s_of ASTD_state.Snd (apply_local astd2 arrow next_state (ASTD_state.init astd2) pos)



        |(ASTD_state.Choice_s (ASTD_state.Undef,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) ->
                       let astd2= (ASTD_astd.get_choice2 astd);
                       in ASTD_state.choice_s_of ASTD_state.Snd (apply_local astd2 arrow next_state (ASTD_state.init astd2) pos)


        |(ASTD_state.Choice_s (ASTD_state.Snd,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) -> 
                       ASTD_state.choice_s_of ASTD_state.Snd (apply_local (ASTD_astd.get_choice2 astd) arrow next_state s pos)

        |(_,ASTD_state.Choice_s (ASTD_state.Undef,next_state)) -> failwith "impossible choice state in execution"



        |(ASTD_state.Kleene_s (_,s),ASTD_state.Kleene_s (true,next_state)) ->  
                       ASTD_state.kleene_s_of true (apply_local (ASTD_astd.get_astd_kleene astd) arrow next_state s pos)

        |(_,ASTD_state.Kleene_s (false,next_state)) -> failwith "impossible kleene state in execution"

        |(ASTD_state.Synchronisation_s (s1,s2),a) ->  

                  if (List.hd pos)=1 
                     then ASTD_state.synchronisation_s_of 
                                           (apply_local (ASTD_astd.get_synchro_astd1 astd) arrow a s1 (List.tl pos))
                                            s2
                     else if (List.hd pos)=2 
                             then ASTD_state.synchronisation_s_of s1
                                                    (apply_local (ASTD_astd.get_synchro_astd2 astd) arrow a s2 (List.tl pos))
                                                               
                             else failwith "impossible synchro state in execution" 

        |(ASTD_state.Guard_s(_,s),ASTD_state.Guard_s(true,next_state)) ->  
                       ASTD_state.guard_s_of true (apply_local (ASTD_astd.get_guard_astd astd) arrow next_state s pos)

        |(_,ASTD_state.Guard_s(false,next_state)) -> failwith "impossible guard state in execution"

        |(_,ASTD_state.QChoice_s(ASTD_state.ChoiceNotMade,_))-> failwith "impossible qchoice execution"

        |(ASTD_state.QChoice_s(_,s),ASTD_state.QChoice_s(val_used,next_state)) -> 
                      ASTD_state.qchoice_s_of val_used (apply_local (ASTD_astd.get_qastd astd) arrow next_state s pos)

        |(ASTD_state.QSynchronisation_s (s_list),s) ->
                      ASTD_state.qsynchronisation_s_of (apply_local_qsynchro_s (ASTD_astd.get_qastd astd) arrow s_list s pos ) 


        |(_,ASTD_state.Call_s (false,state)) -> failwith "impossible call execution"

        |(ASTD_state.Call_s(false,_),ASTD_state.Call_s (true,next_state)) -> 
                      let astd2=ASTD_astd.get_astd (ASTD_astd.get_called_name astd)
                      in ASTD_state.call_s_of true (apply_local astd2 arrow next_state (ASTD_state.init astd2) pos)

        |(ASTD_state.Call_s(true,s),ASTD_state.Call_s (true,next_state)) -> 
                      let astd2=ASTD_astd.get_astd (ASTD_astd.get_called_name astd)
                      in ASTD_state.call_s_of true (apply_local astd2 arrow next_state s pos)

        |(a,ASTD_state.NotDefined)->if a=ASTD_state.NotDefined then failwith "impossible execution: position undefined" 
                                                               else apply_local astd arrow a a pos
        |_-> failwith "impossible execution"

and apply_local_qsynchro_s astd arrow s_list s pos = match (pos,s_list) with
    |(1::q,(value,a)::b) ->  (value,(apply_local astd arrow s a q))::(b)
    |(h::q,(value,a)::b) -> (value,a)::(apply_local_qsynchro_s astd arrow b s ((h-1)::q) )
    |_ -> failwith "qsynchro_s impossible to apply, out of the loop"

;;


let rec apply_tsub astd arrow state2 state pos = 
    let from=ASTD_arrow.get_from arrow
    in match (state,state2) with

        |(ASTD_state.Automata_s(n,h,s),ASTD_state.Automata_s(name_next,h_new,next_state)) ->
              if from=name_next
                 then begin 
                    let middle=ASTD_arrow.get_through arrow and dest=ASTD_arrow.get_to arrow
                    in let next_st = ASTD_state.goto_automata (ASTD_astd.find_substate middle (ASTD_astd.get_sub astd)) dest h;
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
                                (apply_tsub (ASTD_astd.find_substate name_next (ASTD_astd.get_sub astd)) arrow next_state s pos)
                      end


        |(ASTD_state.Sequence_s (ASTD_state.Left,s),ASTD_state.Sequence_s (ASTD_state.Right,next_state))-> 
                       let astd2=(ASTD_astd.get_seq_r astd)
                       in ASTD_state.sequence_s_of ASTD_state.Right (apply_tsub astd2 arrow next_state (ASTD_state.init astd2) pos)


        |(ASTD_state.Sequence_s (_,s),ASTD_state.Sequence_s (ASTD_state.Right,next_state)) -> 
                       ASTD_state.sequence_s_of ASTD_state.Right (apply_tsub (ASTD_astd.get_seq_r astd) arrow next_state s pos )

        |(ASTD_state.Sequence_s (_,s),ASTD_state.Sequence_s (ASTD_state.Left,next_state)) -> 
                       ASTD_state.sequence_s_of ASTD_state.Left (apply_tsub (ASTD_astd.get_seq_l astd) arrow next_state s pos )
        

        |(ASTD_state.Choice_s (ASTD_state.Snd,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) ->
                       let astd2= (ASTD_astd.get_choice1 astd);
                       in ASTD_state.choice_s_of ASTD_state.Fst (apply_tsub astd2 arrow next_state (ASTD_state.init astd2) pos)


        |(ASTD_state.Choice_s (ASTD_state.Undef,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) ->
                       let astd2= (ASTD_astd.get_choice1 astd);
                       in ASTD_state.choice_s_of ASTD_state.Fst (apply_tsub astd2 arrow next_state (ASTD_state.init astd2) pos)

        |(ASTD_state.Choice_s (ASTD_state.Fst,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) -> 
                       ASTD_state.choice_s_of ASTD_state.Fst (apply_tsub (ASTD_astd.get_choice1 astd) arrow next_state s pos )


        |(ASTD_state.Choice_s (ASTD_state.Fst,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) ->
                       let astd2= (ASTD_astd.get_choice2 astd);
                       in ASTD_state.choice_s_of ASTD_state.Fst (apply_tsub astd2 arrow next_state (ASTD_state.init astd2) pos)


        |(ASTD_state.Choice_s (ASTD_state.Undef,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) ->
                       let astd2= (ASTD_astd.get_choice2 astd);
                       in ASTD_state.choice_s_of ASTD_state.Fst (apply_tsub astd2 arrow next_state (ASTD_state.init astd2) pos)

        |(ASTD_state.Choice_s (ASTD_state.Snd,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) -> 
                       ASTD_state.choice_s_of ASTD_state.Snd (apply_tsub (ASTD_astd.get_choice2 astd) arrow next_state s pos )

        |(_,ASTD_state.Choice_s (ASTD_state.Undef,next_state)) -> failwith "impossible choice state in execution"

        |(ASTD_state.Kleene_s (_,s),ASTD_state.Kleene_s (true,next_state)) -> 
                       ASTD_state.kleene_s_of true (apply_tsub (ASTD_astd.get_astd_kleene astd) arrow next_state s pos )

        |(_,ASTD_state.Kleene_s (false,next_state)) -> failwith "impossible kleene state in execution"

        |(ASTD_state.Synchronisation_s (s1,s2),a) -> 
                     if (List.hd pos)=1 
                        then ASTD_state.synchronisation_s_of (apply_tsub (ASTD_astd.get_synchro_astd1 astd) arrow a s1 (List.tl pos))
                                                             s2
                        else if (List.hd pos)=2 
                                then ASTD_state.synchronisation_s_of s1
                                                                (apply_tsub (ASTD_astd.get_synchro_astd2 astd) arrow a s2 (List.tl pos))
                                else failwith "impossible synchro state in execution" 

        |(ASTD_state.Guard_s(_,s),ASTD_state.Guard_s(true,next_state)) -> 
                       ASTD_state.guard_s_of true (apply_tsub (ASTD_astd.get_guard_astd astd) arrow next_state s pos )

        |(_,ASTD_state.Guard_s(false,next_state)) -> failwith "impossible guard state in execution"

        |(_,ASTD_state.QChoice_s(ASTD_state.ChoiceNotMade,_))-> failwith "impossible qchoice execution"

        |(ASTD_state.QChoice_s(_,s),ASTD_state.QChoice_s(val_used,next_state)) -> 
                      ASTD_state.qchoice_s_of val_used (apply_tsub (ASTD_astd.get_qastd astd) arrow next_state s pos)

        |(ASTD_state.QSynchronisation_s (s_list),s) ->
                      ASTD_state.qsynchronisation_s_of (apply_tsub_qsynchro_s (ASTD_astd.get_qastd astd) arrow s_list s pos) 


        |(_,ASTD_state.Call_s (false,state)) -> failwith "impossible call execution"

        |(ASTD_state.Call_s(false,_),ASTD_state.Call_s (true,next_state)) -> 
                      let astd2=ASTD_astd.get_astd (ASTD_astd.get_called_name astd);
                      in ASTD_state.call_s_of true (apply_tsub astd2 arrow next_state (ASTD_state.init astd2) pos)

        |(ASTD_state.Call_s(true,s),ASTD_state.Call_s (true,next_state)) -> 
                      let astd2=ASTD_astd.get_astd (ASTD_astd.get_called_name astd);
                      in ASTD_state.call_s_of true (apply_tsub astd2 arrow next_state s pos)

        |(a,ASTD_state.NotDefined)->if a=ASTD_state.NotDefined then failwith "impossible execution: position undefined" 
                                                               else apply_tsub astd arrow a a pos

        |_-> failwith "impossible execution"


and apply_tsub_qsynchro_s astd arrow s_list s pos = match (pos,s_list) with
    |(1::q,(value,a)::b) ->  (value,(apply_tsub astd arrow s a q))::(b)
    |(h::q,(value,a)::b) -> (value,a)::(apply_tsub_qsynchro_s astd arrow b s ((h-1)::q) )
    |_ -> failwith "qsynchro_s impossible to apply, out of the loop"

;;




let rec apply_fsub astd arrow state2 state pos = 
    let middle=ASTD_arrow.get_through arrow
    in match (state,state2) with
        |(ASTD_state.Automata_s(n,h,s),ASTD_state.Automata_s(name_next,h_new,next_state)) ->
        begin print_endline (middle^"    "^name_next);
              if middle=name_next
                 then begin  
                      let dest=ASTD_arrow.get_to arrow
                      in ASTD_state.automata_s_of dest
                                                  (ASTD_state.modify_h h middle s)
                                                  (ASTD_state.init (ASTD_astd.find_substate dest (ASTD_astd.get_sub astd))) 
                      end
                 else begin 
                      ASTD_state.automata_s_of name_next
                                               h
                                               (apply_fsub (ASTD_astd.find_substate name_next (ASTD_astd.get_sub astd))
                                                           arrow 
                                                           next_state 
                                                           s 
                                                           pos)
                      end
        end

        |(ASTD_state.Sequence_s (ASTD_state.Left,s),ASTD_state.Sequence_s (ASTD_state.Right,next_state))-> 
                       let astd2=(ASTD_astd.get_seq_r astd)
                       in ASTD_state.sequence_s_of ASTD_state.Right (apply_fsub astd2 arrow next_state (ASTD_state.init astd2) pos)


        |(ASTD_state.Sequence_s (_,s),ASTD_state.Sequence_s (ASTD_state.Right,next_state)) -> 
                       ASTD_state.sequence_s_of ASTD_state.Right (apply_fsub (ASTD_astd.get_seq_r astd) arrow next_state s pos)

        |(ASTD_state.Sequence_s (_,s),ASTD_state.Sequence_s (ASTD_state.Left,next_state)) -> 
                       ASTD_state.sequence_s_of ASTD_state.Left (apply_fsub (ASTD_astd.get_seq_l astd) arrow next_state s pos)
 
        |(ASTD_state.Choice_s (ASTD_state.Snd,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) ->
                       let astd2= (ASTD_astd.get_choice1 astd);
                       in ASTD_state.choice_s_of ASTD_state.Fst (apply_fsub astd2 arrow next_state (ASTD_state.init astd2) pos)

        |(ASTD_state.Choice_s (ASTD_state.Undef,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) ->
                       let astd2= (ASTD_astd.get_choice1 astd);
                       in ASTD_state.choice_s_of ASTD_state.Fst (apply_fsub astd2 arrow next_state (ASTD_state.init astd2) pos)
       
        |(ASTD_state.Choice_s (ASTD_state.Fst,s),ASTD_state.Choice_s (ASTD_state.Fst,next_state)) -> 
                       ASTD_state.choice_s_of ASTD_state.Fst (apply_fsub (ASTD_astd.get_choice1 astd) arrow next_state s pos)

        |(ASTD_state.Choice_s (ASTD_state.Fst,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) ->
                       let astd2= (ASTD_astd.get_choice2 astd);
                       in ASTD_state.choice_s_of ASTD_state.Fst (apply_fsub astd2 arrow next_state (ASTD_state.init astd2) pos)


        |(ASTD_state.Choice_s (ASTD_state.Undef,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) ->
                       let astd2= (ASTD_astd.get_choice2 astd);
                       in ASTD_state.choice_s_of ASTD_state.Fst (apply_fsub astd2 arrow next_state (ASTD_state.init astd2) pos)

        |(ASTD_state.Choice_s (ASTD_state.Snd,s),ASTD_state.Choice_s (ASTD_state.Snd,next_state)) -> 
                       ASTD_state.choice_s_of ASTD_state.Snd (apply_fsub (ASTD_astd.get_choice2 astd) arrow next_state s pos)

        |(_,ASTD_state.Choice_s (ASTD_state.Undef,next_state)) -> failwith "impossible choice state in execution"

        |(ASTD_state.Kleene_s (_,s),ASTD_state.Kleene_s (true,next_state)) -> 
                       ASTD_state.kleene_s_of true (apply_fsub (ASTD_astd.get_astd_kleene astd) arrow next_state s pos)

        |(_,ASTD_state.Kleene_s (false,next_state)) -> failwith "impossible kleene state in execution"

        |(ASTD_state.Synchronisation_s (s1,s2),a) -> 
                     if (List.hd pos)=1 
                   then ASTD_state.synchronisation_s_of (apply_fsub (ASTD_astd.get_synchro_astd1 astd) arrow a s1 (List.tl pos))
                                                        s2
                   else if (List.hd pos)=2 
                           then ASTD_state.synchronisation_s_of s1
                                                    (apply_fsub (ASTD_astd.get_synchro_astd2 astd) arrow a s2 (List.tl pos))
                                else failwith "impossible synchro state in execution" 

        |(ASTD_state.Guard_s(_,s),ASTD_state.Guard_s(true,next_state)) -> 
                       ASTD_state.guard_s_of true (apply_fsub (ASTD_astd.get_guard_astd astd) arrow next_state s pos)

        |(_,ASTD_state.Guard_s(false,next_state)) -> failwith "impossible guard state in execution"

        |(_,ASTD_state.QChoice_s(ASTD_state.ChoiceNotMade,next_state))-> failwith "impossible qchoice execution"

        |(ASTD_state.QChoice_s(_,s),ASTD_state.QChoice_s(val_used,next_state)) -> print_endline"coucou";
                      ASTD_state.qchoice_s_of val_used (apply_fsub (ASTD_astd.get_qastd astd) arrow next_state s pos)

        |(ASTD_state.QSynchronisation_s (s_list),s) ->
                      ASTD_state.qsynchronisation_s_of (apply_fsub_qsynchro_s (ASTD_astd.get_qastd astd) arrow s_list s pos) 
                      
        |(_,ASTD_state.Call_s (false,state)) -> failwith "impossible call execution"

        |(ASTD_state.Call_s(false,_),ASTD_state.Call_s (true,next_state)) -> 
                      let astd2=ASTD_astd.get_astd (ASTD_astd.get_called_name astd);
                      in ASTD_state.call_s_of true (apply_fsub astd2 arrow next_state (ASTD_state.init astd2) pos)

        |(ASTD_state.Call_s(true,s),ASTD_state.Call_s (true,next_state)) -> 
                      let astd2=ASTD_astd.get_astd (ASTD_astd.get_called_name astd);
                      in ASTD_state.call_s_of true (apply_fsub astd2 arrow next_state s pos)

        |(a,ASTD_state.NotDefined)->if (a=ASTD_state.NotDefined) 
                                    then failwith "impossible execution: notDef notDef" 
                                    else begin 
                                         if (a=ASTD_state.Elem)
                                         then failwith "impossible execution: notDef elem"
                                         else apply_fsub astd arrow a a pos
                                         end
        |(a,ASTD_state.Elem)->if (a=ASTD_state.NotDefined) 
                                    then failwith "impossible execution: elem notDef" 
                                    else begin 
                                         if (a=ASTD_state.Elem)
                                         then failwith "impossible execution: elem elem"
                                         else apply_fsub astd arrow a a pos
                                         end
        |_-> failwith "impossible execution"


and apply_fsub_qsynchro_s astd arrow s_list s pos = match (pos,s_list) with
    |(1::q,(value,a)::b) ->  (value,(apply_local astd arrow s a q))::(b)
    |(h::q,(value,a)::b) -> (value,a)::(apply_fsub_qsynchro_s astd arrow b s ((h-1)::q) )
    |_ -> failwith "qsynchro_s impossible to apply, out of the loop"

;;





let apply astd arrow state state2 l = print_endline(string_of_list l);
   match arrow with
     | ASTD_arrow.Local(_,_,_,_,_ ) -> apply_local astd arrow state2 state l
     | ASTD_arrow.From_sub (_,_,_,_,_,_ ) -> apply_fsub astd arrow state2 state l
     | ASTD_arrow.To_sub (_,_,_,_,_,_ ) -> apply_tsub astd arrow state2 state l
;;



let rec execute_possibilities astd state list_poss path1 = match (list_poss) with
                           | ASTD_possibilities.Mult(a) -> begin 
                                                    if a=[] 
                                         then state
                                         else execute_possibilities astd state (ASTD_possibilities.choose_next a) path1
                                                           end
                           | ASTD_possibilities.Possibility(state2,arrow) -> 
            begin  print_newline () ;
                   print_endline ("   ...execution en cours de "^(ASTD_transition.get_label(ASTD_arrow.get_transition arrow)));
                   apply astd arrow state state2 path1
            end
                           | ASTD_possibilities.Synch (a) ->synchronize astd state (ASTD_possibilities.Synch a) path1 1 



and synchronize astd state synchro_list path1 pos = match synchro_list with

   |ASTD_possibilities.Synch (a::q) -> 
                    let state2 = synchronize astd state (ASTD_possibilities.Synch q) path1 (pos+1)
                    in if (ASTD_possibilities.no_possibilities a) 
                                               then begin 
                                                         state2
                                                    end 
                                               else begin 
                                                        (execute_possibilities astd state2 a (path1@[pos]))  
                                                    end

   |ASTD_possibilities.Synch [] -> state

   |_ -> failwith "unappropriate synchronise"
;;





let execute astd state event  =
               let (list_poss,f)=ASTD_possibilities.possible_evolutions astd  state event []
                 in begin 
            (*             print_endline "=========Start Possibilities" ; 
                          ASTD_possibilities.print list_poss "" ; 
                          print_endline "=========End Possibilities" ;  *)
                          
                        if (ASTD_possibilities.possible list_poss)
                             then execute_possibilities astd state list_poss [] 
                             else failwith "Not Possible"
                    end
;;






let rec exec_list astd state event_list = match event_list with
   | h::t -> print_newline();
             print_endline ("=============================================================");
             print_endline ("=============================================================");
             print_endline ("=============================================================");
             print_endline ("Execution of : "^(ASTD_label.string_of(ASTD_event.get_label h)));
             let s2= execute astd state h
             in print_newline () ;
                print_endline "=========NewState" ; 
                print_newline () ;
                ASTD_state.print s2 "";
                print_newline () ;
                print_endline "=========End NewState" ;       
                exec_list astd s2 t
   | [] -> print_newline () ;
;;      

