type path = ASTD_term.t list


val string_of_list : path -> string


val execute : ASTD_astd.t -> ASTD_state.t -> ASTD_event.t -> ASTD_state.t 

val execute_possibilities : ASTD_astd.t -> ASTD_state.t -> ASTD_possibilities.t -> path -> ASTD_state.t 

val synchronize : ASTD_astd.t -> ASTD_state.t -> ASTD_possibilities.t -> path -> ASTD_state.t

val apply : ASTD_astd.t -> ASTD_arrow.t -> ASTD_state.t -> ASTD_state.t -> path -> ASTD_state.t

val apply_local : ASTD_astd.t -> ASTD_arrow.t -> ASTD_state.t -> ASTD_state.t -> path -> ASTD_environment.t -> ASTD_state.t 
val apply_tsub : ASTD_astd.t -> ASTD_arrow.t -> ASTD_state.t -> ASTD_state.t -> path -> ASTD_environment.t -> ASTD_state.t 
val apply_fsub : ASTD_astd.t -> ASTD_arrow.t -> ASTD_state.t -> ASTD_state.t -> path -> ASTD_environment.t -> ASTD_state.t 


val apply_local_qsynchro_s : ASTD_astd.t -> ASTD_arrow.t -> (ASTD_term.t*ASTD_state.t) list -> ASTD_state.t -> path ->  
                                                                      ASTD_environment.t -> (ASTD_term.t*ASTD_state.t) list

val apply_fsub_qsynchro_s : ASTD_astd.t -> ASTD_arrow.t -> (ASTD_term.t*ASTD_state.t) list -> ASTD_state.t -> path ->   
                                                                      ASTD_environment.t -> (ASTD_term.t*ASTD_state.t) list

val apply_tsub_qsynchro_s : ASTD_astd.t -> ASTD_arrow.t -> (ASTD_term.t*ASTD_state.t) list -> ASTD_state.t -> path ->   
                                                                      ASTD_environment.t -> (ASTD_term.t*ASTD_state.t) list

val exec_list : ASTD_astd.t -> ASTD_state.t -> ASTD_event.t list -> unit
