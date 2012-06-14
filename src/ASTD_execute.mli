(** ASTD execute module *)
type path = ASTD_term.t list



(** Manipulation Functions *)

val synchronize : ASTD_astd.t -> ASTD_state.t -> ASTD_possibilities.t -> path -> ASTD_state.t
val apply_local : ASTD_astd.t -> ASTD_arrow.t -> ASTD_state.t -> ASTD_state.t -> path -> ASTD_environment.t -> ASTD_state.t 
val apply_tsub : ASTD_astd.t -> ASTD_arrow.t -> ASTD_state.t -> ASTD_state.t -> path -> ASTD_environment.t -> ASTD_state.t 
val apply_fsub : ASTD_astd.t -> ASTD_arrow.t -> ASTD_state.t -> ASTD_state.t -> path -> ASTD_environment.t -> ASTD_state.t 

(** Main Functions *)

val execute : ASTD_astd.t -> ASTD_state.t -> ASTD_event.t -> ASTD_state.t 

val execute_possibilities : ASTD_astd.t -> ASTD_state.t -> ASTD_possibilities.t -> path -> ASTD_state.t 

val apply : ASTD_astd.t -> ASTD_arrow.t -> ASTD_state.t -> ASTD_state.t -> path -> ASTD_state.t

val exec_sequence : ASTD_astd.t -> ASTD_state.t -> ASTD_event.t list -> unit


(** Conversion in string *)

val string_of_list : path -> string
