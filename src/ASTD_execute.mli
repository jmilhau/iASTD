(** ASTD execute module *)

(**The path is a list of terms indicating which state to modify, every time the execution meet a synchronisation*)
type path = ASTD_term.t list

type chosen_possibility =ASTD_state.t
type state_to_modify=ASTD_state.t
type modified_state=ASTD_state.t




(** {3 Manipulation Functions} *)

val synchronize : ASTD_astd.t -> ASTD_state.t -> ASTD_possibilities.t -> path -> ASTD_state.t
val apply_local : ASTD_astd.t -> ASTD_arrow.t -> chosen_possibility -> state_to_modify -> path -> ASTD_environment.t 
                                                                                                        -> modified_state
val apply_tsub : ASTD_astd.t -> ASTD_arrow.t -> chosen_possibility -> state_to_modify -> path -> ASTD_environment.t 
                                                                                                        -> modified_state
val apply_fsub : ASTD_astd.t -> ASTD_arrow.t -> chosen_possibility -> state_to_modify -> path -> ASTD_environment.t 
                                                                                                        -> modified_state




(** {3 Main Functions} *)

val execute : ASTD_astd.t -> state_to_modify -> ASTD_event.t -> modified_state
(**Calculates the possible executions then use execute_possibilities to return the modified state*)

val execute_possibilities : ASTD_astd.t -> ASTD_state.t -> ASTD_possibilities.t -> path -> ASTD_state.t 
(**Chooses the right possibilities to execute, then use apply to return the modified state*)

val apply : ASTD_astd.t -> ASTD_arrow.t -> state_to_modify -> chosen_possibility -> path -> ASTD_state.t
(**Returns the mofied state using the associated astd structure,the arrow to apply, the previous state, the chosen possibility, and the path*)

val exec_sequence : ASTD_astd.t -> state_to_modify -> ASTD_event.t list -> unit
(**Executes a list of events on a state, printing the state at each step*)





(** {3 Conversion in string} *)

val string_of_path : path -> string
