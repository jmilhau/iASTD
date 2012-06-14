type state_to_modify=ASTD_state.t
type modified_state=ASTD_state.t

type astd_name = string
type called_path = astd_name list

val debug_on : unit -> unit


(** {2 Main Functions} *)

(**Returns the initial state of an ASTD*)
val init : ASTD_astd.t -> ASTD_state.t

(**Returns true if the state is final*)
val is_final : ASTD_astd.t -> ASTD_state.t -> ASTD_environment.t->astd_name list -> string -> ( ASTD_state.t*bool)


(**With an astd, his history and a name of ASTD, it returns the right state as a result of transition. If the name is H1, it returns the shallow history, if it is H2, it returns the the deep history*)
val goto_automata : ASTD_astd.t -> astd_name -> ((astd_name * ASTD_state.t) list) -> ASTD_state.t

(**Modify the saved state of an astd in the history*)
val modify_h : ((astd_name * ASTD_state.t) list) -> astd_name -> ASTD_state.t -> ((astd_name * ASTD_state.t) list)




val execute: ASTD_state.t -> ASTD_astd.t -> ASTD_event.t ->ASTD_environment.t -> string ->(ASTD_state.t * string *ASTD_astd.t* ASTD_state.t *
								(((astd_name*ASTD_constant.t option)*ASTD_astd.t*ASTD_state.t*bool) list)* 
								(((ASTD_optimisation.dependency*(ASTD_term.t list))*ASTD_term.t*bool) list)*
								bool)


val execute_event_list: int->ASTD_state.t -> ASTD_astd.t -> ASTD_event.t list -> ASTD_state.t






(** {3 Printers} *)

val print : ASTD_state.t -> ASTD_astd.t -> string -> string-> unit
val print_h : ASTD_astd.t -> ((ASTD_state.astd_name * ASTD_state.t) list) -> string-> string-> unit
