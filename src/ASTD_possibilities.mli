(** ASTD possibilities module *)

(**The type {!ASTD_possibilities.t} represents every road to follow to reach the begining of an executable arrow*)
type t = Possibility of (ASTD_state.t * ASTD_arrow.t) 
(**Possibility is the base of the type, containing both the road to follow using a {ASTD_state.t} and the executable arrow*)

        | Mult of t list 
(**Every token of the Mult list represent one possible execution, in case of undeterminism*)

        | Synch of (ASTD_term.t*t) list
(**Every token of the Synch list should be executed, one after another*)


type astd_name = string
type called_path = astd_name list
(**List of the calls the state has been through*)



(** {3 Constructors} *)



val create_possibilities : ASTD_environment.t -> ASTD_event.t -> ASTD_arrow.t list ->  t
(**Use the environment and the event to evaluate a list of arrows, and returns all the possible executions*)





(** {3 Accessors and manipulation functions} *)


val get_state_data : t -> ASTD_state.t
val is_mult : t -> bool
val is_synch : t -> bool
val possible : t -> bool 
val never_empty : t -> bool 
val no_possibilities : t -> bool






(** {3 Functions to face undeterminism} *)

val choice_is : 'a list -> int -> 'a
val choose_next : 'a list -> 'a






(** {3 Main Functions} *)

val complete_possibilities : ASTD_state.t -> t -> t
(**From a state and a list of executable possibilities from the sub state, it returns the list of possibilities executable from this state*)

val possible_evolutions : ASTD_astd.t -> ASTD_state.t -> ASTD_event.t -> ASTD_environment.t -> called_path ->(t * bool) 
(**Returns a couple with the possibilities and a boolean indicating if the current state is final *)

val clear_concat : t -> t -> t
(**"Smart" concatenation of two list of possibilities, allowing to remove empty lists of possibilities*)




(** {3 Printers} *)

val print : t -> ASTD_astd.t -> string -> ASTD_term.t list ->unit

val print_possibility : ASTD_state.t -> ASTD_astd.t -> string -> ASTD_term.t list->unit
       
