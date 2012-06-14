(** ASTD possibilities module *)

type t = Possibility of (ASTD_state.t * ASTD_arrow.t) | Mult of t list | Synch of t list
;;





(** Constructors *)

val create_possibilities : ASTD_environment.t -> ASTD_event.t -> ASTD_arrow.t list ->  t

val is_mult : t -> bool
val is_synch : t -> bool

val cons_mult : t -> t -> t
val cons_synch : t -> t -> t
val cons : t -> t -> t

val clear_cons : t -> t -> t


val find_m : t -> t -> bool




val get_state_data : t -> ASTD_state.t




(** Functions *)
val complete_possibilities : ASTD_state.t -> t -> t

val complete_synch_poss : t -> t

val complete_synch_side : bool -> t -> t

val complete_single_possibilities : ASTD_state.t -> t -> t



val choice_is : 'a list -> int -> 'a

val choose_next : 'a list -> 'a


val possible : t -> bool 

val never_empty : t -> bool 

val no_possibilities : t -> bool

val possible_evolutions : ASTD_astd.t -> ASTD_state.t -> ASTD_event.t -> ASTD_environment.t -> (t * bool) 

val q_poss : ASTD_astd.t -> ASTD_event.t -> ASTD_variable.t -> ((ASTD_term.t * ASTD_state.t) list) -> ASTD_term.t list -> ASTD_environment.t -> bool -> (t*bool)

val print : t -> string -> unit


       
