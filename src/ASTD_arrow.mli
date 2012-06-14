(** ASTD arrow module *)

type from_state = string
type to_state = string
type through_state = string
type from_final_state = bool



type t = Local of from_state * to_state * ASTD_transition.t * ASTD_predicate.t list * from_final_state
|From_sub of from_state * to_state * through_state * ASTD_transition.t * ASTD_predicate.t list * from_final_state
|To_sub of from_state * to_state * through_state * ASTD_transition.t * ASTD_predicate.t list * from_final_state


(*val _ASTD_arrow_table_ :
      ((from_state,ASTD_transition.t), t list -> bool) Hashtbl.t*)

(** {1 Constructor} *)

val local_arrow :from_state -> to_state-> ASTD_transition.t -> ASTD_predicate.t list -> from_final_state -> t
val fsub_arrow :from_state -> to_state -> through_state -> ASTD_transition.t -> ASTD_predicate.t list -> from_final_state ->  t
val tsub_arrow :from_state -> to_state -> through_state -> ASTD_transition.t -> ASTD_predicate.t list -> from_final_state ->  t


(** {2 Accessors} *)

val get_from : t -> from_state
val get_to : t -> to_state
val get_through : t -> through_state
val get_transition : t -> ASTD_transition.t
val get_predicates : t-> ASTD_predicate.t list
val get_from_final_state : t-> from_final_state

val get_label_transition : t -> ASTD_label.t


(** {3 Registration of predicate} *)

val register : (from_state * ASTD_label.t * from_final_state ) -> t -> unit

val register_arrow : t -> unit

val get : from_state -> ASTD_event.t -> from_final_state -> t list 

val register_transition : ASTD_transition.t -> unit

val get_transition_params : ASTD_label.t -> ASTD_term.params

(** {4 Functions} *)

val evaluate_guard : ASTD_environment.t -> ASTD_predicate.t list -> bool

val valid_arrow : ASTD_event.t -> ASTD_environment.t -> t -> bool

val may_be_the_right : ASTD_label.t -> t -> bool


