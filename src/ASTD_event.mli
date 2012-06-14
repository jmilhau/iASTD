(** ASTD event module *)

type t = Event of ASTD_label.t * ASTD_constant.t list 

(** {Events} *)

(** {Constructors} *)

val event : ASTD_label.t -> ASTD_constant.t list -> t


(** {Comparison with an action} *)

val compare_action_with_event : ASTD_environment.t -> ASTD_transition.t -> t -> bool

val compare_action_with_event2 : ASTD_environment.t -> ASTD_transition.t -> t -> bool

(** {Printers} *)

val print_event : t -> unit

val print_event_ln : t -> unit

(** {Conversion in string} *)

val string_of_event : t -> string


val get_data : t -> (ASTD_label.t * ASTD_constant.t list)
val get_label : t -> ASTD_label.t
