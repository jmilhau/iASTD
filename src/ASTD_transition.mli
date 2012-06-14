(** ASTD event module *)

type t = Transition of ASTD_label.t * ASTD_term.params 




(** {Constructors} *)

val transition : ASTD_label.t -> ASTD_term.params -> t

val get_label : t -> ASTD_label.t

val get_params : t -> ASTD_term.params

val is_included : ASTD_label.t -> t list -> bool


