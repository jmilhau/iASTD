
(** ASTD state module *)


type position = string
type step = Left | Right
type side = Undef | Fst | Snd
type qchoice = Val of ASTD_term.t |ChoiceNotMade
type astd_name = string

type t = Automata_s of position * ((astd_name * t) list) * t
        |Sequence_s of step * t
        |Choice_s of side * t
        |Kleene_s of bool * t
        |Synchronisation_s of t * t
        |QChoice_s of qchoice * t
        |QSynchronisation_s  of t*(ASTD_constant.domain)*((ASTD_term.t *t) list)
        |Guard_s of bool * t
        |Call_s of bool * t
        |NotDefined
        |Elem
;;



(** Constructors *)

val automata_s_of : position -> ((astd_name * t) list) -> t -> t
val sequence_s_of : step -> t -> t
val choice_s_of : side -> t -> t
val kleene_s_of : bool -> t -> t
val synchronisation_s_of : t -> t -> t
val qchoice_s_of : qchoice -> t -> t
val qsynchronisation_s_of :t->(ASTD_constant.domain)->((ASTD_term.t *t) list) -> t
val guard_s_of : bool -> t -> t
val call_s_of : bool -> t -> t
val not_defined_state :  unit -> t

val undef_choice_of : unit -> side
val fst_choice_of : unit -> side
val snd_choice_of : unit -> side

val left_sequence_of : unit -> step
val right_sequence_of : unit -> step

val qchoice_notmade_of : unit -> qchoice



(**Functions*)

val get_pos : t -> astd_name

val is_automata : t -> bool

val is_qsynchro : t -> bool

val get_data_from_qsynchro : t -> t*(ASTD_constant.domain)*((ASTD_term.t *t) list)

val get_data_automata_s : t-> (position * ((astd_name * t) list) * t)

val init : ASTD_astd.t -> t

val goto_automata : ASTD_astd.t -> astd_name -> ((astd_name * t) list) -> t

val init_history : ASTD_astd.t list -> ((astd_name * t) list) 

val modify_h : ((astd_name * t) list) -> astd_name -> t -> ((astd_name * t) list)

val get_deep : ((astd_name * t) list) -> astd_name -> t

val get_shallow : ((astd_name * t) list) -> astd_name -> astd_name


val insert : (ASTD_term.t *t) -> (ASTD_term.t *t) list -> (ASTD_term.t *t) list


val get_val : qchoice -> ASTD_term.t



val val_of : ASTD_term.t -> qchoice


val string_of_qchoice : qchoice -> string
val string_of_seq : step -> string
val string_of_choice : side -> string 



val string_of_bool : bool -> string

val print : t -> string -> unit
val print_qsynch : (ASTD_term.t *t) list -> string -> unit
val print_h : ((astd_name * t) list) -> string -> unit


