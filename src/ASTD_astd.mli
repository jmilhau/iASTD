(** ASTD astd module*)           
type astd_name = string;;
type label = string;;





type t = Automata of astd_name * t list * ASTD_arrow.t list * astd_name list * astd_name
       | Sequence of  astd_name * t * t
       | Choice of astd_name * t * t 
       | Kleene of astd_name * t
       | Synchronisation of astd_name * ASTD_transition.t list * t * t
       | QChoice of astd_name * ASTD_variable.t * ASTD_constant.domain * t
       | QSynchronisation of astd_name * ASTD_variable.t * ASTD_term.t list * ASTD_transition.t list * t 
       | Guard of astd_name * ASTD_predicate.t list * t
       | Call of astd_name *astd_name * (ASTD_variable.t *ASTD_term.t) list 
       | Elem of astd_name
;; 



(** {Constructor} *)


val give_name : unit -> astd_name

val automata_of : astd_name -> t list -> ASTD_arrow.t list -> astd_name list -> astd_name -> t

val sequence_of : astd_name -> t -> t -> t

val choice_of : astd_name -> t -> t -> t

val kleene_of : astd_name -> t -> t

val synchronisation_of : astd_name -> ASTD_transition.t list -> t -> t -> t

val qchoice_of : astd_name -> ASTD_variable.t -> ASTD_constant.domain -> t -> t

val qsynchronisation_of : astd_name -> ASTD_variable.t -> ASTD_term.t list -> ASTD_transition.t list -> t -> t

val guard_of : astd_name -> ASTD_predicate.t list -> t -> t

val call_of : astd_name -> astd_name -> (ASTD_variable.t *ASTD_term.t) list -> t

val elem_of : astd_name -> t



(** {Accessors} *)



val get_name: t -> astd_name
val get_sub : t -> t list
val get_arrows : t -> ASTD_arrow.t list
val get_final : t -> astd_name list
val get_init : t -> astd_name
val get_seq_l : t -> t
val get_seq_r : t -> t
val get_choice1 : t -> t
val get_choice2 : t -> t
val get_astd_kleene : t -> t
val get_trans_synchronised : t -> ASTD_transition.t list
val get_synchro_astd1 : t -> t
val get_synchro_astd2 : t -> t
val get_qvar : t -> ASTD_variable.t
val get_qvalues_c : t -> ASTD_constant.domain
val get_qvalues_s : t -> ASTD_term.t list
val get_qastd : t -> t
val get_guard_pred : t -> ASTD_predicate.t list
val get_guard_astd : t -> t
val get_called_name : t -> astd_name
val get_called_values : t -> (ASTD_variable.t *ASTD_term.t) list




val get_data_automata :t -> (astd_name * t list * ASTD_arrow.t list * astd_name list * astd_name)

val get_data_sequence :t -> (astd_name * t * t )

val get_data_choice :t -> (astd_name * t * t )

val get_data_kleene :t -> (astd_name * t )

val get_data_synchronisation :t -> (astd_name * ASTD_transition.t list * t *t)

val get_data_guard :t -> (astd_name * ASTD_predicate.t list * t)

val get_data_qchoice :t -> (astd_name * ASTD_variable.t * ASTD_constant.domain * t)

val get_data_qsynchronisation :t -> (astd_name * ASTD_variable.t * ASTD_term.t list * ASTD_transition.t list * t)

val get_data_call : t -> (astd_name * astd_name * ((ASTD_variable.t *ASTD_term.t) list))





(** {Functions} *)
val rename_astd: t -> astd_name -> t

val is_state_final_automata : t -> astd_name -> bool

val isElem : t-> bool

val find_substate : astd_name -> t list -> t 

val remember_transitions : t -> unit




(**Hash Table :  _ASTD_astd_table_  *)

(*val _ASTD_astd_table_ :
      (astd_name, t list -> bool) Hashtbl.t*)


val register : t -> unit

val get_astd : astd_name -> t 




val global_save_astd : t-> unit

val string_of : astd_name -> string

val print : t->string-> unit
