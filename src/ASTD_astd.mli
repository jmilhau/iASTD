(** ASTD astd module*)      

     
type astd_name = string;;

type path=string list

type dependancy_path = Dependant of (ASTD_variable.t*(ASTD_variable.t list * ASTD_variable.t * path)*(ASTD_label.t list)*(dependancy_path list))
		| Direct of ASTD_variable.t



(** The type {!ASTD_astd.t} represents the structure of an ASTD. *)
type t = Automata of astd_name * t list * ASTD_arrow.t list * astd_name list * astd_name
(**The automata structure, represented by the name of the astd, a list of sub-astd, a list of arrows, the list of the names of final astds and the name of the initial one*)

    | Sequence of  astd_name * t * t
(**The sequence structure, represented by the name of the astd, the left sub astd and the right one*)

    | Choice of astd_name * t * t 
(**The choice structure, represented by the name of the astd, the first sub astd and the second one*)

    | Kleene of astd_name * t
(**The kleene closure structure, represented by the name of the astd and the sub astd*)

    | Synchronisation of astd_name * ASTD_transition.t list * t * t
(**The synchronisation structure, represented by the name of the astd, the list of synchronized transitions and the two sub astd of the structure*)

    | QChoice of astd_name * ASTD_variable.t * ASTD_constant.domain * t * ((ASTD_label.t list)*(ASTD_variable.t list)) list
(**The quantified choice structure, represented by the name of the astd, the quantified variable, her domain of value, the sub astd and a list obtained by static analysis containing producers for the indirect kappa optimisation*)

    | QSynchronisation of astd_name * ASTD_variable.t * ASTD_constant.domain * ASTD_transition.t list * t 
									* ((ASTD_label.t)*(ASTD_variable.t list)*(dependancy_path list)) list
(**The quantified synchronisation structure, represented by the name of the astd, the quantified variable, her domain of value, the list of synchronized transitions, the sub astd, and a list of users obtained by static analysis for the indirect kappa optimisation.*)

    | Guard of astd_name * ASTD_predicate.t list * t
(**The guard structure, represented by the name of the astd, a list of predicates and the sub astd *)

    | Call of astd_name * astd_name * (ASTD_variable.t * ASTD_term.t) list 
(**The call structure, with the name of the astd, the name of the called astd and a vector containing variables and their assigned value*)

    | Elem of astd_name
(**The base elem ASTD*)




(** {3 Constructor} *)


val give_name : unit -> astd_name
val automata_of : astd_name -> t list -> ASTD_arrow.t list -> astd_name list -> astd_name -> t
val sequence_of : astd_name -> t -> t -> t
val choice_of : astd_name -> t -> t -> t
val kleene_of : astd_name -> t -> t
val synchronisation_of : astd_name -> ASTD_transition.t list -> t -> t -> t
val qchoice_of : astd_name -> ASTD_variable.t -> ASTD_constant.domain -> t -> ((ASTD_label.t list)*(ASTD_variable.t list)) list  ->t
val qsynchronisation_of : astd_name -> ASTD_variable.t -> ASTD_constant.domain -> ASTD_transition.t list -> t  ->
                                            				((ASTD_label.t)*(ASTD_variable.t list)*(dependancy_path list)) list -> t
val guard_of : astd_name -> ASTD_predicate.t list -> t -> t
val call_of : astd_name -> astd_name -> (ASTD_variable.t *ASTD_term.t) list -> t
val elem_of : astd_name -> t



(** {3 Accessors} *)



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
val get_qvalues_s : t -> ASTD_constant.domain
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
val get_data_qchoice :t -> (astd_name * ASTD_variable.t * ASTD_constant.domain * t * ((ASTD_label.t list)*(ASTD_variable.t list)) list )
val get_data_qsynchronisation :
               t -> (astd_name * ASTD_variable.t * ASTD_constant.domain * ASTD_transition.t list * t 
							* ((ASTD_label.t)*(ASTD_variable.t list)*(dependancy_path list)) list)
val get_data_call : t -> (astd_name * astd_name * ((ASTD_variable.t *ASTD_term.t) list))





(** {3 Manipulation Functions} *)

val rename_astd: t -> astd_name -> t
val is_astd_final_in_automata : t -> astd_name -> bool
val is_elem : t-> bool
val is_synchro : t ->bool
val is_qsynchro : t ->bool




(** {3 Main Functions} *)

(**Finds the right sub astd from a list using his name *)
val find_subastd : astd_name -> t list -> t 

(**Registers every arrows in the ASTD. It also registers the list of transitions associated to each quantified astd.*)
val remember_transitions : t -> unit






(** {3 Registration of astd} *)

(** _ASTD_astd_table_ stores astd, using their name. *)

val register : t -> unit
val get_astd : astd_name -> t 
val global_save_astd : t-> unit




(** {3 Printer} *)

val print : t->string-> unit


