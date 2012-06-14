type path=string list

type dependancy_path = ASTD_astd.dependancy_path

(*analyse statique remplissant les listes de producteurs et consommateurs*)
val static_analysis : ASTD_astd.t->ASTD_astd.t


(*test et enregistrement de valeur pour producteur*)
val productor_action : ASTD_term.params -> ASTD_variable.t -> ASTD_event.t -> ((ASTD_label.t list)*(ASTD_variable.t list)) list 
													-> ASTD_environment.t -> unit

(*test et recherche de valeur utile pour user*)
val get_val_if_user : ASTD_variable.t -> ASTD_event.t -> ASTD_term.params -> 
				(ASTD_label.t*(ASTD_label.t list)*(dependancy_path list) ) list -> ASTD_environment.t -> (bool*ASTD_constant.t)

