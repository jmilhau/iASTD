
val debug_on : unit -> unit

val static_analysis : ASTD_astd.t -> (ASTD_optimisation.optimisation list) * (ASTD_optimisation.dependency list) 

val register_static : ASTD_optimisation.optimisation list -> ASTD_optimisation.dependency list -> ASTD_astd.t -> ASTD_astd.t 
