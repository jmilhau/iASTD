(** HISTORY TUPLES *)

exception DBError of string;;

val debug_on : unit -> unit

val register2db : ASTD_astd.t -> ASTD_state.t -> string -> ASTD_constant.t option  -> unit 
(* Stores an astd state in a database *)

val db2state : string -> ASTD_state.t

val initdb : unit -> unit

val deleteTuplesDB : string -> unit
