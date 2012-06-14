
(** ASTD constant module 
rajout de const_list_of_range et de remove_list_from remove_elem_from add_list_to  *)

type t = Integer of int | Symbol of string (* HIDDEN *)
type set_name = string

module Set_of : Set.S with type elt = t
type set = Set_of.t


type value = | Range of (int * int)
             | Val of t

type domain = value list





(** {2 Constant} *)
(** {3 Constructor of constant} *)

val of_int : int -> t

(** {3 Printers} *)

val print : t -> unit
val print_list : t list -> unit

(** {3 Conversion in string} *)

val string_of : t -> string
val string_of_list : t list -> string

(** {2 Set of constant} *)

(** {3 Constructors of constant set} *)

val empty_set : set
val constant_set_from_list : t list -> set
val constant_set_from_range : min:int -> max:int -> set
val const_list_of_range : int -> int -> t list  



(** {3 Iterator over set of constant} *)

val fold_set : (t -> 'a -> 'a) -> set -> 'a -> 'a
val iteration_over_set : (t -> unit) -> set -> unit
val for_all_constants : (t -> bool) -> set -> bool
val exists_a_constant : (t -> bool) -> set -> bool

(** {3 Other functions} *)

val add_in : t -> set -> set
val remove_from : t -> set -> set
val choose_in : set -> t 
val member : t -> set -> bool
val union : set -> set -> set
val is_empty : set -> bool

val add_list_to : t list -> t list -> t list
val remove_list_from : t list -> t list -> t list
val remove_elem_from : t -> t list -> t list
(** {3 Printers} *)

val print_set_name : set_name -> unit
val print_set : set -> unit

(** {3 Conversion in string} *)

val string_of_set_name : set_name -> set_name
val string_of_set : set -> string






(**                                                                             *)

val range_of : int -> int -> value 

val int_of : t -> int
val string_of : t -> string

val value_of : t -> value

val insert : value -> domain -> domain
val insert_range : domain -> int -> int -> domain

val remove : value -> domain -> domain
val remove_range : domain -> int -> int -> domain



val fusion : domain -> domain -> domain
val remove_domain_from : domain -> domain -> domain
val order : domain -> domain

val is_included : t -> domain -> bool


