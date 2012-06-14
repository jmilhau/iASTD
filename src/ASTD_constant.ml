open Functions

type t = | Integer of int
         | Symbol of string

let compare_constant c1 c2 = match (c1,c2) with
    | (Integer n1, Integer n2) -> compare n1 n2
    | (Symbol s1, Symbol s2) -> compare s1 s2
    | _ -> -1

type a_constant = t
module Set_of = Set.Make( struct type t = a_constant (*** constant = t *)
                                 let compare = compare_constant
                          end) 

type set_name = string 
type set = Set_of.t

let of_int n = Integer n

let empty_set = Set_of.empty

let constant_set_from_list  = 
    let add_to_s s elt = Set_of.add elt s
    in List.fold_left add_to_s empty_set 

let constant_set_from_range ~min ~max = 
    let rec add_next n s = if n > max
                           then s
                           else add_next (n+1) (Set_of.add (of_int n) s) 
                           in
                               add_next min (empty_set:set)


let fold_set = Set_of.fold
let iteration_over_set = Set_of.iter 
let for_all_constants = Set_of.for_all 
let exists_a_constant = Set_of.exists 
let member = Set_of.mem
let choose_in = Set_of.choose
let add_in = Set_of.add
let remove_from = Set_of.remove
let union = Set_of.union
let is_empty = Set_of.is_empty

let print = function
    | Integer n -> print_int n
    | Symbol s -> print_string s

let print_list = create_print_list print

let print_set_name n = print_string n

let print_set = create_print_set iteration_over_set print

let string_of = function
    | Integer n -> string_of_int n
    | Symbol s -> s

let string_of_list = create_string_of_list string_of

let string_of_set_name n = n

let string_of_set = create_string_of_set fold_set string_of








let rec remove_elem_from elem l = match l with
|a::q -> if a=elem then q
                   else a::(remove_elem_from elem q)
|[] -> []


let rec remove_list_from l1 l2 = match l2 with
 |a::q-> remove_elem_from a (remove_list_from l1 q)
 |[] -> l1

let rec const_list_of_range min max = 
           if ( min < max ) then (Integer(min)::(const_list_of_range (min+1) max))
                            else if min = max then [Integer(min)]
                                              else failwith "min should be inferior to max"


let rec add_list_to l1 l2 = match l2 with
 |a::q-> a::(remove_elem_from a (add_list_to l1 q))
 |[] -> l1
