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

let int_of n = match n with
  |Integer a -> a
  |_-> failwith "should be used with a constant integer"
;;

let string_of n = match n with
  |Symbol a -> a
  |_-> failwith "should be used with a constant symbol"
;;

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

(**                                                                                      *)
type value = | Range of (int * int)
             | Val of t

type domain = value list


let val_to_int a = match a with
  |Val(Integer v)-> v
  |_-> failwith "should be used with a number"
;;

let val_to_string a = match a with
  |Val(Symbol s) ->s
  |_-> failwith "should be used with a string"


let range_of a b = if a<b then Range(a,b)
                          else failwith "in a range(a,b), we sould have a<b"
;;

let value_of a= match a with
     |Symbol b -> Val a
     |Integer b -> Val a
;;


let rec insert e l = match l with
   |(Val c)::t -> if e < (Val c) then e::l
                                 else if (e = Val c) then l
                                                     else (Val c)::(insert e t) 
   |(Range(a,b))::t -> let v=val_to_int e 
                       in if (v < a) then e::l
                                     else if (v > b) then (Range(a,b))::(insert e t)
                                                     else l
   |[]->[e]
;;


let rec insert_range l fst lst = match l with
   |(Val c)::t -> let e=Val c
                  in let v=val_to_int e 
                  in if (v < fst) then e::(insert_range t fst lst)
                                  else if (v > lst) then (Range(fst,lst))::l
                                                    else  insert_range t fst lst
   |(Range(a,b))::t -> if lst < a then (Range(fst,lst))::l
                                  else if fst > b then (Range(a,b))::(insert_range t fst lst)
                                                  else (Range((min fst a ),(max lst b)))::t
   |[]->[Range(fst,lst)]
;;







let rec remove e l = match l with
   |(Val c)::t -> if ((val_to_int e)<(val_to_int (Val c))) 
                       then l
                       else if ((val_to_int e)=(val_to_int (Val c))) 
                               then t
                               else (Val c)::(remove e t)
   |( Range(c,d) )::t -> let v = (val_to_int e)
                          in if v<c 
                                then l
                                else if v>d 
                                        then (Range (c,d))::(remove e t)
                                        else if v=c 
                                                then if (c+1) = d
                                                        then (Val (Integer d))::t
                                                        else (Range (c+1,d))::t
                                                else if v=c+1 
                                                        then if c+2 =d 
                                                                then (Val(Integer c))::((Val(Integer d))::t)
                                                                else (Val(Integer c))::((Range(c+2,d)) ::t)
                                                        else if v = d-1
                                                                then (Range(c,v))::((Val(Integer d))::t)
                                                                else if v=d
                                                                        then (Range(c,v))::t
                                                                        else (Range(c,v-1))::((Range(v+1,d))::t)
   |[] -> []
;;


let rec remove_range l fst snd = match l with 
   |(Val c)::t -> let v= (val_to_int (Val c))
                  in if v<fst
                        then (Val c)::(remove_range t fst snd)
                        else if v > snd then t
                                        else (remove_range t fst snd)
   |( Range(c,d) )::t -> if c < fst 
                             then if d > snd 
                                     then if fst = (c +1)
                                             then if snd =(d -1)
                                                     then (Val(Integer c))::( (Val(Integer d)) ::t)
                                                     else (Val(Integer c))::( (Range(snd+1,d)) ::t)
                                             else if snd =(d -1)
                                                     then (Range(c,fst-1))::( (Val(Integer d)) ::t)
                                                     else (Range(c,fst-1))::( (Range(snd+1,d)) ::t)
                                     else if fst=(c+1)
                                             then (Val(Integer c))::(remove_range t fst snd)
                                             else (Range(c,fst-1))::(remove_range t fst snd)
                             else if c > snd
                                     then l
                                     else if d > snd
                                             then if snd =(d-1)
                                                     then (Val(Integer d))::t
                                                     else (Range(snd+1,d))::t
                                             else remove_range t fst snd
   |[] -> []
;;



let rec fusion a b = match a with 
   |(Val c)::t -> fusion t (insert (Val c) b)
   |( Range(c,d) )::t -> fusion t (insert_range b c d)
   |[] -> b
;;

let rec remove_domain_from a b = match b with 
   |(Val c)::t -> remove_domain_from (remove (Val c) a) t
   |( Range(c,d) )::t -> remove_domain_from (remove_range a c d) t
   |[] -> a
;;



let order a = match a with
     |a::t -> fusion t [a]
     |[] -> []
;;

let rec is_included a list_val = match list_val with
    |(Val (Integer b))::t -> if (int_of a)=b then true 
                                    else if ((int_of a)>(b)) then is_included a t
                                                             else false
    |(Val (Symbol b))::t -> if (string_of a)=b then true 
                                   else if ((string_of a)>(b)) then is_included a t
                                                               else false
    |(Range(b,c))::t -> let v=int_of a in if v<b then false
                                                 else if v>c then is_included a t
                                                             else true

    |[] -> false
;;






