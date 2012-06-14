(*** PREDICATES ***)

let isGreater = function 
    | n::m::[] -> n > m
    | _ -> failwith "isGreater : bad arity"

let isZero = function 
    | n::[] -> n = ASTD_constant.of_int 0
    | _ -> failwith "isGreater : bad arity"

let isGreaterThanZero = function
    | n::[] -> n > ASTD_constant.of_int 0
    | _ -> failwith "isGreater : bad arity"


let isGreaterThanOne = function
    | n::[] -> begin 
               (n > (ASTD_constant.of_int 1))
               end
    | _ -> failwith "isGreater : bad arity"

let none = function
    | [] -> true
    | _-> failwith "none as no parameters"
;;

(*** REGISTRATION ***)

ASTD_predicate.register "isGreater" isGreater ;;
ASTD_predicate.register "isZero" isZero ;;
ASTD_predicate.register "isGreaterThanZero" isGreaterThanZero ;;
ASTD_predicate.register "isGreaterThanOne" isGreaterThanOne ;;
ASTD_predicate.register "none" none;;
