
type t = Transition of ASTD_label.t * ASTD_term.params 



let transition label consts = Transition (label,consts)
;;

let get_label a = let Transition(label,consts)=a in label
;;

let rec is_included label trans_list = match trans_list with
    |a::q -> if (label = get_label a)
                then true
                else is_included label q
    |[] -> false
;;

