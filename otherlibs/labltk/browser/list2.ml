(* $Id$ *)

let exclude elt:x l = List.filter l pred:((<>) x)

let rec flat_map fun:f = function
    [] -> []
  | x :: l -> f x @ flat_map fun:f l
