(* coerce needs parens around its argument *)

let d = dynamic 1 in let x = coerce d : int in ();;

