
let rec sum = function   (* intended of type [float list -> float] *)
  | [] -> assert false
  | [a] -> a + a         (* intended [a +. a] instead of [a + a] *)
  | a::l -> a +. sum l   (* appropriate code *)
