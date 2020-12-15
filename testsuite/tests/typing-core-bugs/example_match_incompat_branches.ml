let headval = function      (* intended to be of type [int list -> float] *)
   | [] -> 0                (* intended [0.] instead of [0] *)
   | a::_ -> float_of_int a
