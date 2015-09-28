let headmeasure = function 
   | [] -> 0  (* intended to be 0. *)
   | a::[] -> a
   | a::_ -> 1.
