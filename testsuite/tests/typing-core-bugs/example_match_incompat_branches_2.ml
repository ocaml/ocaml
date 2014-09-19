let rec sum = function
   | [] -> 0                (* error might be 0 instead of 0. *)
   | a::l -> a +. (sum l)   (* or it might be +. instead of + *)