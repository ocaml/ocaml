let _ = List.fold_left (fun x acc -> (x > 2) || acc) true [1;2;3]

(* swapped the parameters of the higher-order function *)
