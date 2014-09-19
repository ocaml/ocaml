let _ = List.fold_left (fun acc x -> acc + x) [1;2;3] 0
(* above, last two arguments of fold_left were swapped *)
