let rev_filter f = 
  List.fold_left (fun x acc -> if f x then x::acc else acc) [] [1; 2; 3]
(* swapped the parameters of the higher-order function *)
