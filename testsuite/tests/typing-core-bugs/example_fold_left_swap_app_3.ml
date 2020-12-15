(* intention: build the list [[1]; [2]; [3]] from the list [1; 2; 3] *)

let l = List.fold_left (fun x acc -> [x]::acc) [] [1; 2; 3] in  List.rev l

(* swapped the arguments of the higher-order function *)
