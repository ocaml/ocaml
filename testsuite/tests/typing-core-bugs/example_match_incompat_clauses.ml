let f x = 
  match x with
  | 0 -> []
  | 1 -> [1]     (* maybe meant [1.] *)
  | _ -> [2.]    (* or maybe meant [2] *)
