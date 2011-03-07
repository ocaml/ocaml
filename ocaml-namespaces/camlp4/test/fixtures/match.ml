let x =
  match y with
  | A z -> z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z z 
  | B l ->
      (match l with
       | [] -> ()
       | x::xs -> p x; self xs)
  | C -> ()
in x
