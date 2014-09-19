let sum = function
  | 0, _ -> 0
  | _, [] -> 0
  | a::l -> 1 + (sum l)