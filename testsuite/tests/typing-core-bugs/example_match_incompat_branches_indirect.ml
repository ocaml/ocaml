let test = function
  | x, y when x = y -> 0  (* intended to write: x,[y] when x = y *)
  | 0, _ -> 1
  | _, [] -> 2
  | _, _ -> 3
