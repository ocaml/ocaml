let x = 2
let t = [| 1, 2, 3 |]    (* should be [;] instead of [,] *)
let _ = if t.(1) < x then -1 else 1
