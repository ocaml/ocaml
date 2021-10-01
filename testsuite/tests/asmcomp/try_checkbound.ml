(* TEST *)

(* See PR#10339 *)

let access (a: string array) n =
  try
    ignore (a.(n)); -1
  with _ ->
    n

let _ =
  assert (access [||] 1 = 1)
