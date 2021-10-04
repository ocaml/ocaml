(* TEST *)

type t = A | B of (int -> int)

let p = 1 + 1

let rec b = B g
and g n =
  let b' = b in
  match b' with
  | A -> n + p
  | B f -> f n
