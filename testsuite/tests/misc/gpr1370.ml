(* TEST
*)

type t = A|B|C|D
type s =
  | G of t
  | E of t
  | H of t
  | F of (unit list * t)
  | I of t

let r = ref 0

let set x = r := x

let f x =
  match x with
  | E B | F ([()], B) -> set 0
  | E x | F ([()], x) when Sys.opaque_identity true -> set 1
  | E _ -> set 2
  | F _ -> set 3
  | G _ | H _ | I _ -> set 4
