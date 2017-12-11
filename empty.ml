type t = |;;

type g = A of int | B of t

let f (x:g) =
  match x with
  | A _ | B _ -> .
