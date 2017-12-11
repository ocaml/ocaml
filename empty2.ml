type _ g = U : int g and empty = bool g

type t = A of empty | B of empty

let f (x:t) =
  match x with
  | B _ | A _ -> .
