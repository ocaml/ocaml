type t = A of int
type u = B of t
let f = function B A x -> x
