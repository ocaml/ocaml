type t = int
let greeting = "Hello"

type 'a u = 'a CamlinternalLazy.t
exception Undefined = CamlinternalLazy.Undefined
external force : 'a u -> 'a = "%lazy_force"

let map f x =
  lazy (f (force x))
