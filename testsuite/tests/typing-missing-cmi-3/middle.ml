type 'a t = 'a Original.t = T
type 'a ti = 'a Original.t

let f: (module Original.T with type t = int) -> unit = fun _ -> ()
let x = (module struct type t end: Original.T )
let g: (module Original.T) -> unit = fun _ -> ()
type pack1 = (module Original.T with type t = int)
module type T = sig module M : Original.T end
type pack2 = (module T with type M.t = int)

(* Check the detection of type kind in type-directed disambiguation. *)
type r = Original.r = { x:unit }
let r = Original.r

type s = Original.s = S
let s = Original.s

(* Check expansion in gadt *)
type ('a,'b) gadt =
| G: ('a, 'a ti) gadt
