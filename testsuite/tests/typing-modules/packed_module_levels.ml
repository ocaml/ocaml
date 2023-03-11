(* TEST
   * expect
*)
type (_, _) equ = Refl : ('q, 'q) equ

module type Ty = sig type t end
type 'a modu = (module Ty with type t = 'a)

type 'q1 packed =
    P : 'q0 modu * ('q0, 'q1) equ -> 'q1 packed

(* Adds a module M to the environment where M.t equals an existential *)
let repack (type q) (x : q packed) : q modu =
  match x with
  | P (p, eq) ->
    let module M = (val p) in
    let Refl = eq in
    (module M)

[%%expect{|
type (_, _) equ = Refl : ('q, 'q) equ
module type Ty = sig type t end
type 'a modu = (module Ty with type t = 'a)
type 'q1 packed = P : 'q0 modu * ('q0, 'q1) equ -> 'q1 packed
val repack : 'q packed -> 'q modu = <fun>
|}]

(* Same, using a polymorphic function rather than an existential *)

let mkmod (type a) () : a modu =
  (module struct type t = a end)

let f (type foo) (intish : (foo, int) equ) =
  let module M = (val (mkmod () : foo modu)) in
  let Refl = intish in
  let module C : sig type t = int end = M in
  ()

[%%expect{|
val mkmod : unit -> 'a modu = <fun>
val f : ('foo, int) equ -> unit = <fun>
|}]
