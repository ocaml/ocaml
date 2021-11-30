(* TEST

flags = "-w +A"

* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "w32.mli"
*** ocamlc.byte
module = "w32.ml"
**** check-ocamlc.byte-output

*)

(* from MPR#7624 *)

let[@warning "-32"] f x = x

let g x = x

let h x = x


(* multiple bindings *)

let[@warning "-32"] i x = x
and j x = x

let k x = x
and[@warning "-32"] l x = x

let[@warning "-32"] m x = x
and n x = x

let o x = x
and[@warning "-32"] p x = x


(* recursive bindings *)

let[@warning "-32"] rec q x = x
and r x = x

let[@warning "-32"] rec s x = x
and[@warning "-39"] t x = x

let[@warning "-39"] rec u x = x
and v x = v x


(* disabled then re-enabled warnings *)

module M = struct
  [@@@warning "-32"]
  let f x = x
  let[@warning "+32"] g x = x
  let[@warning "+32"] h x = x
  and i x = x
  let j x = x
  and[@warning "+32"] k x = x
end

(* unused values in functor argument *)
module F (X : sig val x : int end) = struct end

module G (X : sig val x : int end) = X

module H (X : sig val x : int end) = X

module type S = sig
  module F:  sig val x : int end -> sig end
end
