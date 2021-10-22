(* from MPR#7624 *)

val g : 'a -> 'a


(* multiple bindings *)
val n : 'a -> 'a

val o : 'a -> 'a

(* value in functor argument *)
module F (X : sig val x : int end) : sig end

module G (X : sig val x : int end) : sig end

module H (X : sig val x : int end) : sig val x : int end

module type S = sig
  module F:  sig val x : int end -> sig end
end
