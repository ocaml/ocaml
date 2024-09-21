type t (* 0 *)

val x (* 1 *) : t

module type S (* 3 *) = sig
  val y (* 2 *) : t
end

module M (* 4 *) : S

module type Initial (* 7 *) = sig
  module type Nested (* 6 *) = sig
    type t (* 5 *)
  end
end

module FMT (* 11 *) (X (* 10 *) : sig
  module type MT (* 9 *) = sig val x (* 8 *) : int end
end) : sig end
