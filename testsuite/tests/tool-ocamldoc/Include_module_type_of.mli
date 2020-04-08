(* TEST
   * ocamldoc with html
   * ocamldoc with latex
*)

(** Test [include module type of...] variants *)

module A: sig type t end
module M: sig
  (** A module M *)

  module Inner: sig type t end
  module Alias = A
  type t
end

module B: sig
  include module type of M
end

include module type of struct include M end
