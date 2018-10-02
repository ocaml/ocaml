(* TEST
   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

(* A potential ambiguity arises because the arrow -> is used
   both in the syntax of core types and module types, and
   (furthermore) the construction "T with type t = ..." means
   that a module type can end with a core type. *)

module type T = sig type t end

(* This is OK *)
module type Foo =
  (T with type t = int) -> T

(* This is OK *)
module type Bar =
  T with type t = int -> int

(* This is not OK.
   Therefore the shift/reduce conflict on MINUSGREATER
   must be solved in favor of shifting. This is why
   MINUSGREATER is declared right-associative. *)
module type Bar =
  T with type t = int -> T
