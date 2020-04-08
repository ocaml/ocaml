(* TEST
* setup-ocamlc.byte-build-env
flags = "-nostdlib -nopervasives"
** ocamlc.byte
ocamlc_byte_exit_status = "2"
*** check-ocamlc.byte-output
*)

(* Same example as in tests/typing-sigsubst/sigsubst.ml, but not as an
   expect_test so we get the full error.  *)

module type Printable = sig
  type t
  val print : t -> unit
end

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type PrintableComparable = sig
  include Printable
  include Comparable with type t = t
end
