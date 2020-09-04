(* TEST
   modules="i8907_a.ml i8907_b.ml i8907_c.ml"
   ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

(* Erroneous signature mismatch *)
include I8907_b
