(* TEST
files = "unknown-file"
* setup-ocamlc.byte-build-env
** ocamlc.byte
all_modules = ""
flags = "unknown-file"
ocamlc_byte_exit_status = "2"
*** check-ocamlc.byte-output

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
all_modules = ""
flags = "unknown-file"
ocamlopt_byte_exit_status = "2"
*** check-ocamlopt.byte-output

*)

(* this file is just a test driver, the test does not contain real OCamlcode *)

