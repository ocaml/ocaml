(* TEST

files = "unknown-file"

* skip (* setup-ocamlc.byte-build-env *)
** ocamlc.byte
all_modules = ""
flags = "unknown-file"
ocamlc_byte_exit_status = "2"
*** check-ocamlc.byte-output

* skip (* setup-ocamlopt.byte-build-env *)
** ocamlopt.byte
all_modules = ""
flags = "unknown-file"
ocamlopt_byte_exit_status = "2"
*** no-flambda
**** check-ocamlopt.byte-output
*** flambda
**** check-ocamlopt.byte-output
compiler_reference = "${test_source_directory}/test.ocamlopt.byte.flambda.reference"

*)

(* this file is just a test driver, the test does not contain real OCamlcode *)

