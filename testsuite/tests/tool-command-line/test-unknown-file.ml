(* TEST

readonly_files = "unknown-file"

* setup-ocamlc.byte-build-env
compiler_output = "compiler-output.raw"
** ocamlc.byte
all_modules = ""
flags = "unknown-file"
ocamlc_byte_exit_status = "2"
*** script
script = "grep 'know what to do with unknown-file' compiler-output.raw"
output = "compiler-output"
**** check-ocamlc.byte-output
compiler_output = "compiler-output"

* setup-ocamlopt.byte-build-env
compiler_output = "compiler-output.raw"
** ocamlopt.byte
all_modules = ""
flags = "unknown-file"
ocamlopt_byte_exit_status = "2"
*** script
script = "grep 'know what to do with unknown-file' compiler-output.raw"
output = "compiler-output"
**** check-ocamlopt.byte-output
compiler_output = "compiler-output"

*)

(*
  This file is just a test driver, the test does not contain any
  real OCaml code
*)
