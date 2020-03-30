(* TEST
files = "myppx.ml"
include ocamlcommon
* setup-ocamlc.byte-build-env
** ocamlc.byte
program = "${test_build_directory}/myppx.exe"
all_modules = "myppx.ml"
*** ocamlc.byte
module = "test.ml"
flags = "-thread -ppx ${program}"
**** ocamlc.byte
module = "test.ml"
flags = "-vmthread -ppx ${program}"
***** check-ocamlc.byte-output
*)

(* empty *)
