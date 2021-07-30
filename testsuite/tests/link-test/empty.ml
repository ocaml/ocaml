(* TEST

* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "empty.ml"
*** ocamlc.byte
module = ""
flags = "-a"
all_modules = ""
program = "empty.cma"
**** ocamlc.byte
flags = ""
program = "${test_build_directory}/empty.byte"
all_modules = "empty.cma empty.cmo"
***** check-ocamlc.byte-output
* setup-ocamlopt.byte-build-env
** ocamlopt.byte
module = "empty.ml"
*** ocamlopt.byte
module = ""
flags = "-a"
all_modules = ""
program = "empty.cmxa"
**** ocamlopt.byte
flags = ""
program = "${test_build_directory}/empty.native"
all_modules = "empty.cmxa empty.cmx"
***** check-ocamlopt.byte-output
*)
