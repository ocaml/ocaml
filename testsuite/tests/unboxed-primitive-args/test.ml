(* TEST

readonly_files = "common.mli common.ml test_common.c test_common.h"

* setup-ocamlopt.byte-build-env
** ocaml
test_file = "${test_source_directory}/gen_test.ml"
ocaml_script_as_argument = "true"
arguments = "c"
compiler_output = "stubs.c"
*** ocaml
arguments = "ml"
compiler_output = "main.ml"
**** ocamlopt.byte
all_modules = "test_common.c stubs.c common.mli common.ml main.ml"
***** run
****** check-program-output

*)
