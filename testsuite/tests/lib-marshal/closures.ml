(* TEST

files = "closures1.ml closures2.ml"

* setup-ocamlc.byte-build-env
** ocamlc.byte
all_modules = "closures1.ml"
program = "${test_build_directory}/closures1.exe"
*** run
arguments = "save closures.data"
**** run
arguments = "load closures.data"
***** check-program-output
reference = "${test_source_directory}/closures1.reference"
****** ocamlc.byte
all_modules = "closures2.ml"
program = "${test_build_directory}/closures2.exe"
******* run
arguments = "load closures.data"
******** check-program-output
reference = "${test_source_directory}/closures2.reference"

* setup-ocamlopt.byte-build-env
script = "sh ${test_source_directory}/closures-supported.sh"
** script
*** ocamlopt.byte
all_modules = "closures1.ml"
program = "${test_build_directory}/closures1.exe"
**** run
arguments = "save closures.data"
***** run
arguments = "load closures.data"
****** check-program-output
reference = "${test_source_directory}/closures1.reference"
******* ocamlopt.byte
all_modules = "closures2.ml"
program = "${test_build_directory}/closures2.exe"
******** run
arguments = "load closures.data"
********* check-program-output
reference = "${test_source_directory}/closures2.reference"
*)
