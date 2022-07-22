(* TEST
readonly_files = "a.ml b.ml"
ocamldebug_script = "${test_source_directory}/input_script"
* debugger
** shared-libraries
*** setup-ocamlc.byte-build-env
**** ocamlc.byte
module = "a.ml"
flags = "-g -for-pack foo"
***** ocamlc.byte
module = ""
all_modules = "a.cmo"
program = "foo.cmo"
flags = "-g -pack"
****** ocamlc.byte
module = "b.ml"
flags = " -g "
******* ocamlc.byte
module = ""
flags = " -g "
all_modules = "foo.cmo b.cmo"
program = "${test_build_directory}/noev.exe"
******** check-ocamlc.byte-output
********* ocamldebug
********** check-program-output
*)

(* This file only contains the specification of how to run the test *)
