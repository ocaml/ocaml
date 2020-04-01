(* TEST
ocamldebug_script = "${test_source_directory}/input_script"
* debugger
** shared-libraries
*** setup-ocamlc.byte-build-env
**** script
script = "mkdir out"
***** ocamlc.byte
flags = "-g -c"
all_modules = "${test_source_directory}/in/blah.ml"
program = "out/blah.cmo"
****** ocamlc.byte
program = "out/foo.cmo"
flags = "-I out -g -c"
all_modules = "${test_source_directory}/in/foo.ml"
******* ocamlc.byte
all_modules = "out/blah.cmo out/foo.cmo"
flags = " -g "
program = "debuggee.exe"
******** check-ocamlc.byte-output
********* ocamldebug
********** check-program-output
*)

(* This file only contains the specification of how to run the test *)
