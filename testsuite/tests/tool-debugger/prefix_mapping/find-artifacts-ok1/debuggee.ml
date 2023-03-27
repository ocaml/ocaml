(* TEST
ocamldebug_script = "${test_source_directory}/input_script"
* dumpenv
* debugger
** shared-libraries
*** setup-ocamlc.byte-build-env
**** script
script = "mkdir out"
***** ocamlc.byte
flags = "-g -c"
set BUILD_PATH_PREFIX_MAP="/stdlib_root=${bppm_encode ${ocamlsrcdir}}/stdlib"
BUILD_PATH_PREFIX_MAP += ":/source_root=${bppm_encode ${test_source_directory}}"
BUILD_PATH_PREFIX_MAP += ":/build_root=${bppm_encode ${test_build_directory}}"
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
BUILD_PATH_PREFIX_MAP="${bppm_encode ${ocamlsrcdir}}/stdlib=/stdlib_root"
BUILD_PATH_PREFIX_MAP += ":${bppm_encode ${test_source_directory}}=/source_root"
BUILD_PATH_PREFIX_MAP += ":${bppm_encode ${test_build_directory}}=/build_root"
********** check-program-output
*)

(* This file only contains the specification of how to run the test *)
