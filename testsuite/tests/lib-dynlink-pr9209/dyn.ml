(* TEST

include dynlink
readonly_files = "lib.ml lib2.ml test.c"
ld_library_path += "${test_build_directory}"

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
compile_only = "true"
all_modules = "lib.ml lib2.ml test.c dyn.ml"
**** ocamlmklib
program = "lib"
modules = "lib.cmo test.${objext}"
compile_only = "false"
***** ocamlc.byte
program = "lib2.cma"
libraries = ""
all_modules = "lib2.cmo"
compile_only = "false"
flags = "-a"
****** ocamlc.byte
libraries += "dynlink"
program = "${test_build_directory}/main.exe"
all_modules = "dyn.cmo"
flags = ""
******* run
output = "main.output"
******** check-program-output

** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
compile_only = "true"
all_modules = "lib.ml lib2.ml test.c dyn.ml"
***** ocamlmklib
program = "test"
modules = "test.${objext}"
compile_only = "false"
****** ocamlopt.byte
program = "lib.cmxs"
libraries = ""
flags = "-shared -cclib -L. -cclib -ltest"
all_modules = "lib.cmx"
compile_only = "false"
******* ocamlopt.byte
program = "lib2.cmxs"
all_modules = "lib2.cmx"
compile_only = "false"
flags = "-shared"
******** ocamlopt.byte
libraries += "dynlink"
program = "${test_build_directory}/main.exe"
all_modules = "dyn.cmx"
flags = ""
********* run
output = "main.output"
********** check-program-output
*)
let () =
  Dynlink.allow_unsafe_modules true;
  Dynlink.adapt_filename "lib.cma" |> Dynlink.loadfile;
  Dynlink.adapt_filename "lib2.cma" |> Dynlink.loadfile
