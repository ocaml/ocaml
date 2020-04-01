(* TEST
modules = "file.ml"

* setup-ocamlc.byte-build-env
program = "${test_build_directory}/main.exe"
** ocamlc.byte
module = "file.ml"
*** ocamlc.byte
module = ""
program = "lib.cma"
flags = "-a"
all_modules = "file.cmo"
**** ocamlc.byte
program = "${test_build_directory}/main.exe"
all_modules = "lib.cma main.ml"
flags = ""
***** check-ocamlc.byte-output
****** run
******* check-program-output

* setup-ocamlopt.byte-build-env
program = "${test_build_directory}/main.exe"
** ocamlopt.byte
module = "file.ml"
*** ocamlopt.byte
module = ""
program = "lib.cmxa"
flags = "-a"
all_modules = "file.cmx"
**** ocamlopt.byte
program = "${test_build_directory}/main.exe"
all_modules = "lib.cmxa main.ml"
flags = ""
***** check-ocamlopt.byte-output
****** run
******* check-program-output

*)

let () =
  ignore (File.getcwd ())
