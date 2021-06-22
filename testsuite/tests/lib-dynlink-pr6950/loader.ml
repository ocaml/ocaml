(* TEST

include dynlink
libraries = ""
readonly_files = "config.ml b.ml"

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
program = "plugin.cma"
flags = "-a"
all_modules = "config.ml b.ml"
*** ocamlc.byte
program = "${test_build_directory}/loader.byte"
flags = "-linkall"
include ocamlcommon
libraries += "dynlink"
all_modules = "loader.ml"
**** run
arguments = "plugin.cma"
exit_status = "2"
***** check-program-output
reference = "${test_source_directory}/byte.reference"

** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
program = "plugin.cmxs"
flags = "-shared"
all_modules = "config.ml b.ml"
**** ocamlopt.byte
program = "${test_build_directory}/loader.exe"
flags = "-linkall"
include ocamlcommon
libraries += "dynlink"
all_modules = "loader.ml"
***** run
arguments = "plugin.cmxs"
exit_status = "2"
****** check-program-output
reference = "${test_source_directory}/native.reference"
*)
let () =
  try
    Dynlink.loadfile Sys.argv.(1)
  with
  | Dynlink.Error (Dynlink.Module_already_loaded "Config") -> exit 2
  | _ -> exit 1
