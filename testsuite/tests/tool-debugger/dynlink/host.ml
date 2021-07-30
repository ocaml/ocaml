(* TEST

include dynlink
readonly_files = "host.ml plugin.ml"
libraries = ""

flags += " -g "
ocamldebug_script = "${test_source_directory}/input_script"

* debugger
** shared-libraries
*** setup-ocamlc.byte-build-env
**** ocamlc.byte
module = "host.ml"
***** ocamlc.byte
module = "plugin.ml"
****** ocamlc.byte
module = ""
all_modules = "host.cmo"
program = "${test_build_directory}/host.byte"
libraries = "dynlink"

******* run
output = "host.output"
******** check-program-output
reference = "${test_source_directory}/host.reference"

******** ocamldebug
output = "host.debug.output"
********* check-program-output
reference = "${test_source_directory}/host.debug.reference"

*)

let () = print_endline "hello host"; Dynlink.loadfile "plugin.cmo"
