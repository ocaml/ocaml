(* TEST

include dynlink

readonly_files = "test5_plugin_a.ml test5_plugin_b.ml test5_second_plugin.ml"

libraries = ""

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
module = "test5_main.ml"
*** ocamlc.byte
module = "test5_plugin_a.ml"
*** ocamlc.byte
module = "test5_plugin_b.ml"
*** ocamlc.byte
module = "test5_second_plugin.ml"
*** ocamlc.byte
program = "test5_plugin.cma"
flags = "-a"
all_modules = "test5_plugin_a.cmo test5_plugin_b.cmo"
*** ocamlc.byte
program = "${test_build_directory}/test5.byte"
libraries = "dynlink"
all_modules = "test5_main.cmo"
**** run

** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
module = "test5_main.ml"
**** ocamlopt.byte
module = "test5_plugin_a.ml"
**** ocamlopt.byte
module = "test5_plugin_b.ml"
**** ocamlopt.byte
program = "test5_plugin.cmxs"
flags = "-shared"
all_modules = "test5_plugin_a.cmx test5_plugin_b.cmx"
**** ocamlopt.byte
program = "test5_second_plugin.cmxs"
flags = "-shared"
all_modules = "test5_second_plugin.ml"
**** ocamlopt.byte
program = "${test_build_directory}/test5.exe"
libraries = "dynlink"
all_modules = "test5_main.cmx"
***** run
*)

(* Check that when one shared library loads another shared library then
   modules of the second shared library can refer to modules of the
   first shared library, as long as they have already been loaded. *)

let () =
  if Dynlink.is_native then
    Dynlink.loadfile "test5_plugin.cmxs"
  else
    Dynlink.loadfile "test5_plugin.cma"
