(* TEST

include dynlink

readonly_files = "test3_plugin_a.ml test3_plugin_b.ml"

libraries = ""

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
module = "test3_main.ml"
*** ocamlc.byte
module = "test3_plugin_a.ml"
*** ocamlc.byte
module = "test3_plugin_b.ml"
*** ocamlc.byte
program = "test3_plugin.cma"
flags = "-a"
all_modules = "test3_plugin_a.cmo test3_plugin_b.cmo"
*** ocamlc.byte
program = "${test_build_directory}/test3.byte"
libraries = "dynlink"
all_modules = "test3_main.cmo"
**** run

** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
module = "test3_main.ml"
**** ocamlopt.byte
module = "test3_plugin_a.ml"
**** ocamlopt.byte
module = "test3_plugin_b.ml"
**** ocamlopt.byte
program = "test3_plugin.cmxs"
flags = "-shared"
all_modules = "test3_plugin_a.cmx test3_plugin_b.cmx"
**** ocamlopt.byte
program = "${test_build_directory}/test3.exe"
libraries = "dynlink"
all_modules = "test3_main.cmx"
***** run
*)

(* Check that one module in a shared library can refer to another module
   in the same shared library as long as the second module has already
   been loaded. *)

let () =
  if Dynlink.is_native then begin
    Dynlink.loadfile "test3_plugin.cmxs"
  end else begin
    Dynlink.loadfile "test3_plugin.cma"
  end
