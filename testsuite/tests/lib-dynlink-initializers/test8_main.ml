(* TEST

include dynlink

readonly_files = "test8_plugin_a.ml test8_plugin_b.ml test8_plugin_b.mli"

libraries = ""

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
module = "test8_main.ml"
*** ocamlc.byte
module = "test8_plugin_b.mli"
*** ocamlc.byte
module = "test8_plugin_a.ml"
*** ocamlc.byte
module = "test8_plugin_b.ml"
*** ocamlc.byte
program = "test8_plugin.cma"
flags = "-a"
all_modules = "test8_plugin_a.cmo test8_plugin_b.cmo"
*** ocamlc.byte
program = "${test_build_directory}/test8.byte"
libraries = "dynlink"
all_modules = "test8_main.cmo"
**** run

** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
module = "test8_main.ml"
**** ocamlopt.byte
module = "test8_plugin_b.mli"
**** ocamlopt.byte
module = "test8_plugin_a.ml"
**** ocamlopt.byte
module = "test8_plugin_b.ml"
**** ocamlopt.byte
program = "test8_plugin.cmxs"
flags = "-shared"
all_modules = "test8_plugin_a.cmx test8_plugin_b.cmx"
**** ocamlopt.byte
program = "${test_build_directory}/test8.exe"
libraries = "dynlink"
all_modules = "test8_main.cmx"
***** run
*)

(* Check that modules of a shared library can have interface-only
   dependencies to later modules in the same shared library. *)

let () =
  if Dynlink.is_native then begin
    Dynlink.loadfile "test8_plugin.cmxs"
  end else begin
    Dynlink.loadfile "test8_plugin.cma"
  end
