(* TEST

include dynlink

files = "test2_plugin.ml test2_second_plugin.ml"

libraries = ""

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
module = "test2_main.ml"
*** ocamlc.byte
module = "test2_plugin.ml"
*** ocamlc.byte
module = "test2_second_plugin.ml"
*** ocamlc.byte
program = "${test_build_directory}/test2.byte"
libraries = "dynlink"
all_modules = "test2_main.cmo"
**** run

** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
module = "test2_main.ml"
**** ocamlopt.byte
program = "test2_plugin.cmxs"
flags = "-shared"
all_modules = "test2_plugin.ml"
**** ocamlopt.byte
program = "test2_second_plugin.cmxs"
flags = "-shared"
all_modules = "test2_second_plugin.ml"
**** ocamlopt.byte
program = "${test_build_directory}/test2.exe"
libraries = "dynlink"
all_modules = "test2_main.cmx"
***** run
*)

(* Check that a module in a loaded shared library whose initializer has not
   executed completely cannot be depended upon by another shared library being
   loaded. *)

let () =
  if Dynlink.is_native then
    Dynlink.loadfile "test2_plugin.cmxs"
  else
    Dynlink.loadfile "test2_plugin.cmo"
