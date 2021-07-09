(* TEST

include dynlink

readonly_files = "test7_interface_only.mli test7_plugin.ml"

libraries = ""

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
module = "test7_interface_only.mli"
*** ocamlc.byte
module = "test7_main.ml"
*** ocamlc.byte
module = "test7_plugin.ml"
*** ocamlc.byte
program = "${test_build_directory}/test7.byte"
libraries = "dynlink"
all_modules = "test7_main.cmo"
**** run

** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
module = "test7_interface_only.mli"
**** ocamlopt.byte
module = "test7_main.ml"
**** ocamlopt.byte
program = "test7_plugin.cmxs"
flags = "-shared"
all_modules = "test7_plugin.ml"
**** ocamlopt.byte
program = "${test_build_directory}/test7.exe"
libraries = "dynlink"
all_modules = "test7_main.cmx"
***** run
*)

(* Check that a shared library can depend on an interface-only module
   that is also depended on by modules in the main program *)

let f (x : Test7_interface_only.t) = x + 1 [@@inline never]

let () =
  if Dynlink.is_native then
    Dynlink.loadfile "test7_plugin.cmxs"
  else
    Dynlink.loadfile "test7_plugin.cmo"
