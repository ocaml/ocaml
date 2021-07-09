(* TEST

include dynlink

readonly_files = "test2_inited_first.ml test2_plugin.ml"

libraries = ""

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
module = "test2_inited_first.ml"
*** ocamlc.byte
module = "test2_main.ml"
*** ocamlc.byte
module = "test2_plugin.ml"
*** ocamlc.byte
program = "${test_build_directory}/test2.byte"
libraries = "dynlink"
all_modules = "test2_inited_first.cmo test2_main.cmo"
**** run

** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
module = "test2_inited_first.ml"
**** ocamlopt.byte
module = "test2_main.ml"
**** ocamlopt.byte
program = "test2_plugin.cmxs"
flags = "-shared"
all_modules = "test2_plugin.ml"
**** ocamlopt.byte
program = "${test_build_directory}/test2.exe"
libraries = "dynlink"
all_modules = "test2_inited_first.cmx test2_main.cmx"
***** run
*)

(* Check that a shared library can refer to a module in the main program
   as long as that module has already been loaded. *)

let g x = Test2_inited_first.f x

let () =
  if Dynlink.is_native then begin
    Dynlink.loadfile "test2_plugin.cmxs"
  end else begin
    Dynlink.loadfile "test2_plugin.cmo"
  end
