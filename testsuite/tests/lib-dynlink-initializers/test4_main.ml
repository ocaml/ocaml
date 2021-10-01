(* TEST

include dynlink

readonly_files = "test4_plugin_a.ml test4_plugin_b.ml"

libraries = ""

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
module = "test4_main.ml"
*** ocamlc.byte
module = "test4_plugin_b.ml"
*** ocamlc.byte
module = "test4_plugin_a.ml"
*** ocamlc.byte
program = "test4_plugin.cma"
flags = "-a"
all_modules = "test4_plugin_a.cmo test4_plugin_b.cmo"
*** ocamlc.byte
program = "${test_build_directory}/test4.byte"
libraries = "dynlink"
all_modules = "test4_main.cmo"
**** run

** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
module = "test4_main.ml"
**** ocamlopt.byte
module = "test4_plugin_b.ml"
**** ocamlopt.byte
module = "test4_plugin_a.ml"
**** ocamlopt.byte
program = "test4_plugin.cmxs"
flags = "-shared"
all_modules = "test4_plugin_a.cmx test4_plugin_b.cmx"
**** ocamlopt.byte
program = "${test_build_directory}/test4.exe"
libraries = "dynlink"
all_modules = "test4_main.cmx"
***** run
*)

(* Check that a module in a shared library cannot refer to another
   module in the same shared library if it has not yet been loaded. *)

let () =
  try
    if Dynlink.is_native then begin
      Dynlink.loadfile "test4_plugin.cmxs"
    end else begin
      Dynlink.loadfile "test4_plugin.cma"
    end;
    assert false
  with
  | Dynlink.Error (
      Dynlink.Linking_error (_,
        Dynlink.Uninitialized_global "Test4_plugin_b")) -> ()
