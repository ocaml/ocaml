(* TEST

include dynlink

readonly_files = "test10_plugin.ml"
flags += "-g"

libraries = ""

* no-flambda
** shared-libraries
*** setup-ocamlc.byte-build-env
**** ocamlc.byte
module = "test10_main.ml"
**** ocamlc.byte
module = "test10_plugin.ml"
**** ocamlc.byte
program = "${test_build_directory}/test10.byte"
libraries = "dynlink"
all_modules = "test10_main.cmo"
***** run
****** check-program-output
reference = "${test_source_directory}/test10_main.byte.reference"

*** native-dynlink
**** setup-ocamlopt.byte-build-env
***** ocamlopt.byte
module = "test10_main.ml"
***** ocamlopt.byte
program = "test10_plugin.cmxs"
flags = "-shared"
all_modules = "test10_plugin.ml"
***** ocamlopt.byte
program = "${test_build_directory}/test10.exe"
libraries = "dynlink"
all_modules = "test10_main.cmx"
****** run
******* check-program-output
reference = "${test_source_directory}/test10_main.native.reference"
*)

(* Check that a module in the main program whose initializer has not
   executed completely cannot be depended upon by a shared library being
   loaded. *)

let () =
  Printexc.record_backtrace true;
  try
    if Dynlink.is_native then begin
      Dynlink.loadfile "test10_plugin.cmxs"
    end else begin
      Dynlink.loadfile "test10_plugin.cmo"
    end
  with
  | Dynlink.Error (Dynlink.Library's_module_initializers_failed exn) ->
      Printf.eprintf "Error: %s\n%!" (Printexc.to_string exn);
      Printexc.print_backtrace stderr
