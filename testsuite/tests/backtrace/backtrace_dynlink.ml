(* TEST

include dynlink

readonly_files = "backtrace_dynlink_plugin.ml"

libraries = ""

* shared-libraries
** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
module = "backtrace_dynlink.ml"
flags = "-g"
**** ocamlopt.byte
program = "backtrace_dynlink_plugin.cmxs"
flags = "-shared -g"
all_modules = "backtrace_dynlink_plugin.ml"
**** ocamlopt.byte
program = "${test_build_directory}/main.exe"
libraries = "dynlink"
all_modules = "backtrace_dynlink.cmx"
***** run
ocamlrunparam += ",b=1"
****** no-flambda
******* check-program-output
****** flambda
reference = "${test_source_directory}/backtrace_dynlink.flambda.reference"
******* check-program-output
*)

(* test for backtrace and stack unwinding with dynlink. *)
(* https://github.com/ocaml-multicore/ocaml-multicore/issues/440 *)
(* https://github.com/ocaml-multicore/ocaml-multicore/pull/499 *)

let ()  =
  Dynlink.allow_unsafe_modules true;
  try
    Dynlink.loadfile "backtrace_dynlink_plugin.cmxs"
  with
  | Dynlink.Error err ->
     print_endline @@ Dynlink.error_message err;
     Printexc.print_backtrace stdout;
  | exn ->
     Printexc.to_string exn |> print_endline;
     print_endline "ERROR"
