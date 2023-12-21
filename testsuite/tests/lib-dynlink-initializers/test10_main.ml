(* TEST_BELOW
(* Blank lines added here to preserve locations. *)





































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

(* TEST
 include dynlink;
 readonly_files = "test10_plugin.ml";
 flags += "-g";
 libraries = "";
 shared-libraries;
 {
   setup-ocamlc.byte-build-env;
   {
     module = "test10_main.ml";
     ocamlc.byte;
   }{
     module = "test10_plugin.ml";
     ocamlc.byte;
   }{
     program = "${test_build_directory}/test10.byte";
     libraries = "dynlink";
     all_modules = "test10_main.cmo";
     ocamlc.byte;
     run;
     reference = "${test_source_directory}/test10_main.byte.reference";
     check-program-output;
   }
 }{
   no-flambda;
   native-dynlink;
   setup-ocamlopt.byte-build-env;
   {
     module = "test10_main.ml";
     ocamlopt.byte;
   }{
     program = "test10_plugin.cmxs";
     flags = "-shared";
     all_modules = "test10_plugin.ml";
     ocamlopt.byte;
   }{
     program = "${test_build_directory}/test10.exe";
     libraries = "dynlink";
     all_modules = "test10_main.cmx";
     ocamlopt.byte;
     run;
     reference = "${test_source_directory}/test10_main.native.reference";
     check-program-output;
   }
 }
*)
