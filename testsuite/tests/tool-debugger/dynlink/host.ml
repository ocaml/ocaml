(* TEST
 include dynlink;
 readonly_files = "host.ml plugin.ml";
 libraries = "";
 flags += " -g ";
 debugger_script = "${test_source_directory}/input_script";
 debugger;
 shared-libraries;
 setup-ocamlc.byte-build-env;
 module = "host.ml";
 ocamlc.byte;
 module = "plugin.ml";
 ocamlc.byte;
 module = "";
 all_modules = "host.cmo";
 program = "${test_build_directory}/host.byte";
 libraries = "dynlink";
 ocamlc.byte;
 output = "host.output";
 run;
 {
   reference = "${test_source_directory}/host.reference";
   check-program-output;
 }{
   output = "host.debug.output";
   ocamldebug;
   reference = "${test_source_directory}/host.debug.reference";
   check-program-output;
 }
*)

let () = print_endline "hello host"; Dynlink.loadfile "plugin.cmo"
