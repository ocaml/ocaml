(* TEST
 readonly_files = "unknown-file";
 {
   compiler_output = "compiler-output.raw";
   setup-ocamlc.byte-build-env;
   all_modules = "";
   flags = "unknown-file";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep 'know what to do with unknown-file' compiler-output.raw";
   output = "compiler-output";
   script;
   compiler_output = "compiler-output";
   check-ocamlc.byte-output;
 }{
   compiler_output = "compiler-output.raw";
   setup-ocamlopt.byte-build-env;
   all_modules = "";
   flags = "unknown-file";
   ocamlopt_byte_exit_status = "2";
   ocamlopt.byte;
   script = "grep 'know what to do with unknown-file' compiler-output.raw";
   output = "compiler-output";
   script;
   compiler_output = "compiler-output";
   check-ocamlopt.byte-output;
 }
*)

(*
  This file is just a test driver, the test does not contain any
  real OCaml code
*)
