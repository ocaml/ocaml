(* TEST
 native-compiler;
 compiler_output = "compiler-output.raw";
 setup-ocamlopt.byte-build-env;
 {
   flags = "-save-ir-after typing";
   ocamlopt_byte_exit_status = "2";
   ocamlopt.byte;
 }{
   script = "sh ${test_source_directory}/save_ir_after_typing.sh";
   output = "compiler-output";
   script;
   compiler_output = "compiler-output";
   check-ocamlopt.byte-output;
 }
*)

(* this file is just a test driver, the test does not contain real OCaml code *)
