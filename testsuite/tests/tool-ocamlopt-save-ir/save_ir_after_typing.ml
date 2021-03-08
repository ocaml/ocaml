(* TEST
 * native-compiler
 ** setup-ocamlopt.byte-build-env
   compiler_output = "compiler-output.raw"
 *** ocamlopt.byte
   flags = "-save-ir-after typing"
   ocamlopt_byte_exit_status = "2"
 *** script
   script = "sh ${test_source_directory}/save_ir_after_typing.sh"
   output = "compiler-output"
 **** check-ocamlopt.byte-output
   compiler_output = "compiler-output"
*)

(* this file is just a test driver, the test does not contain real OCaml code *)
