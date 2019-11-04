(* TEST
 * native-compiler
 ** setup-ocamlopt.byte-build-env
 *** ocamlopt.byte
   flags = "-stop-after scheduling -S"
   ocamlopt_byte_exit_status = "0"
 **** check-ocamlopt.byte-output
 ***** script
   script = "sh ${test_source_directory}/stop_after_scheduling.sh"
*)

(* this file is just a test driver, the test does not contain real OCaml code *)
