(* TEST
* setup-ocamlc.byte-build-env
compiler_output = "compiler-output.raw"
** ocamlc.byte
   flags = "-stop-after scheduling"
   ocamlc_byte_exit_status = "2"
*** script
   script = "sh ${test_source_directory}/stop_after_scheduling.sh"
   output = "compiler-output"
**** check-ocamlc.byte-output
compiler_output = "compiler-output"
*)

(* this file is just a test driver, the test does not contain real OCaml code *)
