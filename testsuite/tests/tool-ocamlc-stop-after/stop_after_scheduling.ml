(* TEST
 compiler_output = "compiler-output.raw";
 setup-ocamlc.byte-build-env;
 flags = "-stop-after scheduling";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "sh ${test_source_directory}/stop_after_scheduling.sh";
 output = "compiler-output";
 script;
 compiler_output = "compiler-output";
 check-ocamlc.byte-output;
*)

(* this file is just a test driver, the test does not contain real OCaml code *)
