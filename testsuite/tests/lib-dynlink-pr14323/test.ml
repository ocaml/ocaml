(* TEST
   native-dynlink;
   native-compiler;
   output = "test.output";
   reference = "${test_source_directory}/test.reference";
   program = "${test_source_directory}/test.sh";
   arguments = "${ocamlsrcdir}";
   run;
   check-program-output;
*)
