(* TEST
   native-dynlink;
   output = "test.output";
   reference = "${test_source_directory}/test.reference";
   program = "${test_source_directory}/test.sh";
   arguments = "${ocamlsrcdir}";
   run;
   check-program-output;
*)
