(* TEST
   native-dynlink;
   native-compiler;
   output = "test.output";
   reference = "${test_source_directory}/test.reference";
   program = "bash";
   arguments = "${test_source_directory}/test.sh ${ocamlsrcdir}";
   run;
   check-program-output;
*)
