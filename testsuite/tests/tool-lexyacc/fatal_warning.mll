(* TEST
  setup-simple-build-env;
  program = "${ocamlrun} ${ocamlsrcdir}/lex/ocamllex";
  arguments = "-q -w @missing-case fatal_warning.mll";
  exit_status = "3";
  output = "fatal_warning.output";
  reference = "${test_source_directory}/fatal_warning.compilers.reference";
  run;
  check-program-output;
  script = "sh -c 'test ! -e fatal_warning.ml'";
  exit_status = "0";
  script;
  src = "fatal_warning.mll";
  dst = "fatal_warning.ml";
  copy;
  exit_status = "3";
  run;
  check-program-output;
  exit_status = "0";
  script;
*)
(*
   Check that making a warning fatal works without leaving an output file
*)

rule missing_case = parse
| _  { () }
