(* TEST
{
  native-compiler;

  script = "sh ${test_source_directory}/has-afl-fuzz.sh";
  readonly_files = "readline.ml";
  script;

  setup-ocamlopt.byte-build-env;

  program = "${test_build_directory}/readline";
  flags = "-afl-instrument";
  all_modules = "readline.ml";
  ocamlopt.byte;

  run;
}
*)

(* Just a test-driver, no code here. *)
