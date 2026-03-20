(* TEST
   native-dynlink;
   native-compiler;
   hasunix;
   readonly_files = "toto.ml main.ml";
   output = "${test_build_directory}/script.output";
   setup-ocamlopt.opt-build-env;
   script = "sh ${test_source_directory}/test.sh";
   script;
*)
