(* TEST
   script = "sh ${test_source_directory}/has-introspect.sh";
   script;
   native-dynlink;
   native-compiler;
   readonly_files = "loaded.ml main.ml";
   output = "${test_build_directory}/script.output";
   setup-ocamlopt.opt-build-env;
   script = "sh ${test_source_directory}/dynlink_test.sh";
   script;
*)
