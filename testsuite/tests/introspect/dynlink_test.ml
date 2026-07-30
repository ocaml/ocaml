(* TEST
   script = "sh ${test_source_directory}/has-introspect.sh";
   script;
   native-dynlink;
   native-compiler;
   readonly_files = "printval.ml";
   output = "${test_build_directory}/script.output";
   setup-ocamlopt.opt-build-env;
   script = "sh ${test_source_directory}/dynlink_test.sh";
   script;
*)

let () = Dynlink.loadfile (if Dynlink.is_native then "printval.cmxs" else "printval.cmo")
