(* TEST
   script = "sh ${test_source_directory}/has-introspect.sh";
   script;
   readonly_files = "printval.ml";
   {
     native-dynlink;
     native-compiler;
     setup-ocamlopt.opt-build-env;
     script = "sh ${test_source_directory}/dynlink_test.sh";
     script;
   }{
     setup-ocamlc.byte-build-env;
     script = "sh ${test_source_directory}/dynlink_test_bytecode.sh";
     script;
   }
*)

let () = Dynlink.loadfile (if Dynlink.is_native then "printval.cmxs" else "printval.cmo")
