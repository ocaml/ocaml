(* TEST
   readonly_files = "printval.ml";
   {
     introspect;
     reference = "${test_source_directory}/dynlink_test.reference";
   }{
     no-introspect;
     reference = "${test_source_directory}/dynlink_test.no-introspect.reference";
   }{
     native-dynlink;
     native-compiler;
     setup-ocamlopt.opt-build-env;
     script = "sh ${test_source_directory}/dynlink_test.sh";
     script;
     check-program-output;
   }{
     setup-ocamlc.byte-build-env;
     script = "sh ${test_source_directory}/dynlink_test_bytecode.sh";
     script;
     check-program-output;
   }
*)

let () = Dynlink.loadfile (if Dynlink.is_native then "printval.cmxs" else "printval.cmo")
