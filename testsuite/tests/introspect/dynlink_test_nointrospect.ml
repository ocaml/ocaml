(* TEST
   readonly_files = "printval.ml";
   no-introspect;
   {
     native-dynlink;
     native-compiler;
     setup-ocamlopt.opt-build-env;
     script = "sh ${test_source_directory}/dynlink_test.sh dynlink_test_nointrospect.ml";
     script;
     check-program-output;
   }{
     setup-ocamlc.byte-build-env;
     script = "sh ${test_source_directory}/dynlink_test_bytecode.sh dynlink_test_nointrospect.ml";
     script;
     check-program-output;
   }
*)

let () = Dynlink.loadfile (if Dynlink.is_native then "printval.cmxs" else "printval.cmo")
