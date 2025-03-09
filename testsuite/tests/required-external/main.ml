(* TEST
 modules = "file.ml";
 {
   program = "${test_build_directory}/main.exe";
   setup-ocamlc.byte-build-env;
   module = "file.ml";
   ocamlc.byte;
   module = "";
   program = "lib.cma";
   flags = "-a";
   all_modules = "file.cmo";
   ocamlc.byte;
   program = "${test_build_directory}/main.exe";
   all_modules = "lib.cma main.ml";
   flags = "";
   ocamlc.byte;
   check-ocamlc.byte-output;
   run;
   check-program-output;
 }{
   program = "${test_build_directory}/main.exe";
   setup-ocamlopt.byte-build-env;
   module = "file.ml";
   ocamlopt.byte;
   module = "";
   program = "lib.cmxa";
   flags = "-a";
   all_modules = "file.cmx";
   ocamlopt.byte;
   program = "${test_build_directory}/main.exe";
   all_modules = "lib.cmxa main.ml";
   flags = "";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   run;
   check-program-output;
 }
*)

let () =
  ignore (File.getcwd ())
