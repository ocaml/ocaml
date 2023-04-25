(* TEST
 native-compiler;
 script = "sh ${test_source_directory}/has-afl-showmap.sh";
 readonly_files = "harness.ml test.ml";
 script;
 setup-ocamlopt.byte-build-env;
 module = "test.ml";
 flags = "-afl-instrument";
 ocamlopt.byte;
 module = "";
 program = "${test_build_directory}/test";
 flags = "-afl-inst-ratio 0";
 all_modules = "test.cmx harness.ml";
 ocamlopt.byte;
 run;
*)

(* No code here, this file is a pure test script. *)
