(* TEST
 subdirectories = "dir1 dir2";
 setup-ocamlc.byte-build-env;
 commandline = "-depend -slash -I dir1 -I dir2 a.ml";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/a.reference";
 check-ocamlc.byte-output;
*)

include B
include C
