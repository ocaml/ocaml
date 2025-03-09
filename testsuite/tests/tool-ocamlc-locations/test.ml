(* TEST
 readonly_files="trivpp.ml testloc.ml";

 setup-ocamlc.byte-build-env;

 program="trivpp.byte";
 all_modules = "trivpp.ml";
 ocamlc.byte;

 all_modules = "testloc.ml";
 flags = "-error-style contextual -stop-after typing -pp ${test_build_directory}/trivpp.byte";
 ocamlc_byte_exit_status="2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* This is a repro case for #12238. *)
