(* TEST
 readonly_files = "foo.ml foo.mli";
 setup-ocamlc.byte-build-env;

 module = "foo.mli";
 ocamlc.byte;

 module = "-impl foo.ml";
 flags = "-cmi-file foo.cmi";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
