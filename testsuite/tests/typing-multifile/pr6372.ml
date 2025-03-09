(* TEST
 readonly_files = "d.mli e.ml";
 setup-ocamlc.byte-build-env;
 module = "d.mli";
 ocamlc.byte;
 module = "e.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
