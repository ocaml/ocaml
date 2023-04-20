(* TEST
 readonly_files = "a.ml b.ml";
 setup-ocamlc.byte-build-env;
 module = "a.ml";
 ocamlc.byte;
 module = "b.ml";
 flags = "-open A.M";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
