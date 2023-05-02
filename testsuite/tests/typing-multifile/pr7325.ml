(* TEST
 readonly_files = "a.ml b.ml c.ml";
 setup-ocamlc.byte-build-env;
 module = "a.ml";
 ocamlc.byte;
 module = "b.ml";
 ocamlc.byte;
 script = "rm a.cmi";
 script;
 module = "c.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
