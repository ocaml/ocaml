(* TEST
 readonly_files = "a.ml b.ml b2.ml";
 subdirectories = "dir";
 setup-ocamlopt.byte-build-env;
 module = "a.ml";
 ocamlopt.byte;
 module = "b.ml";
 ocamlopt.byte;
 module = "b2.ml";
 ocamlopt.byte;
 src = "b.cmx b.cmi b2.cmx b2.cmi";
 dst = "dir/";
 copy;
 cwd = "dir";
 cd;
 module = "c.ml";
 flags = "-w -58";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)
