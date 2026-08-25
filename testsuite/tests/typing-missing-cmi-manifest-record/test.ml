(* TEST
 readonly_files = "a.ml main.ml";
 subdirectories = "subdir1 subdir2";
 setup-ocamlc.byte-build-env;
 module = "subdir1/m.ml";
 ocamlc.byte;
 flags = "-I subdir1";
 module = "subdir2/n.ml";
 ocamlc.byte;
 flags = "-I subdir2";
 module = "a.ml";
 ocamlc.byte;
 flags = "";
 module = "main.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
