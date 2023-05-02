(* TEST
 readonly_files = "a.ml b.ml c.ml main.ml main_ok.ml";
 subdirectories = "subdir";
 setup-ocamlc.byte-build-env;
 module = "subdir/m.ml";
 ocamlc.byte;
 flags = "-I subdir";
 module = "a.ml";
 ocamlc.byte;
 module = "b.ml";
 ocamlc.byte;
 module = "c.ml";
 ocamlc.byte;
 flags = "";
 module = "main_ok.ml";
 ocamlc.byte;
 module = "main.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
