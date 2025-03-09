(* TEST
 readonly_files = "foo.mli bar.mli baz.ml";
 setup-ocamlc.byte-build-env;
 module = "foo.mli";
 ocamlc.byte;
 module = "bar.mli";
 ocamlc.byte;
 script = "rm foo.cmi";
 script;
 flags = "-c -i";
 module = "baz.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
