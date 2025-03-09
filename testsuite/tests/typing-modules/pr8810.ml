(* TEST
 flags = "-no-alias-deps -w -49 -c";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
*)
module Loop = Pr8810
