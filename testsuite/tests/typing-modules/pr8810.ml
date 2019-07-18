(* TEST
* setup-ocamlc.byte-build-env
flags = "-no-alias-deps -w -49 -c"
** ocamlc.byte
ocamlc_byte_exit_status = "2"
*)
module Loop = Pr8810
