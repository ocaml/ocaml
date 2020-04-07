(* TEST
* setup-ocamlc.byte-build-env
** ocamlc.byte
ocamlc_byte_exit_status = "2"
*** check-ocamlc.byte-output
*)

class c = object
  method x = 1
