(* TEST
* setup-ocamlc.byte-build-env
** ocamlc.byte
ocamlc_byte_exit_status = "2"
*** check-ocamlc.byte-output
*)

(* Failed to get the unclosed object error message. *)

let o = object
