(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* It is apparently impossible to get the "unclosed object" message. *)

class c : object
