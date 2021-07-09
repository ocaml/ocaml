(* TEST
flags = " -w -a "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

(* Bad (not regular) *)
module rec A : sig type 'a t = <m: 'a list A.t> end
             = struct type 'a t = <m: 'a list A.t> end;;
