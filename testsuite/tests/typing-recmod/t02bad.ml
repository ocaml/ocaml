(* TEST
flags = " -w -a "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

(* Bad (t = t) *)
module rec A : sig type t = B.t end = struct type t = B.t end
       and B : sig type t = A.t end = struct type t = A.t end;;
