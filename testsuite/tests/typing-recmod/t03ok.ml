(* TEST
flags = " -w -a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

(* OK (t = int) *)
module rec A : sig type t = B.t end = struct type t = B.t end
       and B : sig type t = int end = struct type t = int end;;
