(* TEST_BELOW
(* Blank lines added here to preserve locations. *)




*)

(* Bad (t = t) *)
module rec A : sig type t = A.t end = struct type t = A.t end;;

(* TEST
 flags = " -w -a ";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
