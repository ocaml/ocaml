(* TEST
flags = " -w -a "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

(* Bad - PR 4512 *)
module type S' = sig type t = int end
module rec M : S' with type t = M.t = struct type t = M.t end;;
