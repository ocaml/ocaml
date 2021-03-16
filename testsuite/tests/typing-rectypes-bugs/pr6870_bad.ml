(* TEST
flags = " -w -a -rectypes "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module type T = sig type 'a t end
module Fix (T : T) = struct type r = ('r T.t as 'r) end
