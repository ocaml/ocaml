(* TEST
flags = " -w -a "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module type S = sig type t = { a : int; b : int; } end;;
let f (module M : S with type t = int) = { M.a = 0 };;
