(* TEST
flags = " -w -a "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module A = struct module type S module S = struct end end
module F (_ : sig end) = struct module type S module S = A.S end
module M = struct end
module N = M
module G (X : F(N).S) : A.S = X
