(* TEST
flags = " -w -a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module F (_ : sig end) = struct module type S end
module M = struct end
module N = M
module G (X : F(N).S) : F(M).S = X
