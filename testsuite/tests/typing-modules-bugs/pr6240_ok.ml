(* TEST
flags = " -w -a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module M : sig
  module type T
  module F (X : T) : sig end
end = struct
  module type T = sig end
  module F (X : T) = struct end
end

module type T = M.T

module F : functor (X : T) -> sig end = M.F
