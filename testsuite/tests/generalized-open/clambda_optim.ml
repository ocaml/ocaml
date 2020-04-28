(* TEST

compile_only = "true"

* no-flambda
** setup-ocamlopt.byte-build-env
*** ocamlopt.byte
**** check-ocamlopt.byte-output

*)

module Stable = struct
  open struct module V0 = struct module U = struct end end end
  module V0 = V0.U
end
