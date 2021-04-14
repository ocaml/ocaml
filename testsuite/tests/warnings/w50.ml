(* TEST

flags = "-w +A-70"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

module A : sig end = struct
  module L = List

  module X1 = struct end

  module Y1 = X1
end
