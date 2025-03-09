(* TEST_BELOW
(* Blank lines added here to preserve locations. *)



*)

type fpclass = A

module Stdlib = struct
  type fpclass = B
end

let f A Stdlib.B = FP_normal

(* TEST
 flags = "-i -w +63";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
