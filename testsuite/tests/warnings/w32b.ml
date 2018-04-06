(* TEST

flags = "-w A"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

(* Check that [t] is considered unused without an .mli file (see GPR#1358) *)
module Q (M : sig type t end) = struct end
