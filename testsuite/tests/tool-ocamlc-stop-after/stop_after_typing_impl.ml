(* TEST
* setup-ocamlc.byte-build-env
** ocamlc.byte
    flags = "-stop-after typing -dno-unique-ids -dtypedtree"
    ocamlc_byte_exit_status = "0"
*** check-ocamlc.byte-output
*)

(* we intentionally write an output that is type-correct
   but will be rejected before bytecode compilation
   due to the incorrect type given to the %apply
   compiler primitive. *)
external apply: int -> int = "%apply"
