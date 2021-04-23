(* TEST

flags = "-w +a"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

[@@@warning "-missing-mli"]
