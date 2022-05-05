(* TEST
modules = "dep.ml a.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
commandline = "-depend -strict a.ml"
*** check-ocamlc.byte-output
*)
