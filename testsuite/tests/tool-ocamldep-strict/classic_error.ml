(* TEST
modules = "dep.ml a.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
commandline = "-depend a.ml doesnotexist.ml"
*** check-ocamlc.byte-output
*)
