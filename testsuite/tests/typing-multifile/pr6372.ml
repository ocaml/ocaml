(* TEST
readonly_files = "d.mli e.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "d.mli"
*** ocamlc.byte
module = "e.ml"
**** check-ocamlc.byte-output
*)
