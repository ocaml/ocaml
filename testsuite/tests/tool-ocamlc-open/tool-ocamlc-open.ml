(* TEST
readonly_files = "a.ml b.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "a.ml"
*** ocamlc.byte
module = "b.ml"
flags = "-open A.M"
**** check-ocamlc.byte-output
*)
