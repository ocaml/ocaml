(* TEST
readonly_files = "a.ml b.ml c.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "a.ml"
*** ocamlc.byte
module = "b.ml"
**** script
script = "rm a.cmi"
***** ocamlc.byte
module = "c.ml"
****** check-ocamlc.byte-output
*)
