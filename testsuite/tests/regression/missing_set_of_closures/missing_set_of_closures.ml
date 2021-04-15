(* TEST
readonly_files = "a.ml b.ml b2.ml"
subdirectories = "dir"
* setup-ocamlopt.byte-build-env
** ocamlopt.byte
module = "a.ml"
*** ocamlopt.byte
module = "b.ml"
**** ocamlopt.byte
module = "b2.ml"
***** copy
src = "b.cmx b.cmi b2.cmx b2.cmi"
dst = "dir/"
****** cd
cwd = "dir"
******* ocamlopt.byte
module = "c.ml"
flags = "-w -58"
******** check-ocamlopt.byte-output
*)
