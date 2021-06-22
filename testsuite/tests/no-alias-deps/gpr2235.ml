(* TEST
flags = "-no-alias-deps -w -49"
compile_only = "true"
readonly_files = "a2235.ml lib__2235.ml lib2235.ml user_of_lib2235.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "lib__2235.ml"
*** check-ocamlc.byte-output
**** ocamlc.byte
flags = "-no-alias-deps -w -49 -open Lib__2235 -o lib__A2235.cmo"
module = "a2235.ml"
***** check-ocamlc.byte-output
****** ocamlc.byte
flags = "-no-alias-deps -w -49 -open Lib__2235"
module = "lib2235.ml"
******* check-ocamlc.byte-output
******** ocamlc.byte
flags = "-no-alias-deps -w -49"
module = "user_of_lib2235.ml"
********* check-ocamlc.byte-output
*)
