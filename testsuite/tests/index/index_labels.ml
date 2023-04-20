(* TEST
flags = "-bin-annot"
compile_only = "true"
readonly_files = "index_labels.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
all_modules = "index_labels.ml"
*** check-ocamlc.byte-output
**** ocamlobjinfo
program = "-index index_labels.cmt"
output = "out_objinfo"
***** run
program = "awk '/Indexed/,0' out_objinfo"
output = "out_awk"
****** check-program-output
*)

type t = { mutable a: int; b: string }

let x = { a = 42; b = "" }
let _y =
  x.a <- 32;
  x.a

let f = function
  | { a = 42; b } -> ()
  | _ -> ()
