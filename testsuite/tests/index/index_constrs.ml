(* TEST
flags = "-bin-annot"
compile_only = "true"
readonly_files = "index_constrs.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
all_modules = "index_constrs.ml"
*** check-ocamlc.byte-output
**** ocamlobjinfo
program = "-index index_constrs.cmt"
output = "out_objinfo"
***** run
program = "awk '/Indexed/,0' out_objinfo"
output = "out_awk"
****** check-program-output
*)

exception E
module M = struct
  exception F = E
end

type t = E

let x_ = E
let () = raise E
let f x = match x with (* FIXME: this x is not exposed but still indexed *)
  | E -> ()
  | exception E -> ()
