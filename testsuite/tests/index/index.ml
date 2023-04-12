(* TEST
flags = "-bin-annot"
compile_only = "true"
readonly_files = "aux.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
all_modules = "aux.ml index.ml"
*** check-ocamlc.byte-output
**** ocamlobjinfo
program = "-index index.cmt"
***** check-program-output
*)

module A = struct
  type t = int
  let (x : t) = 42
end

module B = A

let y = A.x + Aux.z

let () = print_int y
