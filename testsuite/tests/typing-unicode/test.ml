(* TEST
readonly_files = "genfiles.ml"
*01 setup-ocamlc.byte-build-env
*02 ocamlc.byte
all_modules = "genfiles.ml"
program = "./genfiles.byte.exe"
*03 run
*04 ocamlc.byte
all_modules = "été.ml ça.ml test.ml"
program = "./main.byte.exe"
*05 run
*)

let _ =
  assert (Été.x + Ça.x = 3)
