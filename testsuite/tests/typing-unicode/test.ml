(* TEST
readonly_files = "genfiles.ml";
setup-ocamlc.byte-build-env;
all_modules = "genfiles.ml";
program = "./genfiles.byte.exe";
ocamlc.byte;
run;
all_modules = "été.ml ça.ml test.ml";
program = "./main.byte.exe";
ocamlc.byte;
run;
*)

let _ =
  assert (Été.x + Ça.x = 3)
