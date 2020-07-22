(* TEST

files = "sigint.c"

* hasunix
include unix

** setup-ocamlc.byte-build-env

program = "${test_build_directory}/break_EINTR.byte"

*** ocamlc.byte

program = "sigint"
all_modules = "sigint.c"

**** ocamlc.byte

program = "${test_build_directory}/break_EINTR.byte"
all_modules = "break_EINTR.ml"

***** check-ocamlc.byte-output
****** run
******* check-program-output

** setup-ocamlopt.byte-build-env

program = "${test_build_directory}/break_EINTR.opt"

*** ocamlopt.byte

program = "sigint"
all_modules = "sigint.c"

**** ocamlopt.byte

program = "${test_build_directory}/break_EINTR.opt"
all_modules = "break_EINTR.ml"

***** check-ocamlopt.byte-output
****** run
******* check-program-output

*)

exception Break

let handler _ =
  raise Break

let () =
  ignore (Sys.signal Sys.sigint (Sys.Signal_handle handler));
  try
    ignore (Unix.select [] [] [] (-100.));
    Printf.printf "No exception\n";
  with Break -> Printf.printf "GOOD!\n"
