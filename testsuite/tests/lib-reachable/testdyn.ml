(* TEST

include dynlink
files = "plugin.ml"

* setup-ocamlopt.opt-build-env
** ocamlopt.opt
program2 = "testdyn.exe"
*** ocamlopt.opt
flags = "-shared"
program2 = "plugin.cmxs"
all_modules = "plugin.ml"
**** run
program = "./testdyn.exe"
***** check-program-output
*)
let _ =
  try
    Dynlink.loadfile "plugin.cmxs"
  with
  | Dynlink.Error error ->
    prerr_endline (Dynlink.error_message error)
