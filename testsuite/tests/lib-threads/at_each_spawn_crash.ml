(* TEST
readonly_files = "early_hook.ml"
* hassysthreads
include systhreads
** setup-ocamlopt.byte-build-env
*** ocamlopt.byte
module = "early_hook.ml"
**** ocamlopt.byte
program = "${test_build_directory}/main.exe"
module = ""
flags = "early_hook.cmx"
***** run
****** check-program-output
*)


let print_self () =
  Printf.printf "self=%d\n" (Thread.id (Thread.self ()))

let () =
  Early_hook.f := print_self;
  for i = 0 to 50 do
    Domain.join (Domain.spawn ignore)
  done
