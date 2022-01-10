(* TEST

use_runtime = "false"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags = "-w -a -output-complete-exe -ccopt -I'${ocamlsrcdir}'/runtime"
program = "./github9344"
*** run
ocamlrunparam += ",b=1"
exit_status = "2"
**** check-program-output
*)

raise Not_found
