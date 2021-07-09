(* TEST

use_runtime = "false"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags = "-w -a -output-complete-exe -ccopt -I${ocamlsrcdir}/runtime"
program = "github9344"
*** run
program = "sh ${test_source_directory}/github9344.sh"
**** check-program-output
*)

raise Not_found
