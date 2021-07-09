(* TEST
include tool-ocaml-lib
flags = "-w -a"
ocaml_script_as_argument = "true"
* setup-ocaml-build-env
** ocaml
*)

let _ = () in 2;;

(**
       0 CONST0
       1 PUSHCONST2
       2 POP 1
       4 ATOM0
       5 SETGLOBAL T021-pushconst2
       7 STOP
**)
