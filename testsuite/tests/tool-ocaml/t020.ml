(* TEST
include tool-ocaml-lib
flags = "-w -a"
ocaml_script_as_argument = "true"
* setup-ocaml-build-env
** ocaml
*)

let _ = () in ();;

(**
       0 CONST0
       1 PUSHCONST0
       2 POP 1
       4 ATOM0
       5 SETGLOBAL T020
       7 STOP
**)
