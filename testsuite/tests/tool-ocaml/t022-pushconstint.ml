(* TEST
include tool-ocaml-lib
flags = "-w -a"
ocaml_script_as_argument = "true"
* setup-ocaml-build-env
** ocaml
*)

let _ = () in -1;;

(**
       0 CONST0
       1 PUSHCONSTINT -1
       3 POP 1
       5 ATOM0
       6 SETGLOBAL T022-pushconstint
       8 STOP
**)
