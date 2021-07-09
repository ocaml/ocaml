(* TEST
include tool-ocaml-lib
flags = "-w -a"
ocaml_script_as_argument = "true"
* setup-ocaml-build-env
** ocaml
*)

let _ = () in 0.01;;

(**
       0 CONST0
       1 PUSHGETGLOBAL 0.01
       3 POP 1
       5 ATOM0
       6 SETGLOBAL T050-pushgetglobal
       8 STOP
**)
