(* TEST
include tool-ocaml-lib
flags = "-w -a"
ocaml_script_as_argument = "true"
* setup-ocaml-build-env
** ocaml
*)

[1];;

(**
       0 GETGLOBAL <0>(1, 0)
       2 ATOM0
       3 SETGLOBAL T050-getglobal
       5 STOP
**)
