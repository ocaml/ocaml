(* TEST
include tool-ocaml-lib
flags = "-w -a"
ocaml_script_as_argument = "true"
* setup-ocaml-build-env
** ocaml
*)

4;;

(**
       0 CONSTINT 4
       2 ATOM0
       3 SETGLOBAL T011-constint
       5 STOP
**)
