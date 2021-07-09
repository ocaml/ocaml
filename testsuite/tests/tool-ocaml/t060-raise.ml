(* TEST
include tool-ocaml-lib
flags = "-w -a"
ocaml_script_as_argument = "true"
ocaml_exit_status = "2"
* setup-ocaml-build-env
** ocaml
*)

open Lib;;
raise End_of_file;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 GETGLOBAL End_of_file
      11 MAKEBLOCK1 0
      13 RAISE
      14 SETGLOBAL T060-raise
      16 STOP
**)
