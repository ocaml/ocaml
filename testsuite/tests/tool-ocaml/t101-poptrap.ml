(* TEST
include tool-ocaml-lib
flags = "-w -a"
ocaml_script_as_argument = "true"
* setup-ocaml-build-env
** ocaml
*)

open Lib;;
try ()
with _ -> ()
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 PUSHTRAP 15
      11 CONST0
      12 POPTRAP
      13 BRANCH 18
      15 PUSHCONST0
      16 POP 1
      18 ATOM0
      19 SETGLOBAL T101-poptrap
      21 STOP
**)
