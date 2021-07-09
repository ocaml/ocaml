(* TEST
include tool-ocaml-lib
flags = "-w -a"
ocaml_script_as_argument = "true"
* setup-ocaml-build-env
** ocaml
*)

open Lib;;
let rec f _ = 0;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 14
      11 CONST0
      12 RETURN 1
      14 CLOSUREREC 0, 11
      18 ACC0
      19 MAKEBLOCK1 0
      21 POP 1
      23 SETGLOBAL T250-closurerec-1
      25 STOP
**)
