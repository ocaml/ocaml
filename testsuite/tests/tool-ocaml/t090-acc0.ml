(* TEST
include tool-ocaml-lib
flags = "-w -a"
ocaml_script_as_argument = "true"
* setup-ocaml-build-env
** ocaml
*)

open Lib;;
let x = true in
();
if not x then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST1
      10 PUSHCONST0
      11 ACC0
      12 BOOLNOT
      13 BRANCHIFNOT 20
      15 GETGLOBAL Not_found
      17 MAKEBLOCK1 0
      19 RAISE
      20 POP 1
      22 ATOM0
      23 SETGLOBAL T090-acc0
      25 STOP
**)
