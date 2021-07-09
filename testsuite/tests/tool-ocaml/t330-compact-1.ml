(* TEST
include tool-ocaml-lib
flags = "-w -a"
ocaml_script_as_argument = "true"
* setup-ocaml-build-env
** ocaml
*)

open Lib;;
Gc.compact ();;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST0
      10 C_CALL1 gc_compaction
      12 ATOM0
      13 SETGLOBAL T330-compact-1
      15 STOP
**)
