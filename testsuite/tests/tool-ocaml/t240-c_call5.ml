(* TEST
include tool-ocaml-lib
flags = "-w a"
ocaml_script_as_argument = "true"
* setup-ocaml-build-env
** ocaml
*)

open Lib;;
let s = Bytes.of_string "abcdefgh" in
Bytes.unsafe_blit s 3 s 0 3;
if Bytes.get s 0 <> 'd' then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 GETGLOBAL "abcdefgh"
      11 PUSHCONST3
      12 PUSHCONST0
      13 PUSHACC2
      14 PUSHCONST3
      15 PUSHACC4
      16 C_CALL5 blit_string
      18 CONSTINT 100
      20 PUSHCONST0
      21 PUSHACC2
      22 GETSTRINGCHAR
      23 NEQ
      24 BRANCHIFNOT 31
      26 GETGLOBAL Not_found
      28 MAKEBLOCK1 0
      30 RAISE
      31 POP 1
      33 ATOM0
      34 SETGLOBAL T240-c_call5
      36 STOP
**)
