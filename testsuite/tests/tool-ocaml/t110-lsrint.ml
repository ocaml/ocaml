open Lib;;
if (14 lsr 2) <> 3 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST3
      10 PUSHCONST2
      11 PUSHCONSTINT 14
      13 LSRINT
      14 NEQ
      15 BRANCHIFNOT 22
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE
      22 ATOM0
      23 SETGLOBAL T110-lsrint
      25 STOP
**)
