(* TEST
   flags = " -w -a+21 "
   * expect
*)

let () = (let module L = List in raise Exit); () ;;
[%%expect {|
Line 1, characters 33-43:
1 | let () = (let module L = List in raise Exit); () ;;
                                     ^^^^^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)
Exception: Stdlib.Exit.
|}]
let () = (let exception E in raise Exit); ();;
[%%expect {|
Line 1, characters 29-39:
1 | let () = (let exception E in raise Exit); ();;
                                 ^^^^^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)
Exception: Stdlib.Exit.
|}]
let () = (raise Exit : _); ();;
[%%expect {|
Line 1, characters 10-20:
1 | let () = (raise Exit : _); ();;
              ^^^^^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)
Exception: Stdlib.Exit.
|}]
let () = (let open Stdlib in raise Exit); ();;
[%%expect {|
Line 1, characters 29-39:
1 | let () = (let open Stdlib in raise Exit); ();;
                                 ^^^^^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)
Exception: Stdlib.Exit.
|}]
