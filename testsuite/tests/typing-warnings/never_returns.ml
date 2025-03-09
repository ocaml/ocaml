(* TEST
 flags = " -w -a+21 ";
 expect;
*)

(** For now, we don't warn for non-terminating while loops, for backwards compatibility. *)
fun () -> while true do () done; 3;;

[%%expect{|
- : unit -> int = <fun>
|}];;

(** For now, we don't warn for non-terminating while loops, for backwards compatibility. *)
fun () -> (if true then while true do () done else while true do () done); 3;;

[%%expect{|
- : unit -> int = <fun>
|}];;

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
