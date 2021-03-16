(* TEST
   flags = " -w +A -strict-sequence "
   * expect
*)

(* Ignore OCAMLRUNPARAM=b to be reproducible *)
Printexc.record_backtrace false;;
[%%expect {|
- : unit = ()
|}]

let () = raise Exit; () ;; (* warn *)
[%%expect {|
Line 1, characters 9-19:
1 | let () = raise Exit; () ;; (* warn *)
             ^^^^^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)
Exception: Stdlib.Exit.
|}]
