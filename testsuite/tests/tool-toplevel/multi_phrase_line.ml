(* TEST_BELOW *)

Printexc.record_backtrace false;;

1;; 2;; (* Two phrases on the same line *)

3;; ignore
4;; (* Wait for ;; at end of line before evaluating anything. *)

5;;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ignore
6;; (* Very long line needs buffer refills. *)

7;; (* linefeed in a comment after double-semi
*)

750;; (*) comment-start warning after semicolon must be displayed once
*)

8;; let 9;; 10;; (* Syntax error in second phrase. *)

11;; let x = 12+true;; 13;; (* Type error in second phrase. *)

match 14 with 15 -> ();; 16;; 17;; (* Warning + run-time error in 1st phrase. *)

18;; match 19 with 20 -> ();; 21;; (* Warning + run-time error in 2nd phrase. *)

let f 22 = ();; let f 23 = ();; let f 24 = ();; (* Several warnings. *)

(* TEST
   flags = "-prompt";
   toplevel;
*)
