(* TEST_BELOW *)

1;; 2;; (* Two phrases on the same line *)

1;; ignore
2;; (* Wait for ;; at end of line before evaluating anything. *)

1;;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ignore
2;; (* Very long line needs buffer refills. *)

1;; let;; 3;; (* Syntax error in second phrase. *)

1;; let x = 2+true;; 3;; (* Type error in second phrase. *)

match 1 with 11 -> ();; 2;; 3;; (* Warning + run-time error in first phrase. *)

1;; match 2 with 22 -> ();; 3;; (* Warning + run-time error in second phrase. *)

let f 1 = ();; let f 2 = ();; let f 3 = ();; (* Several warnings. *)

(* TEST
   * toplevel
     flags = "-prompt"
*)
