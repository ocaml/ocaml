(* Transformation of N-way integer branches *)

open Lambda

(* Input: a list of (key, action) pairs, where keys are integers. *)
(* Output: a table of (low, high, offset) triples for Ktranslate
           an array of actions for Kswitch *)

val make_decision_tree:
  (int * lambda) list -> (int * int * int) array * lambda array
