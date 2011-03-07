(* Some Some Some None;; *)
(* ((Some None) None) None;; *)
((Some) None);;
(* ((Some Some) Some) None;; *)
type t = A of int * int * int;;
A (1, 2, 3);;
(A) (1, 2, 3);;
(A (1, 2)) 3;;
