(* TEST
   * ocamldoc with html
   * ocamldoc with latex
*)
module rec A : sig type t end = B and B : sig type t = A.t end = A;;
