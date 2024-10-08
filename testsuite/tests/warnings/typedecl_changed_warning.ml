(* TEST_BELOW
*)

type t = A [@deprecated "blah"]
type t2 = t = A [@@warning "-3"]
(* TEST
 flags = "-w +A-70";
 bytecode;
*)
