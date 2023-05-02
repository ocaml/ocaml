(* TEST
 plugins = "odoc_test.ml";
 flags = "-I ${ocamlsrcdir}/ocamldoc";
 ocamldoc;
*)

module rec A : sig type t end = B and B : sig type t = A.t end = A;;
