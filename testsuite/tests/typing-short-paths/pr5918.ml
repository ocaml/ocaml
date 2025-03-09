(* TEST_BELOW
(* Blank lines added here to preserve locations. *)

*)

module rec A : sig
 type t
end = struct
 type t = { a : unit; b : unit }
 let _ = { a = () }
end
;;

(* TEST
 flags = " -short-paths ";
 toplevel;
*)
