(* TEST
 modules = "inner.ml alias.ml";
 ocamldoc_backend = "latex";
 ocamldoc_flags = " -open Alias.Container -open Aliased_inner ";
 ocamldoc;
*)

(** Documentation test *)

type t = a
(** Alias to type Inner.a *)
