(* TEST
 ocamldoc with html;
*)

(** Test the rendering of alerts. *)

val a : int
[@@alert foo "bar"]
(** Documentation after. *)

(** Documentation before. *)
val b : int
[@@alert foo "bar"]

(* No documentation. *)
val c : int
[@@alert foo "bar"]

(** The deprecated attribute. *)

val d : int
[@@deprecated "foo"]

val d' : int
[@@alert deprecated "foo"]

(** No payload *)

val e : int
[@@deprecated]

val f : int
[@@alert foo]

(** Both deprecated tag and alert *)

val g : int
[@@deprecated "foo"]
(** @deprecated bar *)

(** Constructors and fields and other items. *)

type r = { x : int [@deprecated "foo"] }

type v = C [@deprecated "foo"]

type t [@@deprecated "foo"]

exception E [@deprecated "foo"]

external f : int -> int = "" [@@deprecated "foo"]

type e = ..
type e += Ext [@@deprecated "foo"]
type e += Ext2 [@deprecated "foo"]
