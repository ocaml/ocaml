(* TEST
   * ocamldoc with html
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

(** No payload *)

val e : int
[@@deprecated]

val f : int
[@@alert foo]
