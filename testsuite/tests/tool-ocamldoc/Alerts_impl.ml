(* TEST
 * ocamldoc with html
 *)

(** Alerts from implementation. *)

let x = 0
[@@deprecated "foo"]

type r = { x : int [@deprecated "foo"] }

type v = C [@deprecated "foo"]

type t [@@deprecated "foo"]

exception E [@deprecated "foo"]

external f : int -> int = "" [@@deprecated "foo"]

type e = ..
type e += Ext [@@deprecated "foo"]
type e += Ext2 [@deprecated "foo"]
