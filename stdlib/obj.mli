(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Obj]: operations on internal representations of values *)

(* Not for the casual user. *)

type t

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
external is_block : t -> bool = "obj_is_block"
external tag : t -> int = "obj_tag"
external size : t -> int = "%obj_size"
external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
external new_block : int -> int -> t = "obj_block"
external dup : t -> t = "obj_dup"
external truncate : t -> int -> unit = "obj_truncate"

val no_scan_tag : int
val closure_tag : int
val infix_tag : int
val object_tag : int
val abstract_tag : int
val string_tag : int
val double_tag : int
val double_array_tag : int
val final_tag : int

(* The following two functions are deprecated.  Use module [Marshal]
   instead. *)

val marshal : t -> string
val unmarshal : string -> int -> t * int

