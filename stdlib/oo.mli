(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Oo]: object-oriented extension *)

val copy : (< .. > as 'a) -> 'a
        (* [Oo.copy o] returns a copy of object [o]. *)


(*--*)

(*** For system use only, not for the casual user *)

(* Methods *)
type label
val new_method: string -> label

(* Classes *)
type table
type item
type obj_init
type class_info
val set_initializer: table -> obj_init -> unit
val inheritance: table -> class_info -> unit
val get_method: table -> label -> item
val set_method: table -> label -> item -> unit
val get_variable: table -> string -> int
val hide_variable: table -> string -> unit
val get_private_variable: table -> string -> int
val create_class: (table -> unit) -> class_info

(* Objects *)
type t
type object
val send:	object -> label -> t

(* Statistics *)
type stats =
  { classes: int; labels: int; methods: int; inst_vars: int; buckets: int;
    distrib : int array; small_bucket_count: int; small_bucket_max: int }
val stats: unit -> stats
val show_buckets: unit -> unit
