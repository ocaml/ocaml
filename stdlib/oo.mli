(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
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
        (* [Oo.copy o] returns a copy of object [o], that is a fresh
           object with the same methods and instance variables as [o]  *)

(*--*)

(*** For system use only, not for the casual user *)

(* Methods *)
type label
val new_method: string -> label

(* Classes *)
type t
type table
type item
type obj_init
type class_info
val set_initializer: table -> obj_init -> unit
val inheritance: table -> class_info -> string list -> string list -> unit
val get_method: table -> label -> item
val set_method: table -> string -> item -> unit
val get_method_label: table -> string -> label
val get_variable: table -> string -> int
val hide_variable: table -> string -> unit
val get_private_variable: table -> string -> int
val create_class:
        class_info -> string list -> (table -> t) ->
        (table -> unit) -> unit

(* Objects *)
type object
val create_object: table -> t
val send:   object -> label -> t

(* Parameters *)
type params = {
    mutable compact_table : bool;
    mutable copy_parent : bool;
    mutable clean_when_copying : bool;
    mutable retry_count : int;
    mutable bucket_small_size : int
  } 

val params : params

(* Statistics *)
type stats =
  { classes: int; labels: int; methods: int; inst_vars: int; buckets: int;
    distrib : int array; small_bucket_count: int; small_bucket_max: int }
val stats: unit -> stats
val show_buckets: unit -> unit
