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

val copy : < .. > as 'a -> 'a
        (* [Oo.copy o] returns a copy of object [o], that is a fresh
           object with the same methods and instance variables as [o]  *)

(*--*)

(*** For system use only, not for the casual user *)

(* Methods *)
type label
val new_method: string -> label

(* Classes *)
type table
type meth
type obj_init
type class_info
type t
type obj
val get_class: table -> class_info -> obj_init
val new_variable: table -> string -> int
val get_variable: table -> string -> int
val copy_variables: class_info -> table -> unit -> obj -> unit
val get_method_label: table -> string -> label
val get_method: table -> label -> meth
val set_method: table -> label -> meth -> unit
val narrow: table -> string list -> string list -> string list -> unit
val widen: table -> unit
val add_initializer: table -> (obj -> unit) -> unit
val create_table: string list -> table
val init_class: table -> unit

(* Objects *)
val create_object: table -> obj
val run_initializers: obj -> table -> unit
val object_from_struct: class_info -> obj
val send:   obj -> label -> t

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
