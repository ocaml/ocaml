(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Run-time support for objects and classes.
    All functions in this module are for system use only, not for the
    casual user. *)

(** {6 Methods} *)

type label
val new_method : string -> label
val public_method_label : string -> label

(** {6 Classes} *)

type table
type meth
type t
type obj
val new_variable : table -> string -> int
val get_variable : table -> string -> int
val get_method_label : table -> string -> label
val get_method : table -> label -> meth
val set_method : table -> label -> meth -> unit
val narrow : table -> string list -> string list -> string list -> unit
val widen : table -> unit
val add_initializer : table -> (obj -> unit) -> unit
val dummy_table : table
val create_table : string list -> table
val init_class : table -> unit

(** {6 Objects} *)

val copy : (< .. > as 'a) -> 'a
val create_object : table -> obj
val create_object_opt : obj -> table -> obj
val run_initializers : obj -> table -> unit
val run_initializers_opt : obj -> obj -> table -> obj
val create_object_and_run_initializers : obj -> table -> obj
val send : obj -> label -> t

(** {6 Table cache} *)

type tables
val lookup_tables : tables -> table array -> tables

(** {6 Builtin methods} *)

open Obj
val ret_const : t -> obj -> t
val ret_var : int -> obj -> t
val ret_env : int -> int -> obj -> t
val ret_meth : label -> obj -> t
val set_var : int -> obj -> t -> unit
val app_const : (t -> t) -> t -> obj -> t
val app_var : (t -> t) -> int -> obj -> t
val app_env : (t -> t) -> int -> int -> obj -> t
val app_meth : (t -> t) -> label -> obj -> t
val app_const_const : (t -> t -> t) -> t -> t -> obj -> t
val app_const_var : (t -> t -> t) -> t -> int -> obj -> t
val app_const_env : (t -> t -> t) -> t -> int -> int -> obj -> t
val app_const_meth : (t -> t -> t) -> t -> label -> obj -> t
val app_var_const : (t -> t -> t) -> int -> t -> obj -> t
val app_env_const : (t -> t -> t) -> int -> int -> t -> obj -> t
val app_meth_const : (t -> t -> t) -> label -> t -> obj -> t

(** {6 Parameters} *)

type params =
  { mutable compact_table : bool;
    mutable copy_parent : bool;
    mutable clean_when_copying : bool;
    mutable retry_count : int;
    mutable bucket_small_size : int }

val params : params

(** {6 Statistics} *)

type stats =
  { classes : int; 
    labels : int; 
    methods : int; 
    inst_vars : int; 
    buckets : int;
    distrib : int array; 
    small_bucket_count : int; 
    small_bucket_max : int }
val stats : unit -> stats
val show_buckets : unit -> unit
