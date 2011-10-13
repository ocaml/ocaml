(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Basic operations on core types *)

open Asttypes
open Types

val generic_level: int

val newty2: int -> type_desc -> type_expr
        (* Create a type *)
val newgenty: type_desc -> type_expr
        (* Create a generic type *)
val newgenvar: ?name:string -> unit -> type_expr
        (* Return a fresh generic variable *)

(* Use Tsubst instead
val newmarkedvar: int -> type_expr
        (* Return a fresh marked variable *)
val newmarkedgenvar: unit -> type_expr
        (* Return a fresh marked generic variable *)
*)

val is_Tvar: type_expr -> bool
val is_Tunivar: type_expr -> bool

val repr: type_expr -> type_expr
        (* Return the canonical representative of a type. *)

val field_kind_repr: field_kind -> field_kind
        (* Return the canonical representative of an object field
           kind. *)

val commu_repr: commutable -> commutable
        (* Return the canonical representative of a commutation lock *)

val row_repr: row_desc -> row_desc
        (* Return the canonical representative of a row description *)
val row_field_repr: row_field -> row_field
val row_field: label -> row_desc -> row_field
        (* Return the canonical representative of a row field *)
val row_more: row_desc -> type_expr
        (* Return the extension variable of the row *)
val static_row: row_desc -> bool
        (* Return whether the row is static or not *)
val hash_variant: label -> int
        (* Hash function for variant tags *)

val proxy: type_expr -> type_expr
        (* Return the proxy representative of the type: either itself
           or a row variable *)

(**** Utilities for private abbreviations with fixed rows ****)
val has_constr_row: type_expr -> bool
val is_row_name: string -> bool

(**** Utilities for type traversal ****)

val iter_type_expr: (type_expr -> unit) -> type_expr -> unit
        (* Iteration on types *)
val iter_row: (type_expr -> unit) -> row_desc -> unit
        (* Iteration on types in a row *)
val iter_abbrev: (type_expr -> unit) -> abbrev_memo -> unit
        (* Iteration on types in an abbreviation list *)

val copy_type_desc: (type_expr -> type_expr) -> type_desc -> type_desc
        (* Copy on types *)
val copy_row:
    (type_expr -> type_expr) ->
    bool -> row_desc -> bool -> type_expr -> row_desc
val copy_kind: field_kind -> field_kind

val save_desc: type_expr -> type_desc -> unit
        (* Save a type description *)
val dup_kind: field_kind option ref -> unit
        (* Save a None field_kind, and make it point to a fresh Fvar *)
val cleanup_types: unit -> unit
        (* Restore type descriptions *)

val lowest_level: int
        (* Marked type: ty.level < lowest_level *)
val pivot_level: int
        (* Type marking: ty.level <- pivot_level - ty.level *)
val mark_type: type_expr -> unit
        (* Mark a type *)
val mark_type_node: type_expr -> unit
        (* Mark a type node (but not its sons) *)
val mark_type_params: type_expr -> unit
        (* Mark the sons of a type node *)
val unmark_type: type_expr -> unit
val unmark_type_decl: type_declaration -> unit
val unmark_class_type: class_type -> unit
val unmark_class_signature: class_signature -> unit
        (* Remove marks from a type *)

(**** Memorization of abbreviation expansion ****)

val find_expans: private_flag -> Path.t -> abbrev_memo -> type_expr option
        (* Look up a memorized abbreviation *)
val cleanup_abbrev: unit -> unit
        (* Flush the cache of abbreviation expansions.
           When some types are saved (using [output_value]), this
           function MUST be called just before. *)
val memorize_abbrev:
        abbrev_memo ref ->
        private_flag -> Path.t -> type_expr -> type_expr -> unit
        (* Add an expansion in the cache *)
val forget_abbrev:
        abbrev_memo ref -> Path.t -> unit
        (* Remove an abbreviation from the cache *)

(**** Utilities for labels ****)

val is_optional : label -> bool
val label_name : label -> label
val extract_label :
    label -> (label * 'a) list ->
    label * 'a * (label * 'a) list * (label * 'a) list
    (* actual label, value, before list, after list *)

(**** Utilities for backtracking ****)

type snapshot
        (* A snapshot for backtracking *)
val snapshot: unit -> snapshot
        (* Make a snapshot for later backtracking. Costs nothing *)
val backtrack: snapshot -> unit
        (* Backtrack to a given snapshot. Only possible if you have
           not already backtracked to a previous snapshot.
           Calls [cleanup_abbrev] internally *)

(* Functions to use when modifying a type (only Ctype?) *)
val link_type: type_expr -> type_expr -> unit
        (* Set the desc field of [t1] to [Tlink t2], logging the old
           value if there is an active snapshot *)
val set_level: type_expr -> int -> unit
val set_name:
    (Path.t * type_expr list) option ref ->
    (Path.t * type_expr list) option -> unit
val set_row_field: row_field option ref -> row_field -> unit
val set_univar: type_expr option ref -> type_expr -> unit
val set_kind: field_kind option ref -> field_kind -> unit
val set_commu: commutable ref -> commutable -> unit
        (* Set references, logging the old value *)
val log_type: type_expr -> unit
        (* Log the old value of a type, before modifying it by hand *)

(**** Sets, maps and hashtables of types ****)

module TypeSet  : Set.S with type elt = type_expr
module TypeMap  : Map.S with type key = type_expr
module TypeHash : Hashtbl.S with type key = type_expr

(**** Forward declarations ****)
val print_raw: (Format.formatter -> type_expr -> unit) ref
