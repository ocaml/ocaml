(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Printing functions *)

open Format
open Types

val longident: formatter -> Longident.t -> unit
val ident: formatter -> Ident.t -> unit
val path: formatter -> Path.t -> unit
val reset: unit -> unit
val mark_loops: type_expr -> unit
val reset_and_mark_loops: type_expr -> unit
val reset_and_mark_loops_list: type_expr list -> unit
val type_expr: formatter -> type_expr -> unit
val type_scheme: formatter -> type_expr -> unit
val value_description: Ident.t -> formatter -> value_description -> unit
val type_declaration: Ident.t -> formatter -> type_declaration -> unit
val exception_declaration: Ident.t -> formatter -> exception_declaration -> unit
val modtype: formatter -> module_type -> unit
val signature: formatter -> signature -> unit
val signature_body: bool -> formatter -> signature -> unit
val modtype_declaration: Ident.t -> formatter -> modtype_declaration -> unit
val class_type: formatter -> class_type -> unit
val class_declaration: Ident.t -> formatter -> class_declaration -> unit
val cltype_declaration: Ident.t -> formatter -> cltype_declaration -> unit
val type_expansion: type_expr -> Format.formatter -> type_expr -> unit
val prepare_expansion: type_expr * type_expr -> type_expr * type_expr
val trace: bool -> string -> formatter -> (type_expr * type_expr) list -> unit
val unification_error:
    bool -> (type_expr * type_expr) list ->
    (formatter -> unit) -> formatter -> (formatter -> unit) ->
    unit
val report_unification_error:
    formatter -> (type_expr * type_expr) list ->
    (formatter -> unit) -> (formatter -> unit) ->
    unit

val tree_of_path: Path.t -> Outcometree.out_ident
val outcome_type_hook: (formatter -> Outcometree.out_type -> unit) ref
