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

(* Printing functions *)

open Types

val longident: Longident.t -> unit
val ident: Ident.t -> unit
val path: Path.t -> unit
val reset: unit -> unit
val mark_loops: type_expr -> unit
val type_expr: type_expr -> unit
val type_scheme: type_expr -> unit
val value_description: Ident.t -> value_description -> unit
val type_declaration: Ident.t -> type_declaration -> unit
val exception_declaration: Ident.t -> exception_declaration -> unit
val modtype: module_type -> unit
val signature: signature -> unit
val signature_body: bool -> signature -> unit
val modtype_declaration: Ident.t -> modtype_declaration -> unit
val class_type: class_type -> unit
val class_declaration: Ident.t -> class_declaration -> unit
val cltype_declaration: Ident.t -> cltype_declaration -> unit
val type_expansion: type_expr -> type_expr -> unit
val trace: bool -> (unit -> unit) -> (type_expr * type_expr) list -> unit
val unification_error:
        bool -> (type_expr * type_expr) list ->
        (unit -> unit) -> (unit -> unit) ->
        unit
