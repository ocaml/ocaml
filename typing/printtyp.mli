(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Printing functions *)

open Typedtree

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
val signature_item: signature_item -> unit
val modtype_declaration: Ident.t -> modtype_declaration -> unit
val class_type: Ident.t -> class_type -> unit
