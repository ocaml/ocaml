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

(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Asttypes
open Types

val constructor_descrs:
  type_expr -> (Ident.t * type_expr list * type_expr option) list ->
  private_flag -> (Ident.t * constructor_description) list
val exception_descr:
  Path.t -> exception_declaration -> constructor_description
val extension_descr:
  Path.t -> extension_constructor -> constructor_description
val label_descrs:
  type_expr -> (Ident.t * mutable_flag * type_expr) list ->
    record_representation -> private_flag ->
    (Ident.t * label_description) list

exception Constr_not_found

val find_constr_by_tag:
  constructor_tag -> (Ident.t * type_expr list * type_expr option) list ->
    Ident.t * type_expr list * type_expr option
