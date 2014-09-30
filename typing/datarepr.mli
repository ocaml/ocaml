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

(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Asttypes
open Types

val constructor_descrs:
  Path.t -> type_declaration ->
  constructor_declaration list ->
  (Ident.t * constructor_description) list

val extension_descr:
  ?rebind:Path.t ->
  Path.t -> extension_constructor ->
  constructor_description

val label_descrs:
  type_expr -> label_declaration list ->
    record_representation -> private_flag ->
    (Ident.t * label_description) list

exception Constr_not_found

val find_constr_by_tag:
  constructor_tag -> constructor_declaration list ->
    constructor_declaration


val free_vars: type_expr -> Btype.TypeSet.t * type_expr list
