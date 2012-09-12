(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*                  Projet Cristal, INRIA Rocquencourt                 *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format
open Outcometree
open Types

val out_value : (formatter -> out_value -> unit) ref
val out_type : (formatter -> out_type -> unit) ref
val out_class_type : (formatter -> out_class_type -> unit) ref
val out_module_type : (formatter -> out_module_type -> unit) ref
val out_sig_item : (formatter -> out_sig_item -> unit) ref
val out_signature : (formatter -> out_sig_item list -> unit) ref
val out_phrase : (formatter -> out_phrase -> unit) ref
val out_contract_declaration : (string * type_declaration) list ->
    (formatter -> Types.contract_declaration -> unit) ref
val out_core_contract : (string * type_declaration) list ->
    (formatter -> Types.core_contract -> unit) ref
val out_expression : (string * type_declaration) list ->
    (formatter -> Types.expression -> unit) ref
val out_expression_desc : (string * type_declaration) list ->
    (formatter -> Types.expression_desc -> unit) ref
val out_pattern_desc : (string * type_declaration) list ->
    (formatter -> Types.pattern_desc -> unit) ref

val parenthesized_ident : string -> bool
