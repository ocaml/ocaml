(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Gilles Peskine, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type type_bytes
type type_data
(* type dyn = type_bytes * 'a with (t, v : dyn) <==> v : t *)
type module_type_data
(* type dynamically_typed_module = module_type_data * 'a
   with (S, M : dynamically_typed_module) <==> M : S *)

exception Type_error of type_data * type_data
exception Module_type_error of module_type_data * module_type_data

val type_of : dyn -> type_data
val module_type_of : dynamically_typed_module -> module_type_data

(*--*)
type anything
type nothing
val coerce_internal : dyn -> type_bytes -> anything
val coerce_module : dynamically_typed_module -> module_type_data -> nothing
