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

type type_repr
(* type dyn = type_repr * 'a with (t, v : dyn) <==> v : t *)
type module_type_repr
(* type dynamically_typed_module = module_type_repr * 'a
   with (S, M : dynamically_typed_module) <==> M : S *)

exception Type_error of type_repr * type_repr
exception Module_type_error of module_type_repr * module_type_repr

external type_of : dyn -> type_repr = "%field0"
external module_type_of : dynamically_typed_module -> module_type_repr = "%field0"

(*--*)
type anything
type nothing
val coerce_internal : dyn -> type_repr -> anything
val coerce_module : dynamically_typed_module -> module_type_repr -> nothing
