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

exception Type_error of type_repr * type_repr

external type_of : dyn -> type_repr = "%field0"

(*--*)
type anything
val coerce_internal : dyn -> type_repr -> anything
