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

(* This type resembles [Types.type_desc] in the compiler, although it differs
   in quite a few ways. *)
type type_expr =
  | Pvar of int
  | Builtin of string * type_expr list
  | Tuple of type_expr list
  | Arrow of string * type_expr * type_expr * bool
  | Variant of string * type_expr list

type type_repr = {
    expr : type_expr;
  }

type anything

exception Type_error of type_repr * type_repr

let coerce_internal (sent_type, v) expected_type =
  if sent_type.expr = expected_type.expr
  then (Obj.magic v : anything)
  else raise (Type_error (sent_type, expected_type))
