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

type type_expr = int

type type_repr = {
    expr : type_expr;
  }

type anything

exception Type_error of type_repr * type_repr

let coerce_internal (sent_type, v) expected_type =
  if sent_type.expr = expected_type.expr
  then (Obj.magic v : anything)
  else raise (Type_error (sent_type, expected_type))
