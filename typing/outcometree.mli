(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*     Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format

type out_value =
    Oval_int of int
  | Oval_float of float
  | Oval_char of char
  | Oval_string of string
  | Oval_list of out_value list
  | Oval_array of out_value list
  | Oval_constr of out_ident * out_value list
  | Oval_variant of string * out_value option
  | Oval_stuff of string
  | Oval_tuple of out_value list
  | Oval_record of (out_ident * out_value) list
  | Oval_ellipsis
  | Oval_printer of (formatter -> unit)
and out_ident =
    Oide_ident of string
  | Oide_dot of out_ident * string
  | Oide_apply of out_ident * out_ident
