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

(* Module [Outcometree]: results displayed by the toplevel *)

(* These types represent values and types that the toplevel displays
   as normal results or errors messages. The real displaying is customisable
   using the hooks [Toploop.print_out_value] and [Toploop.print_out_type]. *)

type out_value =
    Oval_int of int
  | Oval_float of float
  | Oval_char of char
  | Oval_string of string
  | Oval_constr of out_ident * out_value list
  | Oval_variant of string * out_value option
  | Oval_list of out_value list
  | Oval_array of out_value list
  | Oval_tuple of out_value list
  | Oval_record of (out_ident * out_value) list
  | Oval_stuff of string
  | Oval_ellipsis
  | Oval_printer of (Format.formatter -> unit)
and out_ident =
    Oide_ident of string
  | Oide_dot of out_ident * string
  | Oide_apply of out_ident * out_ident

type out_type =
    Otyp_alias of out_type * string
  | Otyp_var of bool * string
  | Otyp_arrow of string * out_type * out_type
  | Otyp_tuple of out_type list
  | Otyp_constr of out_ident * out_type list
  | Otyp_object of (string * out_type) list * bool option
  | Otyp_class of bool * out_ident * out_type list * string list
  | Otyp_variant of
      bool * (string * bool * out_type list) list * bool * (string list) option
  | Otyp_stuff of string
