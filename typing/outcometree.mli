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

(* These types represent values, types and signature items that the toplevel
   displays as normal results or errors messages. The real displaying is
   customisable using the hooks:
      [Toploop.print_out_value]
      [Toploop.print_out_type]
      [Toploop.print_out_sig_item] *)

type out_ident =
  | Oide_apply of out_ident * out_ident
  | Oide_dot of out_ident * string
  | Oide_ident of string

type out_value =
  | Oval_array of out_value list
  | Oval_char of char
  | Oval_constr of out_ident * out_value list
  | Oval_ellipsis
  | Oval_float of float
  | Oval_int of int
  | Oval_list of out_value list
  | Oval_printer of (Format.formatter -> unit)
  | Oval_record of (out_ident * out_value) list
  | Oval_string of string
  | Oval_stuff of string
  | Oval_tuple of out_value list
  | Oval_variant of string * out_value option

type out_type =
  | Otyp_abstract
  | Otyp_alias of out_type * string
  | Otyp_arrow of string * out_type * out_type
  | Otyp_class of bool * out_ident * out_type list * string list
  | Otyp_constr of out_ident * out_type list
  | Otyp_manifest of out_type * out_type
  | Otyp_object of (string * out_type) list * bool option
  | Otyp_record of (string * bool * out_type) list
  | Otyp_stuff of string
  | Otyp_sum of (string * out_type list) list
  | Otyp_tuple of out_type list
  | Otyp_var of bool * string
  | Otyp_variant of
      bool * (string * bool * out_type list) list * bool * (string list) option

type out_module_type =
  | Omty_abstract
  | Omty_functor of string * out_module_type * out_module_type
  | Omty_ident of out_ident
  | Omty_signature of out_sig_item list
and out_sig_item =
  | Osig_exception of string * out_type list
  | Osig_modtype of string * out_module_type
  | Osig_module of string * out_module_type
  | Osig_printer of (Format.formatter -> unit)
  | Osig_type of out_type_decl list
  | Osig_value of string * out_type * string list
and out_type_decl =
  string * (string * (bool * bool)) list * out_type *
  (out_type * out_type) list
