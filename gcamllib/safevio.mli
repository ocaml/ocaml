(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                   Jun Furuse, University of Tokyo                   *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Rtype

type fingerprint =
    BuiltinBase of string
  | Digest of Digest.t
  | Recursive of string

type fingerprint_type_expr = fingerprint raw_type_expr
type type_signature = fingerprint raw_type_declaration list

val type_expr : type_expr -> fingerprint_type_expr
val fingerprint : type_declaration -> fingerprint
val type_signature : type_declaration -> type_signature

val print_type_signature : Format.formatter -> type_declaration -> unit

val string_of_type_signature : type_declaration -> string

val output_value : { 'a } => out_channel -> 'a -> unit
val input_value : { 'a } => in_channel -> 'a
