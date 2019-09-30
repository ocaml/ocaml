(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Inclusion checks for the core language *)

open Typedtree
open Types

exception Dont_match

type position = Ctype.Unification_trace.position = First | Second

type label_mismatch =
  | Type
  | Mutability of position

type record_mismatch =
  | Label_mismatch of label_declaration * label_declaration * label_mismatch
  | Label_names of int * Ident.t * Ident.t
  | Label_missing of position * Ident.t
  | Unboxed_float_representation of position

type constructor_mismatch =
  | Type
  | Arity
  | Inline_record of record_mismatch
  | Kind of position
  | Explicit_return_type of position

type variant_mismatch =
  | Constructor_mismatch of constructor_declaration
                            * constructor_declaration
                            * constructor_mismatch
  | Constructor_names of int * Ident.t * Ident.t
  | Constructor_missing of position * Ident.t

type extension_constructor_mismatch =
  | Constructor_privacy
  | Constructor_mismatch of Ident.t
                            * extension_constructor
                            * extension_constructor
                            * constructor_mismatch

type type_mismatch =
  | Arity
  | Privacy
  | Kind
  | Constraint
  | Manifest
  | Variance
  | Record_mismatch of record_mismatch
  | Variant_mismatch of variant_mismatch
  | Unboxed_representation of position
  | Immediate of Type_immediacy.Violation.t

val value_descriptions:
  loc:Location.t -> Env.t -> string ->
  value_description -> value_description -> module_coercion

val type_declarations:
  ?equality:bool ->
  loc:Location.t ->
  Env.t -> mark:bool -> string ->
  type_declaration -> Path.t -> type_declaration -> type_mismatch option

val extension_constructors:
  loc:Location.t -> Env.t -> mark:bool -> Ident.t ->
  extension_constructor -> extension_constructor ->
  extension_constructor_mismatch option
(*
val class_types:
        Env.t -> class_type -> class_type -> bool
*)

val report_type_mismatch:
    string -> string -> string -> Format.formatter -> type_mismatch -> unit
val report_extension_constructor_mismatch: string -> string -> string ->
  Format.formatter -> extension_constructor_mismatch -> unit
