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

type position = Errortrace.position = First | Second

type primitive_mismatch =
  | Name
  | Arity
  | No_alloc of position
  | Native_name
  | Result_repr
  | Argument_repr of int

type value_mismatch =
  | Primitive_mismatch of primitive_mismatch
  | Not_a_primitive
  | Type of Env.t * Errortrace.comparison Errortrace.t

exception Dont_match of value_mismatch

type label_mismatch =
  | Type of Env.t * Errortrace.comparison Errortrace.t
  | Mutability of position

type record_mismatch =
  | Label_mismatch of label_declaration * label_declaration * label_mismatch
  | Label_names of int * Ident.t * Ident.t
  | Label_missing of position * Ident.t
  | Unboxed_float_representation of position

type constructor_mismatch =
  | Type of Env.t * Errortrace.comparison Errortrace.t
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

type private_variant_mismatch =
  | Openness
  | Missing of position * string
  | Presence of string
  | Incompatible_types_for of string
  | Types of Env.t * Errortrace.comparison Errortrace.t

type private_object_mismatch =
  | Missing of string
  | Types of Env.t * Errortrace.comparison Errortrace.t

type type_mismatch =
  | Arity
  | Privacy
  | Kind
  | Constraint of Env.t * Errortrace.comparison Errortrace.t
  | Manifest of Env.t * Errortrace.comparison Errortrace.t
  | Private_variant of type_expr * type_expr * private_variant_mismatch
  | Private_object of type_expr * type_expr * private_object_mismatch
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
