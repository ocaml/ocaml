(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Inclusion checks for the module language *)

open Typedtree

val modtypes: Env.t -> module_type -> module_type -> module_coercion
val signatures: Env.t -> signature -> signature -> module_coercion
val compunit: string -> signature -> string -> signature -> module_coercion
val type_declarations:
      Env.t -> Ident.t -> type_declaration -> type_declaration -> unit

type error =
    Missing_field of Ident.t
  | Value_descriptions of Ident.t * value_description * value_description
  | Type_declarations of Ident.t * type_declaration * type_declaration
  | Exception_declarations of
      Ident.t * exception_declaration * exception_declaration
  | Module_types of module_type * module_type
  | Modtype_infos of Ident.t * modtype_declaration * modtype_declaration
  | Modtype_permutation
  | Interface_mismatch of string * string
  | Class_type of Ident.t * class_type * class_type

exception Error of error list

val report_error: error list -> unit
