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

(* Inclusion checks for the module language *)

open Typedtree
open Types
open Format

(** Type describing which arguments of an inclusion to consider as used
    for the usage warnings. [Mark_both] is the default. *)
type mark =
  | Mark_both
      (** Mark definitions used from both arguments *)
  | Mark_positive
      (** Mark definitions used from the positive (first) argument *)
  | Mark_negative
      (** Mark definitions used from the negative (second) argument *)
  | Mark_neither
      (** Do not mark definitions used from either argument *)

val modtypes:
  loc:Location.t -> Env.t -> ?mark:mark ->
  module_type -> module_type -> module_coercion

val check_modtype_inclusion :
  loc:Location.t -> Env.t -> Types.module_type -> Path.t -> Types.module_type ->
  unit
(** [check_modtype_inclusion ~loc env mty1 path1 mty2] checks that the
    functor application F(M) is well typed, where mty2 is the type of
    the argument of F and path1/mty1 is the path/unstrenghened type of M. *)

val signatures: Env.t -> ?mark:mark ->
  signature -> signature -> module_coercion

val compunit:
      Env.t -> ?mark:mark -> string -> signature ->
      string -> signature -> module_coercion

val type_declarations:
  loc:Location.t -> Env.t -> ?mark:mark ->
  Ident.t -> type_declaration -> type_declaration -> unit

val print_coercion: formatter -> module_coercion -> unit

type symptom =
    Missing_field of Ident.t * Location.t * string (* kind *)
  | Value_descriptions of Ident.t * value_description * value_description
  | Type_declarations of Ident.t * type_declaration
        * type_declaration * Includecore.type_mismatch
  | Extension_constructors of Ident.t * extension_constructor
        * extension_constructor * Includecore.type_mismatch
  | Module_types of module_type * module_type
  | Modtype_infos of Ident.t * modtype_declaration * modtype_declaration
  | Modtype_permutation
  | Interface_mismatch of string * string
  | Class_type_declarations of
      Ident.t * class_type_declaration * class_type_declaration *
      Ctype.class_match_failure list
  | Class_declarations of
      Ident.t * class_declaration * class_declaration *
      Ctype.class_match_failure list
  | Unbound_modtype_path of Path.t
  | Unbound_module_path of Path.t
  | Invalid_module_alias of Path.t

type pos =
    Module of Ident.t | Modtype of Ident.t | Arg of Ident.t | Body of Ident.t
type error = pos list * Env.t * symptom

exception Error of error list

val report_error: formatter -> error list -> unit
val expand_module_alias: Env.t -> pos list -> Path.t -> Types.module_type
