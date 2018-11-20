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

(* Typing of type definitions and primitive definitions *)

open Types
open Format

val transl_type_decl:
    Env.t -> Asttypes.rec_flag -> Parsetree.type_declaration list ->
    Typedtree.type_declaration list * Env.t

val transl_exception:
    Env.t -> Parsetree.extension_constructor ->
    Typedtree.extension_constructor * Env.t

val transl_type_exception:
    Env.t ->
    Parsetree.type_exception -> Typedtree.type_exception * Env.t

val transl_type_extension:
    bool -> Env.t -> Location.t -> Parsetree.type_extension ->
    Typedtree.type_extension * Env.t

val transl_value_decl:
    Env.t -> Location.t ->
    Parsetree.value_description -> Typedtree.value_description * Env.t

val transl_with_constraint:
    Env.t -> Ident.t -> Path.t option -> Types.type_declaration ->
    Parsetree.type_declaration -> Typedtree.type_declaration

val abstract_type_decl: int -> type_declaration
val approx_type_decl:
    Parsetree.type_declaration list ->
                                  (Ident.t * type_declaration) list
val check_recmod_typedecl:
    Env.t -> Location.t -> Ident.t list -> Path.t -> type_declaration -> unit
val check_coherence:
    Env.t -> Location.t -> Ident.t -> type_declaration -> unit

(* for fixed types *)
val is_fixed_type : Parsetree.type_declaration -> bool

(* for typeopt.ml *)
val get_unboxed_type_representation: Env.t -> type_expr -> type_expr option

type native_repr_kind = Unboxed | Untagged

type error =
    Repeated_parameter
  | Duplicate_constructor of string
  | Too_many_constructors
  | Duplicate_label of string
  | Recursive_abbrev of string
  | Cycle_in_def of string * type_expr
  | Definition_mismatch of type_expr * Includecore.type_mismatch option
  | Constraint_failed of type_expr * type_expr
  | Inconsistent_constraint of Env.t * Ctype.Unification_trace.t
  | Type_clash of Env.t * Ctype.Unification_trace.t
  | Parameters_differ of Path.t * type_expr * type_expr
  | Null_arity_external
  | Missing_native_external
  | Unbound_type_var of type_expr * type_declaration
  | Cannot_extend_private_type of Path.t
  | Not_extensible_type of Path.t
  | Extension_mismatch of Path.t * Includecore.type_mismatch
  | Rebind_wrong_type of Longident.t * Env.t * Ctype.Unification_trace.t
  | Rebind_mismatch of Longident.t * Path.t * Path.t
  | Rebind_private of Longident.t
  | Variance of Typedecl_variance.error
  | Unavailable_type_constructor of Path.t
  | Bad_fixed_type of string
  | Unbound_type_var_ext of type_expr * extension_constructor
  | Val_in_structure
  | Multiple_native_repr_attributes
  | Cannot_unbox_or_untag_type of native_repr_kind
  | Deep_unbox_or_untag_attribute of native_repr_kind
  | Immediacy of Typedecl_immediacy.error
  | Bad_unboxed_attribute of string
  | Wrong_unboxed_type_float
  | Boxed_and_unboxed
  | Nonrec_gadt

exception Error of Location.t * error

val report_error: formatter -> error -> unit
