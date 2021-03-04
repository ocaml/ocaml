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

(* Typechecking of type expressions for the core language *)

open Types

val valid_tyvar_name : string -> bool

val transl_simple_type:
        Env.t -> bool -> Parsetree.core_type -> Typedtree.core_type
val transl_simple_type_univars:
        Env.t -> Parsetree.core_type -> Typedtree.core_type
val transl_simple_type_delayed
  :  Env.t
  -> Parsetree.core_type
  -> Typedtree.core_type * type_expr * (unit -> unit)
        (* Translate a type, but leave type variables unbound. Returns
           the type, an instance of the corresponding type_expr, and a
           function that binds the type variable. *)
val transl_type_scheme:
        Env.t -> Parsetree.core_type -> Typedtree.core_type
val reset_type_variables: unit -> unit
val type_variable: Location.t -> string -> type_expr
val transl_type_param:
  Env.t -> Parsetree.core_type -> Typedtree.core_type

type variable_context
val narrow: unit -> variable_context
val widen: variable_context -> unit

exception Already_bound

type error =
    Unbound_type_variable of string
  | Undefined_type_constructor of Path.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Unbound_row_variable of Longident.t
  | Type_mismatch of Ctype.Unification_trace.t
  | Alias_type_mismatch of Ctype.Unification_trace.t
  | Present_has_conjunction of string
  | Present_has_no_type of string
  | Constructor_mismatch of type_expr * type_expr
  | Not_a_variant of type_expr
  | Variant_tags of string * string
  | Invalid_variable_name of string
  | Cannot_quantify of string * type_expr
  | Multiple_constraints_on_type of Longident.t
  | Method_mismatch of string * type_expr * type_expr
  | Opened_object of Path.t option
  | Not_an_object of type_expr

exception Error of Location.t * Env.t * error

val report_error: Env.t -> Format.formatter -> error -> unit

(* Support for first-class modules. *)
val transl_modtype_longident:  (* from Typemod *)
    (Location.t -> Env.t -> Longident.t -> Path.t) ref
val transl_modtype: (* from Typemod *)
    (Env.t -> Parsetree.module_type -> Typedtree.module_type) ref
val create_package_mty:
    Location.t -> Env.t -> Parsetree.package_type ->
    (Longident.t Asttypes.loc * Parsetree.core_type) list *
      Parsetree.module_type
