(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Typechecking of type expressions for the core language *)

open Format;;

val transl_simple_type:
        Env.t -> bool -> Parsetree.core_type -> Types.type_expr
val transl_simple_type_univars:
        Env.t -> Parsetree.core_type -> Types.type_expr
val transl_simple_type_delayed:
        Env.t -> Parsetree.core_type -> Types.type_expr * (unit -> unit)
        (* Translate a type, but leave type variables unbound. Returns
           the type and a function that binds the type variable. *)
val transl_type_scheme:
        Env.t -> Parsetree.core_type -> Types.type_expr
val reset_type_variables: unit -> unit
val enter_type_variable: bool -> Location.t -> string -> Types.type_expr
val type_variable: Location.t -> string -> Types.type_expr

type variable_context
val narrow: unit -> variable_context
val widen: variable_context -> unit

exception Already_bound

type error =
    Unbound_type_variable of string
  | Unbound_type_constructor of Longident.t
  | Unbound_type_constructor_2 of Path.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Unbound_class of Longident.t
  | Unbound_row_variable of Longident.t
  | Type_mismatch of (Types.type_expr * Types.type_expr) list
  | Alias_type_mismatch of (Types.type_expr * Types.type_expr) list
  | Present_has_conjunction of string
  | Present_has_no_type of string
  | Constructor_mismatch of Types.type_expr * Types.type_expr
  | Not_a_variant of Types.type_expr
  | Variant_tags of string * string
  | Invalid_variable_name of string
  | Cannot_quantify of string * Types.type_expr
  | Repeated_method_label of string

exception Error of Location.t * error

val report_error: formatter -> error -> unit
