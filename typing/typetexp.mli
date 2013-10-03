(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Typechecking of type expressions for the core language *)

open Format;;

val transl_simple_type:
        Env.t -> bool -> Parsetree.core_type -> Typedtree.core_type
val transl_simple_type_univars:
        Env.t -> Parsetree.core_type -> Typedtree.core_type
val transl_simple_type_delayed:
        Env.t -> Parsetree.core_type -> Typedtree.core_type * (unit -> unit)
        (* Translate a type, but leave type variables unbound. Returns
           the type and a function that binds the type variable. *)
val transl_type_scheme:
        Env.t -> Parsetree.core_type -> Typedtree.core_type
val reset_type_variables: unit -> unit
val enter_type_variable: bool -> Location.t -> string -> Types.type_expr
val type_variable: Location.t -> string -> Types.type_expr
val transl_type_param: 
  Env.t -> bool -> Parsetree.core_type -> Typedtree.core_type

type variable_context
val narrow: unit -> variable_context
val widen: variable_context -> unit

exception Already_bound

type error =
    Unbound_type_variable of string
  | Unbound_type_constructor of Env.t * Longident.t
  | Unbound_type_constructor_2 of Path.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
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
  | Multiple_constraints_on_type of Longident.t
  | Repeated_method_label of string
  | Unbound_value of Env.t * Longident.t
  | Unbound_constructor of Env.t * Longident.t
  | Unbound_label of Env.t * Longident.t
  | Unbound_module of Env.t * Longident.t
  | Unbound_class of Env.t * Longident.t
  | Unbound_modtype of Env.t * Longident.t
  | Unbound_cltype of Env.t * Longident.t
  | Ill_typed_functor_application of Longident.t

exception Error of Location.t * error

val report_error: formatter -> error -> unit

(* Support for first-class modules. *)
val transl_modtype_longident:  (* from Typemod *)
    (Location.t -> Env.t -> Longident.t -> Path.t) ref
val transl_modtype: (* from Typemod *)
    (Env.t -> Parsetree.module_type -> Typedtree.module_type) ref
val create_package_mty:
    Location.t -> Env.t -> Parsetree.package_type ->
    (Longident.t Asttypes.loc * Parsetree.core_type) list *
      Parsetree.module_type

val find_type:
    Env.t -> Location.t -> Longident.t -> Path.t * Types.type_declaration
val find_constructor:
    Env.t -> Location.t -> Longident.t -> Types.constructor_description
val find_label:
    Env.t -> Location.t -> Longident.t -> Types.label_description
val find_value:
    Env.t -> Location.t -> Longident.t -> Path.t * Types.value_description
val find_class:
    Env.t -> Location.t -> Longident.t -> Path.t * Types.class_declaration
val find_module:
    Env.t -> Location.t -> Longident.t -> Path.t * Types.module_type
val find_modtype:
    Env.t -> Location.t -> Longident.t -> Path.t * Types.modtype_declaration
val find_class_type:
    Env.t -> Location.t -> Longident.t -> Path.t * Types.class_type_declaration
