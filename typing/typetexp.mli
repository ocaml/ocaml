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
val type_variable: Location.t -> string -> type_expr
val transl_type_param:
  Env.t -> Parsetree.core_type -> Typedtree.core_type

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
  | Unbound_value of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Unbound_module of Longident.t
  | Unbound_class of Longident.t
  | Unbound_modtype of Longident.t
  | Unbound_cltype of Longident.t
  | Ill_typed_functor_application
      of Longident.t * Longident.t * Includemod.error list option
  | Illegal_reference_to_recursive_module
  | Wrong_use_of_module of Longident.t * [ `Structure_used_as_functor
                                         | `Abstract_used_as_functor
                                         | `Functor_used_as_structure
                                         | `Abstract_used_as_structure
                                         | `Generative_used_as_applicative
                                         ]
  | Cannot_scrape_alias of Longident.t * Path.t
  | Opened_object of Path.t option
  | Not_an_object of type_expr
  | Unbound_value_missing_rec of Longident.t * Location.t

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

val find_type:
    Env.t -> Location.t -> Longident.t -> Path.t * type_declaration
val find_constructor:
    Env.t -> Location.t -> Longident.t -> constructor_description
val find_all_constructors:
    Env.t -> Location.t -> Longident.t ->
    (constructor_description * (unit -> unit)) list
val find_label:
    Env.t -> Location.t -> Longident.t -> label_description
val find_all_labels:
    Env.t -> Location.t -> Longident.t ->
    (label_description * (unit -> unit)) list
val find_value:
    Env.t -> Location.t -> Longident.t -> Path.t * value_description
val find_class:
    Env.t -> Location.t -> Longident.t -> Path.t * class_declaration
val find_module:
    Env.t -> Location.t -> Longident.t -> Path.t * module_declaration
val lookup_module:
    ?load:bool -> Env.t -> Location.t -> Longident.t -> Path.t
val find_modtype:
    Env.t -> Location.t -> Longident.t -> Path.t * modtype_declaration
val find_class_type:
    Env.t -> Location.t -> Longident.t -> Path.t * class_type_declaration

val unbound_constructor_error: Env.t -> Longident.t Location.loc -> 'a
val unbound_label_error: Env.t -> Longident.t Location.loc -> 'a

(* To update location from typemod errors *)
val typemod_update_location: (Location.t -> exn -> exn) ref
