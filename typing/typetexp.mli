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

module TyVarEnv : sig
  (* this is just the subset of [TyVarEnv] that is needed outside
     of [Typetexp]. See the ml file for more. *)

  val reset : unit -> unit
  (** removes all type variables from scope *)

  val with_local_scope : (unit -> 'a) -> 'a
  (** Evaluate in a narrowed type-variable scope *)

  type poly_univars
  val make_poly_univars : string list -> poly_univars
    (** remember that a list of strings connotes univars; this must
        always be paired with a [check_poly_univars]. *)

  val check_poly_univars :
     Env.t -> Location.t -> poly_univars -> type_expr list
    (** Verify that the given univars are universally quantified,
       and return the list of variables. The type in which the
       univars are used must be generalised *)

  val instance_poly_univars :
     Env.t -> Location.t -> poly_univars -> type_expr list
    (** Same as [check_poly_univars], but instantiates the resulting
       type scheme (i.e. variables become Tvar rather than Tunivar) *)

end

(* Forward declaration, to be filled in by Typemod.type_open *)
val type_open:
  (?used_slot:bool ref -> Asttypes.override_flag -> Env.t -> Location.t ->
   Longident.t Asttypes.loc -> Path.t * Env.t)
    ref

val valid_tyvar_name : string -> bool

val transl_simple_type:
        Env.t -> ?univars:TyVarEnv.poly_univars -> closed:bool
        -> Parsetree.core_type -> Typedtree.core_type
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
val transl_type_param:
  Env.t -> Parsetree.core_type -> Typedtree.core_type

exception Already_bound

type error =
  | Unbound_type_variable of string * string list
  | No_type_wildcards
  | Undefined_type_constructor of Path.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Type_mismatch of Errortrace.unification_error
  | Alias_type_mismatch of Errortrace.unification_error
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

val report_error: Env.t -> error Format_doc.format_printer
val report_error_doc: Env.t -> error Format_doc.printer

(* Support for first-class modules. *)
val transl_modtype_longident:  (* from Typemod *)
    (Location.t -> Env.t -> Longident.t -> Path.t) ref
val transl_modtype: (* from Typemod *)
    (Env.t -> Parsetree.module_type -> Typedtree.module_type) ref
val check_package_with_type_constraints: (* from Typemod *)
    (Location.t -> Env.t -> Types.module_type ->
     (Longident.t Asttypes.loc * Typedtree.core_type) list ->
     Types.module_type) ref
