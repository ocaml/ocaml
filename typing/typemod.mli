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

(** Type-checking of the module language and typed ast plugin hooks

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Types
open Format

module Signature_names : sig
  type t

  val simplify: Env.t -> t -> signature -> signature
end

val type_module:
        Env.t -> Parsetree.module_expr -> Typedtree.module_expr
val type_structure:
  Env.t -> Parsetree.structure -> Location.t ->
  Typedtree.structure * Types.signature * Signature_names.t * Env.t
val type_toplevel_phrase:
  Env.t -> Parsetree.structure ->
  Typedtree.structure * Types.signature * Signature_names.t * Env.t
val type_implementation:
  string -> string -> string -> Env.t -> Parsetree.structure ->
  Typedtree.structure * Typedtree.module_coercion
val type_interface:
        string -> Env.t -> Parsetree.signature -> Typedtree.signature
val transl_signature:
        Env.t -> Parsetree.signature -> Typedtree.signature
val check_nongen_schemes:
        Env.t -> Types.signature -> unit
        (*
val type_open_:
        ?used_slot:bool ref -> ?toplevel:bool ->
        Asttypes.override_flag ->
        Env.t -> Location.t -> Longident.t Asttypes.loc -> Path.t * Env.t
        *)
val modtype_of_package:
        Env.t -> Location.t ->
        Path.t -> Longident.t list -> type_expr list -> module_type

val path_of_module : Typedtree.module_expr -> Path.t option

val save_signature:
  string -> Typedtree.signature -> string -> string ->
  Env.t -> Cmi_format.cmi_infos -> unit

val package_units:
  Env.t -> string list -> string -> string -> Typedtree.module_coercion

(* Should be in Envaux, but it breaks the build of the debugger *)
val initial_env:
  loc:Location.t -> safe_string:bool ->
  initially_opened_module:string option ->
  open_implicit_modules:string list -> Env.t

module Sig_component_kind : sig
  type t =
    | Value
    | Type
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  val to_string : t -> string
end

type hiding_error =
  | Illegal_shadowing of {
      shadowed_item_id: Ident.t;
      shadowed_item_kind: Sig_component_kind.t;
      shadowed_item_loc: Location.t;
      shadower_id: Ident.t;
      user_id: Ident.t;
      user_kind: Sig_component_kind.t;
      user_loc: Location.t;
    }
  | Appears_in_signature of {
      opened_item_id: Ident.t;
      opened_item_kind: Sig_component_kind.t;
      user_id: Ident.t;
      user_kind: Sig_component_kind.t;
      user_loc: Location.t;
    }

type error =
    Cannot_apply of module_type
  | Not_included of Includemod.error list
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Includemod.error list
  | With_makes_applicative_functor_ill_typed of
      Longident.t * Path.t * Includemod.error list
  | With_changes_module_alias of Longident.t * Ident.t * Path.t
  | With_cannot_remove_constrained_type
  | Repeated_name of Sig_component_kind.t * string
  | Non_generalizable of type_expr
  | Non_generalizable_class of Ident.t * class_declaration
  | Non_generalizable_module of module_type
  | Implementation_is_required of string
  | Interface_not_compiled of string
  | Not_allowed_in_functor_body
  | Not_a_packed_module of type_expr
  | Incomplete_packed_module of type_expr
  | Scoping_pack of Longident.t * type_expr
  | Recursive_module_require_explicit_type
  | Apply_generative
  | Cannot_scrape_alias of Path.t
  | Badly_formed_signature of string * Typedecl.error
  | Cannot_hide_id of hiding_error
  | Invalid_type_subst_rhs

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

val report_error: Env.t -> formatter -> error -> unit


module ImplementationHooks : Misc.HookSig
  with type t = Typedtree.structure * Typedtree.module_coercion
module InterfaceHooks : Misc.HookSig
  with type t = Typedtree.signature
