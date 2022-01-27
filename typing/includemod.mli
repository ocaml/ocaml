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

module Error: sig

  type ('elt,'explanation) diff = {
    got:'elt;
    expected:'elt;
    symptom:'explanation
  }
  type 'elt core_diff =('elt,unit) diff

  type functor_arg_descr =
    | Anonymous
    | Named of Path.t
    | Unit

  type core_sigitem_symptom =
    | Value_descriptions of
        (Types.value_description, Includecore.value_mismatch) diff
    | Type_declarations of
        (Types.type_declaration, Includecore.type_mismatch) diff
    | Extension_constructors of
        (Types.extension_constructor,
         Includecore.extension_constructor_mismatch) diff
    | Class_type_declarations of
        (Types.class_type_declaration, Ctype.class_match_failure list) diff
    | Class_declarations of
        (Types.class_declaration, Ctype.class_match_failure list) diff

  type core_module_type_symptom =
    | Not_an_alias
    | Not_an_identifier
    | Incompatible_aliases
    | Abstract_module_type
    | Unbound_module_path of Path.t

  type module_type_symptom =
    | Mt_core of core_module_type_symptom
    | Signature of signature_symptom
    | Functor of functor_symptom
    | Invalid_module_alias of Path.t
    | After_alias_expansion of module_type_diff


  and module_type_diff = (Types.module_type, module_type_symptom) diff

  and functor_symptom =
    | Params of functor_params_diff
    | Result of module_type_diff

  and ('arg,'path) functor_param_symptom =
    | Incompatible_params of 'arg * Types.functor_parameter
    | Mismatch of module_type_diff

  and arg_functor_param_symptom =
    (Types.functor_parameter, Ident.t) functor_param_symptom

  and functor_params_diff =
    (Types.functor_parameter list * Types.module_type) core_diff

  and signature_symptom = {
    env: Env.t;
    missings: Types.signature_item list;
    incompatibles: (Ident.t * sigitem_symptom) list;
    oks: (int * Typedtree.module_coercion) list;
    leftovers: ((Types.signature_item as 'it) * 'it * int) list
    (** signature items that could not be compared due to type divergence *)
  }
  and sigitem_symptom =
    | Core of core_sigitem_symptom
    | Module_type_declaration of
        (Types.modtype_declaration, module_type_declaration_symptom) diff
    | Module_type of module_type_diff

  and module_type_declaration_symptom =
    | Illegal_permutation of Typedtree.module_coercion
    | Not_greater_than of module_type_diff
    | Not_less_than of module_type_diff
    | Incomparable of
        {less_than:module_type_diff; greater_than: module_type_diff}


  type all =
    | In_Compilation_unit of (string, signature_symptom) diff
    | In_Signature of signature_symptom
    | In_Module_type of module_type_diff
    | In_Module_type_substitution of
        Ident.t * (Types.module_type,module_type_declaration_symptom) diff
    | In_Type_declaration of Ident.t * core_sigitem_symptom
    | In_Expansion of core_module_type_symptom
end
type explanation = Env.t * Error.all

(* Extract name, kind and ident from a signature item *)
type field_kind =
  | Field_value
  | Field_type
  | Field_exception
  | Field_typext
  | Field_module
  | Field_modtype
  | Field_class
  | Field_classtype

type field_desc = { name: string; kind: field_kind }

val kind_of_field_desc: field_desc -> string
val field_desc: field_kind -> Ident.t -> field_desc

(** Map indexed by both field types and names.
    This avoids name clashes between different sorts of fields
    such as values and types. *)
module FieldMap: Map.S with type key = field_desc

val item_ident_name: Types.signature_item -> Ident.t * Location.t * field_desc
val is_runtime_component: Types.signature_item -> bool


(* Typechecking *)

val modtypes:
  loc:Location.t -> Env.t -> mark:mark ->
  module_type -> module_type -> module_coercion

val modtypes_with_shape:
  shape:Shape.t -> loc:Location.t -> Env.t -> mark:mark ->
  module_type -> module_type -> module_coercion * Shape.t

val strengthened_module_decl:
  loc:Location.t -> aliasable:bool -> Env.t -> mark:mark ->
  module_declaration -> Path.t -> module_declaration -> module_coercion

val check_modtype_inclusion :
  loc:Location.t -> Env.t -> Types.module_type -> Path.t -> Types.module_type ->
  explanation option
(** [check_modtype_inclusion ~loc env mty1 path1 mty2] checks that the
    functor application F(M) is well typed, where mty2 is the type of
    the argument of F and path1/mty1 is the path/unstrenghened type of M. *)

val check_modtype_equiv:
  loc:Location.t -> Env.t -> Ident.t -> module_type -> module_type -> unit

val signatures: Env.t -> mark:mark ->
  signature -> signature -> module_coercion

val compunit:
      Env.t -> mark:mark -> string -> signature ->
      string -> signature -> Shape.t -> module_coercion * Shape.t

val type_declarations:
  loc:Location.t -> Env.t -> mark:mark ->
  Ident.t -> type_declaration -> type_declaration -> unit

val print_coercion: Format.formatter -> module_coercion -> unit

type symptom =
    Missing_field of Ident.t * Location.t * string (* kind *)
  | Value_descriptions of
      Ident.t * value_description * value_description
      * Includecore.value_mismatch
  | Type_declarations of Ident.t * type_declaration
        * type_declaration * Includecore.type_mismatch
  | Extension_constructors of Ident.t * extension_constructor
        * extension_constructor * Includecore.extension_constructor_mismatch
  | Module_types of module_type * module_type
  | Modtype_infos of Ident.t * modtype_declaration * modtype_declaration
  | Modtype_permutation of Types.module_type * Typedtree.module_coercion
  | Interface_mismatch of string * string
  | Class_type_declarations of
      Ident.t * class_type_declaration * class_type_declaration *
      Ctype.class_match_failure list
  | Class_declarations of
      Ident.t * class_declaration * class_declaration *
      Ctype.class_match_failure list
  | Unbound_module_path of Path.t
  | Invalid_module_alias of Path.t

type pos =
  | Module of Ident.t
  | Modtype of Ident.t
  | Arg of functor_parameter
  | Body of functor_parameter

exception Error of explanation
exception Apply_error of {
    loc : Location.t ;
    env : Env.t ;
    lid_app : Longident.t option ;
    mty_f : module_type ;
    args : (Error.functor_arg_descr * Types.module_type)  list ;
  }

val expand_module_alias: strengthen:bool -> Env.t -> Path.t -> Types.module_type

module Functor_inclusion_diff: sig
  module Defs: sig
    type left = Types.functor_parameter
    type right = left
    type eq = Typedtree.module_coercion
    type diff = (Types.functor_parameter, unit) Error.functor_param_symptom
    type state
  end
  val diff: Env.t ->
    Types.functor_parameter list * Types.module_type ->
    Types.functor_parameter list * Types.module_type ->
    Diffing.Define(Defs).patch
end

module Functor_app_diff: sig
  module Defs: sig
    type left = Error.functor_arg_descr * Types.module_type
    type right = Types.functor_parameter
    type eq = Typedtree.module_coercion
    type diff = (Error.functor_arg_descr, unit) Error.functor_param_symptom
    type state
  end
  val diff:
    Env.t ->
    f:Types.module_type ->
    args:(Error.functor_arg_descr * Types.module_type) list ->
    Diffing.Define(Defs).patch
end
