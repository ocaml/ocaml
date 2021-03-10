(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Error symptom *)

type symptom =
  | Missing_field of Ident.t * Location.t * string (* kind *)
  | Value_descriptions of
      Ident.t * Types.value_description * Types.value_description
  | Type_declarations of Ident.t * Types.type_declaration
        *  Types.type_declaration * Includecore.type_mismatch
  | Extension_constructors of Ident.t * extension_constructor
        * extension_constructor * Includecore.extension_constructor_mismatch
  | Module_types of  Types.module_type * Types.module_type
  | Modtype_infos of
      Ident.t * Types.modtype_declaration * Types.modtype_declaration
  | Modtype_permutation of Types.module_type * Typedtree.module_coercion
  | Interface_mismatch of string * string
  | Class_type_declarations of
      Ident.t * Types.class_type_declaration * Types.class_type_declaration *
      Ctype.class_match_failure list
  | Class_declarations of
      Ident.t * Types.class_declaration * Types.class_declaration *
      Ctype.class_match_failure list
  | Unbound_module_path of Path.t
  | Invalid_module_alias of Path.t


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

(* Print a coercion *)

val print_coercion: Format.formatter -> Typedtree.module_coercion -> unit
