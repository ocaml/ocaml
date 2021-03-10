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

module Context: sig
  type pos =
    | Module of Ident.t
    | Modtype of Ident.t
    | Arg of Types.functor_parameter
    | Body of Types.functor_parameter

  val pp: Format.formatter -> pos list -> unit
  val alt_pp: Format.formatter -> pos list -> unit

end

type ('elt,'explanation) diff = {got:'elt; expected:'elt; symptom:'explanation}
type 'elt core_diff =('elt,unit) diff
val diff: 'elt -> 'elt -> 'explanation -> ('elt,'explanation) diff
val sdiff:  'elt -> 'elt -> 'elt core_diff

type functor_arg_descr =
  | Anonymous of Parsetree.module_expr
  | Named_arg of Path.t
  | Unit_arg


type core_sigitem_symptom =
  | Value_descriptions of Types.value_description core_diff
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
  | In_Type_declaration of Ident.t * core_sigitem_symptom
  | In_Expansion of core_module_type_symptom

module Illegal_permutation: sig
  val pp: (Format.formatter -> Context.pos list -> unit) -> Env.t ->
    Format.formatter -> Types.module_type * Typedtree.module_coercion -> unit
end
