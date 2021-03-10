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

let kind_of_field_desc fd = match fd.kind with
  | Field_value -> "value"
  | Field_type -> "type"
  | Field_exception -> "exception"
  | Field_typext -> "extension constructor"
  | Field_module -> "module"
  | Field_modtype -> "module type"
  | Field_class -> "class"
  | Field_classtype -> "class type"

let field_desc kind id = { kind; name = Ident.name id }

(** Map indexed by both field types and names.
    This avoids name clashes between different sorts of fields
    such as values and types. *)
module FieldMap = Map.Make(struct
    type t = field_desc
    let compare = Stdlib.compare
  end)

let item_ident_name = function
    Types.Sig_value(id, d, _) -> (id, d.val_loc, field_desc Field_value id)
  | Types.Sig_type(id, d, _, _) -> (id, d.type_loc, field_desc Field_type  id )
  | Types.Sig_typext(id, d, _, _) ->
      let kind =
        if Path.same d.ext_type_path Predef.path_exn
        then Field_exception
        else Field_typext
      in
      (id, d.ext_loc, field_desc kind id)
  | Types.Sig_module(id, _, d, _, _) ->
      (id, d.md_loc, field_desc Field_module id)
  | Types.Sig_modtype(id, d, _) -> (id, d.mtd_loc, field_desc Field_modtype id)
  | Types.Sig_class(id, d, _, _) -> (id, d.cty_loc, field_desc Field_class id)
  | Types.Sig_class_type(id, d, _, _) ->
      (id, d.clty_loc, field_desc Field_classtype id)

let is_runtime_component = function
  | Types.Sig_value(_,{val_kind = Val_prim _}, _)
  | Types.Sig_type(_,_,_,_)
  | Types.Sig_module(_,Mp_absent,_,_,_)
  | Types.Sig_modtype(_,_,_)
  | Types.Sig_class_type(_,_,_,_) -> false
  | Types.Sig_value(_,_,_)
  | Types.Sig_typext(_,_,_,_)
  | Types.Sig_module(_,Mp_present,_,_,_)
  | Types.Sig_class(_,_,_,_) -> true

(* Print a coercion *)

let rec print_list pr ppf = function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; Format.fprintf ppf ";@ "; print_list pr ppf l
let print_list pr ppf l =
  Format.fprintf ppf "[@[%a@]]" (print_list pr) l

let rec print_coercion ppf c =
  let pr fmt = Format.fprintf ppf fmt in
  match c with
    Typedtree.Tcoerce_none -> pr "id"
  | Typedtree.Tcoerce_structure (fl, nl) ->
      pr "@[<2>struct@ %a@ %a@]"
        (print_list print_coercion2) fl
        (print_list print_coercion3) nl
  | Typedtree.Tcoerce_functor (inp, out) ->
      pr "@[<2>functor@ (%a)@ (%a)@]"
        print_coercion inp
        print_coercion out
  | Typedtree.Tcoerce_primitive {pc_desc; pc_env = _; pc_type}  ->
      pr "prim %s@ (%a)" pc_desc.Primitive.prim_name
        Printtyp.raw_type_expr pc_type
  | Typedtree.Tcoerce_alias (_, p, c) ->
      pr "@[<2>alias %a@ (%a)@]"
        Printtyp.path p
        print_coercion c
and print_coercion2 ppf (n, c) =
  Format.fprintf ppf "@[%d,@ %a@]" n print_coercion c
and print_coercion3 ppf (i, n, c) =
  Format.fprintf ppf "@[%s, %d,@ %a@]"
    (Ident.unique_name i) n print_coercion c
