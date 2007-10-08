(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2007  Institut  National  de  Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

module Make (Ast : Sig.Ast) : Sig.DynAst with module Ast = Ast = struct
  module Ast = Ast;
  type tag 'a =
    [ Tag_ctyp
    | Tag_patt
    | Tag_expr
    | Tag_module_type
    | Tag_sig_item
    | Tag_with_constr
    | Tag_module_expr
    | Tag_str_item
    | Tag_class_type
    | Tag_class_sig_item
    | Tag_class_expr
    | Tag_class_str_item
    | Tag_match_case
    | Tag_ident
    | Tag_binding
    | Tag_rec_binding
    | Tag_module_binding ];

  value string_of_tag =
    fun
    [ Tag_ctyp -> "ctyp"
    | Tag_patt -> "patt"
    | Tag_expr -> "expr"
    | Tag_module_type -> "module_type"
    | Tag_sig_item -> "sig_item"
    | Tag_with_constr -> "with_constr"
    | Tag_module_expr -> "module_expr"
    | Tag_str_item -> "str_item"
    | Tag_class_type -> "class_type"
    | Tag_class_sig_item -> "class_sig_item"
    | Tag_class_expr -> "class_expr"
    | Tag_class_str_item -> "class_str_item"
    | Tag_match_case -> "match_case"
    | Tag_ident -> "ident"
    | Tag_binding -> "binding"
    | Tag_rec_binding -> "rec_binding"
    | Tag_module_binding -> "module_binding" ];

  value ctyp_tag = Tag_ctyp;
  value patt_tag = Tag_patt;
  value expr_tag = Tag_expr;
  value module_type_tag = Tag_module_type;
  value sig_item_tag = Tag_sig_item;
  value with_constr_tag = Tag_with_constr;
  value module_expr_tag = Tag_module_expr;
  value str_item_tag = Tag_str_item;
  value class_type_tag = Tag_class_type;
  value class_sig_item_tag = Tag_class_sig_item;
  value class_expr_tag = Tag_class_expr;
  value class_str_item_tag = Tag_class_str_item;
  value match_case_tag = Tag_match_case;
  value ident_tag = Tag_ident;
  value binding_tag = Tag_binding;
  value rec_binding_tag = Tag_rec_binding;
  value module_binding_tag = Tag_module_binding;

  type dyn;
  external dyn_tag : tag 'a -> tag dyn = "%identity";

  module Pack(X : sig type t 'a; end) = struct
    (* These Obj.* hacks should be avoided with GADTs *)
    type pack = (tag dyn * Obj.t);
    exception Pack_error;
    value pack tag v = (dyn_tag tag, Obj.repr v);
    value unpack (tag : tag 'a) (tag', obj) =
      if dyn_tag tag = tag' then (Obj.obj obj : X.t 'a) else raise Pack_error;
    value print_tag f (tag, _) = Format.pp_print_string f (string_of_tag tag);
  end;
end;
