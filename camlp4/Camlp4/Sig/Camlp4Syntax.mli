(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

(** A syntax module is a sort of constistent bunch of modules and values.
    In such a module you have a parser, a printer, and also modules for
    locations, syntax trees, tokens, grammars, quotations, anti-quotations.
    There is also the main grammar entries. *)
module type S = sig
  module Loc            : Loc.S;
  module Warning        : Warning.S with module Loc = Loc;

  module Ast            : Camlp4Ast.S with module Loc = Loc;
  module Token          : Camlp4Token.S with module Loc = Loc;

  module Gram           : Grammar.Static.S with module Loc = Loc and module Token = Token;
  module AntiquotSyntax : AntiquotSyntax.S with module Ast = Camlp4Ast.ToAst Ast;
                          (* Gram is not constrained here for flexibility *)
  module Quotation      : Quotation.S with module Ast = Camlp4Ast.ToAst Ast;
  module Parser         : Parser.S with module Ast = Camlp4Ast.ToAst Ast;
  module Printer        : Printer.S with module Ast = Camlp4Ast.ToAst Ast;

  value interf : Gram.Entry.t (list Ast.sig_item * bool);
  value implem : Gram.Entry.t (list Ast.str_item * bool);
  value top_phrase : Gram.Entry.t (option Ast.str_item);
  value use_file : Gram.Entry.t (list Ast.str_item * bool);
  value a_CHAR : Gram.Entry.t string;
  value a_FLOAT : Gram.Entry.t string;
  value a_INT : Gram.Entry.t string;
  value a_INT32 : Gram.Entry.t string;
  value a_INT64 : Gram.Entry.t string;
  value a_LABEL : Gram.Entry.t string;
  value a_LIDENT : Gram.Entry.t string;
  value a_LIDENT_or_operator : Gram.Entry.t string;
  value a_NATIVEINT : Gram.Entry.t string;
  value a_OPTLABEL : Gram.Entry.t string;
  value a_STRING : Gram.Entry.t string;
  value a_UIDENT : Gram.Entry.t string;
  value a_ident : Gram.Entry.t string;
  value amp_ctyp : Gram.Entry.t Ast.ctyp;
  value and_ctyp : Gram.Entry.t Ast.ctyp;
  value match_case : Gram.Entry.t Ast.match_case;
  value match_case0 : Gram.Entry.t Ast.match_case;
  value match_case_quot : Gram.Entry.t Ast.match_case;
  value binding : Gram.Entry.t Ast.binding;
  value binding_quot : Gram.Entry.t Ast.binding;
  value class_declaration : Gram.Entry.t Ast.class_expr;
  value class_description : Gram.Entry.t Ast.class_type;
  value class_expr : Gram.Entry.t Ast.class_expr;
  value class_expr_quot : Gram.Entry.t Ast.class_expr;
  value class_fun_binding : Gram.Entry.t Ast.class_expr;
  value class_fun_def : Gram.Entry.t Ast.class_expr;
  value class_info_for_class_expr : Gram.Entry.t Ast.class_expr;
  value class_info_for_class_type : Gram.Entry.t Ast.class_type;
  value class_longident : Gram.Entry.t Ast.ident;
  value class_longident_and_param : Gram.Entry.t Ast.class_expr;
  value class_name_and_param : Gram.Entry.t (string * Ast.ctyp);
  value class_sig_item : Gram.Entry.t Ast.class_sig_item;
  value class_sig_item_quot : Gram.Entry.t Ast.class_sig_item;
  value class_signature : Gram.Entry.t Ast.class_sig_item;
  value class_str_item : Gram.Entry.t Ast.class_str_item;
  value class_str_item_quot : Gram.Entry.t Ast.class_str_item;
  value class_structure : Gram.Entry.t Ast.class_str_item;
  value class_type : Gram.Entry.t Ast.class_type;
  value class_type_declaration : Gram.Entry.t Ast.class_type;
  value class_type_longident : Gram.Entry.t Ast.ident;
  value class_type_longident_and_param : Gram.Entry.t Ast.class_type;
  value class_type_plus : Gram.Entry.t Ast.class_type;
  value class_type_quot : Gram.Entry.t Ast.class_type;
  value comma_ctyp : Gram.Entry.t Ast.ctyp;
  value comma_expr : Gram.Entry.t Ast.expr;
  value comma_ipatt : Gram.Entry.t Ast.patt;
  value comma_patt : Gram.Entry.t Ast.patt;
  value comma_type_parameter : Gram.Entry.t Ast.ctyp;
  value constrain : Gram.Entry.t (Ast.ctyp * Ast.ctyp);
  value constructor_arg_list : Gram.Entry.t Ast.ctyp;
  value constructor_declaration : Gram.Entry.t Ast.ctyp;
  value constructor_declarations : Gram.Entry.t Ast.ctyp;
  value ctyp : Gram.Entry.t Ast.ctyp;
  value ctyp_quot : Gram.Entry.t Ast.ctyp;
  value cvalue_binding : Gram.Entry.t Ast.expr;
  value direction_flag : Gram.Entry.t Ast.meta_bool;
  value dummy : Gram.Entry.t unit;
  value eq_expr : Gram.Entry.t (string -> Ast.patt -> Ast.patt);
  value expr : Gram.Entry.t Ast.expr;
  value expr_eoi : Gram.Entry.t Ast.expr;
  value expr_quot : Gram.Entry.t Ast.expr;
  value field : Gram.Entry.t Ast.ctyp;
  value field_expr : Gram.Entry.t Ast.binding;
  value fun_binding : Gram.Entry.t Ast.expr;
  value fun_def : Gram.Entry.t Ast.expr;
  value ident : Gram.Entry.t Ast.ident;
  value ident_quot : Gram.Entry.t Ast.ident;
  value ipatt : Gram.Entry.t Ast.patt;
  value ipatt_tcon : Gram.Entry.t Ast.patt;
  value label : Gram.Entry.t string;
  value label_declaration : Gram.Entry.t Ast.ctyp;
  value label_expr : Gram.Entry.t Ast.binding;
  value label_ipatt : Gram.Entry.t Ast.patt;
  value label_longident : Gram.Entry.t Ast.ident;
  value label_patt : Gram.Entry.t Ast.patt;
  value labeled_ipatt : Gram.Entry.t Ast.patt;
  value let_binding : Gram.Entry.t Ast.binding;
  value meth_list : Gram.Entry.t Ast.ctyp;
  value module_binding : Gram.Entry.t Ast.module_binding;
  value module_binding0 : Gram.Entry.t Ast.module_expr;
  value module_binding_quot : Gram.Entry.t Ast.module_binding;
  value module_declaration : Gram.Entry.t Ast.module_type;
  value module_expr : Gram.Entry.t Ast.module_expr;
  value module_expr_quot : Gram.Entry.t Ast.module_expr;
  value module_longident : Gram.Entry.t Ast.ident;
  value module_longident_with_app : Gram.Entry.t Ast.ident;
  value module_rec_declaration : Gram.Entry.t Ast.module_binding;
  value module_type : Gram.Entry.t Ast.module_type;
  value module_type_quot : Gram.Entry.t Ast.module_type;
  value more_ctyp : Gram.Entry.t Ast.ctyp;
  value name_tags : Gram.Entry.t Ast.ctyp;
  value opt_as_lident : Gram.Entry.t string;
  value opt_class_self_patt : Gram.Entry.t Ast.patt;
  value opt_class_self_type : Gram.Entry.t Ast.ctyp;
  value opt_comma_ctyp : Gram.Entry.t Ast.ctyp;
  value opt_dot_dot : Gram.Entry.t Ast.meta_bool;
  value opt_eq_ctyp : Gram.Entry.t (list Ast.ctyp -> Ast.ctyp);
  value opt_expr : Gram.Entry.t Ast.expr;
  value opt_meth_list : Gram.Entry.t Ast.ctyp;
  value opt_mutable : Gram.Entry.t Ast.meta_bool;
  value opt_polyt : Gram.Entry.t Ast.ctyp;
  value opt_private : Gram.Entry.t Ast.meta_bool;
  value opt_rec : Gram.Entry.t Ast.meta_bool;
  value opt_virtual : Gram.Entry.t Ast.meta_bool;
  value opt_when_expr : Gram.Entry.t Ast.expr;
  value patt : Gram.Entry.t Ast.patt;
  value patt_as_patt_opt : Gram.Entry.t Ast.patt;
  value patt_eoi : Gram.Entry.t Ast.patt;
  value patt_quot : Gram.Entry.t Ast.patt;
  value patt_tcon : Gram.Entry.t Ast.patt;
  value phrase : Gram.Entry.t Ast.str_item;
  value pipe_ctyp : Gram.Entry.t Ast.ctyp;
  value poly_type : Gram.Entry.t Ast.ctyp;
  value row_field : Gram.Entry.t Ast.ctyp;
  value sem_ctyp : Gram.Entry.t Ast.ctyp;
  value sem_expr : Gram.Entry.t Ast.expr;
  value sem_expr_for_list : Gram.Entry.t (Ast.expr -> Ast.expr);
  value sem_patt : Gram.Entry.t Ast.patt;
  value sem_patt_for_list : Gram.Entry.t (Ast.patt -> Ast.patt);
  value semi : Gram.Entry.t unit;
  value sequence : Gram.Entry.t Ast.expr;
  value sig_item : Gram.Entry.t Ast.sig_item;
  value sig_item_quot : Gram.Entry.t Ast.sig_item;
  value sig_items : Gram.Entry.t Ast.sig_item;
  value star_ctyp : Gram.Entry.t Ast.ctyp;
  value str_item : Gram.Entry.t Ast.str_item;
  value str_item_quot : Gram.Entry.t Ast.str_item;
  value str_items : Gram.Entry.t Ast.str_item;
  value type_constraint : Gram.Entry.t unit;
  value type_declaration : Gram.Entry.t Ast.ctyp;
  value type_ident_and_parameters : Gram.Entry.t (string * list Ast.ctyp);
  value type_kind : Gram.Entry.t Ast.ctyp;
  value type_longident : Gram.Entry.t Ast.ident;
  value type_longident_and_parameters : Gram.Entry.t Ast.ctyp;
  value type_parameter : Gram.Entry.t Ast.ctyp;
  value type_parameters : Gram.Entry.t (Ast.ctyp -> Ast.ctyp);
  value typevars : Gram.Entry.t Ast.ctyp;
  value val_longident : Gram.Entry.t Ast.ident;
  value value_let : Gram.Entry.t unit;
  value value_val : Gram.Entry.t unit;
  value with_constr : Gram.Entry.t Ast.with_constr;
  value with_constr_quot : Gram.Entry.t Ast.with_constr;
end;
