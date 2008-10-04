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
 * - Nicolas Pouillard: initial version
 *)

module Make (Ast     : Sig.Camlp4Ast)
            (Gram    : Sig.Grammar.Static with module Loc = Ast.Loc
                                            with type Token.t = Sig.camlp4_token)
            (Quotation : Sig.Quotation with module Ast = Sig.Camlp4AstToAst Ast)
: Sig.Camlp4Syntax with module Loc = Ast.Loc
                    and module Ast = Ast
                    and module Token = Gram.Token
                    and module Gram = Gram
                    and module Quotation = Quotation
= struct

  module Loc     = Ast.Loc;
  module Ast     = Ast;
  module Gram    = Gram;
  module Token   = Gram.Token;
  open Sig;

  (* Warnings *)
  type warning = Loc.t -> string -> unit;
  value default_warning loc txt = Format.eprintf "<W> %a: %s@." Loc.print loc txt;
  value current_warning = ref default_warning;
  value print_warning loc txt = current_warning.val loc txt;

  value a_CHAR = Gram.Entry.mk "a_CHAR";
  value a_FLOAT = Gram.Entry.mk "a_FLOAT";
  value a_INT = Gram.Entry.mk "a_INT";
  value a_INT32 = Gram.Entry.mk "a_INT32";
  value a_INT64 = Gram.Entry.mk "a_INT64";
  value a_LABEL = Gram.Entry.mk "a_LABEL";
  value a_LIDENT = Gram.Entry.mk "a_LIDENT";
  value a_NATIVEINT = Gram.Entry.mk "a_NATIVEINT";
  value a_OPTLABEL = Gram.Entry.mk "a_OPTLABEL";
  value a_STRING = Gram.Entry.mk "a_STRING";
  value a_UIDENT = Gram.Entry.mk "a_UIDENT";
  value a_ident = Gram.Entry.mk "a_ident";
  value amp_ctyp = Gram.Entry.mk "amp_ctyp";
  value and_ctyp = Gram.Entry.mk "and_ctyp";
  value match_case = Gram.Entry.mk "match_case";
  value match_case0 = Gram.Entry.mk "match_case0";
  value binding = Gram.Entry.mk "binding";
  value class_declaration = Gram.Entry.mk "class_declaration";
  value class_description = Gram.Entry.mk "class_description";
  value class_expr = Gram.Entry.mk "class_expr";
  value class_fun_binding = Gram.Entry.mk "class_fun_binding";
  value class_fun_def = Gram.Entry.mk "class_fun_def";
  value class_info_for_class_expr = Gram.Entry.mk "class_info_for_class_expr";
  value class_info_for_class_type = Gram.Entry.mk "class_info_for_class_type";
  value class_longident = Gram.Entry.mk "class_longident";
  value class_longident_and_param = Gram.Entry.mk "class_longident_and_param";
  value class_name_and_param = Gram.Entry.mk "class_name_and_param";
  value class_sig_item = Gram.Entry.mk "class_sig_item";
  value class_signature = Gram.Entry.mk "class_signature";
  value class_str_item = Gram.Entry.mk "class_str_item";
  value class_structure = Gram.Entry.mk "class_structure";
  value class_type = Gram.Entry.mk "class_type";
  value class_type_declaration = Gram.Entry.mk "class_type_declaration";
  value class_type_longident = Gram.Entry.mk "class_type_longident";
  value class_type_longident_and_param = Gram.Entry.mk "class_type_longident_and_param";
  value class_type_plus = Gram.Entry.mk "class_type_plus";
  value comma_ctyp = Gram.Entry.mk "comma_ctyp";
  value comma_expr = Gram.Entry.mk "comma_expr";
  value comma_ipatt = Gram.Entry.mk "comma_ipatt";
  value comma_patt = Gram.Entry.mk "comma_patt";
  value comma_type_parameter = Gram.Entry.mk "comma_type_parameter";
  value constrain = Gram.Entry.mk "constrain";
  value constructor_arg_list = Gram.Entry.mk "constructor_arg_list";
  value constructor_declaration = Gram.Entry.mk "constructor_declaration";
  value constructor_declarations = Gram.Entry.mk "constructor_declarations";
  value ctyp = Gram.Entry.mk "ctyp";
  value cvalue_binding = Gram.Entry.mk "cvalue_binding";
  value direction_flag = Gram.Entry.mk "direction_flag";
  value dummy = Gram.Entry.mk "dummy";
  value entry_eoi = Gram.Entry.mk "entry_eoi";
  value eq_expr = Gram.Entry.mk "eq_expr";
  value expr = Gram.Entry.mk "expr";
  value expr_eoi = Gram.Entry.mk "expr_eoi";
  value field_expr = Gram.Entry.mk "field_expr";
  value field_expr_list = Gram.Entry.mk "field_expr_list";
  value fun_binding = Gram.Entry.mk "fun_binding";
  value fun_def = Gram.Entry.mk "fun_def";
  value ident = Gram.Entry.mk "ident";
  value implem = Gram.Entry.mk "implem";
  value interf = Gram.Entry.mk "interf";
  value ipatt = Gram.Entry.mk "ipatt";
  value ipatt_tcon = Gram.Entry.mk "ipatt_tcon";
  value label = Gram.Entry.mk "label";
  value label_declaration = Gram.Entry.mk "label_declaration";
  value label_declaration_list = Gram.Entry.mk "label_declaration_list";
  value label_expr = Gram.Entry.mk "label_expr";
  value label_expr_list = Gram.Entry.mk "label_expr_list";
  value label_ipatt = Gram.Entry.mk "label_ipatt";
  value label_ipatt_list = Gram.Entry.mk "label_ipatt_list";
  value label_longident = Gram.Entry.mk "label_longident";
  value label_patt = Gram.Entry.mk "label_patt";
  value label_patt_list = Gram.Entry.mk "label_patt_list";
  value labeled_ipatt = Gram.Entry.mk "labeled_ipatt";
  value let_binding = Gram.Entry.mk "let_binding";
  value meth_list = Gram.Entry.mk "meth_list";
  value meth_decl = Gram.Entry.mk "meth_decl";
  value module_binding = Gram.Entry.mk "module_binding";
  value module_binding0 = Gram.Entry.mk "module_binding0";
  value module_declaration = Gram.Entry.mk "module_declaration";
  value module_expr = Gram.Entry.mk "module_expr";
  value module_longident = Gram.Entry.mk "module_longident";
  value module_longident_with_app = Gram.Entry.mk "module_longident_with_app";
  value module_rec_declaration = Gram.Entry.mk "module_rec_declaration";
  value module_type = Gram.Entry.mk "module_type";
  value more_ctyp = Gram.Entry.mk "more_ctyp";
  value name_tags = Gram.Entry.mk "name_tags";
  value opt_as_lident = Gram.Entry.mk "opt_as_lident";
  value opt_class_self_patt = Gram.Entry.mk "opt_class_self_patt";
  value opt_class_self_type = Gram.Entry.mk "opt_class_self_type";
  value opt_class_signature = Gram.Entry.mk "opt_class_signature";
  value opt_class_structure = Gram.Entry.mk "opt_class_structure";
  value opt_comma_ctyp = Gram.Entry.mk "opt_comma_ctyp";
  value opt_dot_dot = Gram.Entry.mk "opt_dot_dot";
  value opt_eq_ctyp = Gram.Entry.mk "opt_eq_ctyp";
  value opt_expr = Gram.Entry.mk "opt_expr";
  value opt_meth_list = Gram.Entry.mk "opt_meth_list";
  value opt_mutable = Gram.Entry.mk "opt_mutable";
  value opt_polyt = Gram.Entry.mk "opt_polyt";
  value opt_private = Gram.Entry.mk "opt_private";
  value opt_rec = Gram.Entry.mk "opt_rec";
  value opt_sig_items = Gram.Entry.mk "opt_sig_items";
  value opt_str_items = Gram.Entry.mk "opt_str_items";
  value opt_virtual = Gram.Entry.mk "opt_virtual";
  value opt_when_expr = Gram.Entry.mk "opt_when_expr";
  value patt = Gram.Entry.mk "patt";
  value patt_as_patt_opt = Gram.Entry.mk "patt_as_patt_opt";
  value patt_eoi = Gram.Entry.mk "patt_eoi";
  value patt_tcon = Gram.Entry.mk "patt_tcon";
  value phrase = Gram.Entry.mk "phrase";
  value poly_type = Gram.Entry.mk "poly_type";
  value row_field = Gram.Entry.mk "row_field";
  value sem_expr = Gram.Entry.mk "sem_expr";
  value sem_expr_for_list = Gram.Entry.mk "sem_expr_for_list";
  value sem_patt = Gram.Entry.mk "sem_patt";
  value sem_patt_for_list = Gram.Entry.mk "sem_patt_for_list";
  value semi = Gram.Entry.mk "semi";
  value sequence = Gram.Entry.mk "sequence";
  value do_sequence = Gram.Entry.mk "do_sequence";
  value sig_item = Gram.Entry.mk "sig_item";
  value sig_items = Gram.Entry.mk "sig_items";
  value star_ctyp = Gram.Entry.mk "star_ctyp";
  value str_item = Gram.Entry.mk "str_item";
  value str_items = Gram.Entry.mk "str_items";
  value top_phrase = Gram.Entry.mk "top_phrase";
  value type_constraint = Gram.Entry.mk "type_constraint";
  value type_declaration = Gram.Entry.mk "type_declaration";
  value type_ident_and_parameters = Gram.Entry.mk "type_ident_and_parameters";
  value type_kind = Gram.Entry.mk "type_kind";
  value type_longident = Gram.Entry.mk "type_longident";
  value type_longident_and_parameters = Gram.Entry.mk "type_longident_and_parameters";
  value type_parameter = Gram.Entry.mk "type_parameter";
  value type_parameters = Gram.Entry.mk "type_parameters";
  value typevars = Gram.Entry.mk "typevars";
  value use_file = Gram.Entry.mk "use_file";
  value val_longident = Gram.Entry.mk "val_longident";
  value value_let = Gram.Entry.mk "value_let";
  value value_val = Gram.Entry.mk "value_val";
  value with_constr = Gram.Entry.mk "with_constr";
  value expr_quot = Gram.Entry.mk "quotation of expression";
  value patt_quot = Gram.Entry.mk "quotation of pattern";
  value ctyp_quot = Gram.Entry.mk "quotation of type";
  value str_item_quot = Gram.Entry.mk "quotation of structure item";
  value sig_item_quot = Gram.Entry.mk "quotation of signature item";
  value class_str_item_quot = Gram.Entry.mk "quotation of class structure item";
  value class_sig_item_quot = Gram.Entry.mk "quotation of class signature item";
  value module_expr_quot = Gram.Entry.mk "quotation of module expression";
  value module_type_quot = Gram.Entry.mk "quotation of module type";
  value class_type_quot = Gram.Entry.mk "quotation of class type";
  value class_expr_quot = Gram.Entry.mk "quotation of class expression";
  value with_constr_quot = Gram.Entry.mk "quotation of with constraint";
  value binding_quot = Gram.Entry.mk "quotation of binding";
  value rec_binding_quot = Gram.Entry.mk "quotation of record binding";
  value match_case_quot = Gram.Entry.mk "quotation of match_case (try/match/function case)";
  value module_binding_quot = Gram.Entry.mk "quotation of module rec binding";
  value ident_quot = Gram.Entry.mk "quotation of identifier";
  value prefixop = Gram.Entry.mk "prefix operator (start with '!', '?', '~')";
  value infixop0 = Gram.Entry.mk "infix operator (level 0) (comparison operators, and some others)";
  value infixop1 = Gram.Entry.mk "infix operator (level 1) (start with '^', '@')";
  value infixop2 = Gram.Entry.mk "infix operator (level 2) (start with '+', '-')";
  value infixop3 = Gram.Entry.mk "infix operator (level 3) (start with '*', '/', '%')";
  value infixop4 = Gram.Entry.mk "infix operator (level 4) (start with \"**\") (right assoc)";

  EXTEND Gram
    top_phrase:
      [ [ `EOI -> None ] ]
    ;
  END;

  module AntiquotSyntax = struct
    module Loc  = Ast.Loc;
    module Ast  = Sig.Camlp4AstToAst Ast;
    module Gram = Gram;
    value antiquot_expr = Gram.Entry.mk "antiquot_expr";
    value antiquot_patt = Gram.Entry.mk "antiquot_patt";
    EXTEND Gram
      antiquot_expr:
        [ [ x = expr; `EOI -> x ] ]
      ;
      antiquot_patt:
        [ [ x = patt; `EOI -> x ] ]
      ;
    END;
    value parse_expr loc str = Gram.parse_string antiquot_expr loc str;
    value parse_patt loc str = Gram.parse_string antiquot_patt loc str;
  end;

  module Quotation = Quotation;

  value wrap directive_handler pa init_loc cs =
    let rec loop loc =
      let (pl, stopped_at_directive) = pa loc cs in
      match stopped_at_directive with
      [ Some new_loc ->
        let pl =
          match List.rev pl with
          [ [] -> assert False
          | [x :: xs] ->
              match directive_handler x with
              [ None -> xs
              | Some x -> [x :: xs] ] ]
        in (List.rev pl) @ (loop new_loc)
      | None -> pl ]
    in loop init_loc;

  value parse_implem ?(directive_handler = fun _ -> None) _loc cs =
    let l = wrap directive_handler (Gram.parse implem) _loc cs in
    <:str_item< $list:l$ >>;

  value parse_interf ?(directive_handler = fun _ -> None) _loc cs =
    let l = wrap directive_handler (Gram.parse interf) _loc cs in
    <:sig_item< $list:l$ >>;

  value print_interf ?input_file:(_) ?output_file:(_) _ = failwith "No interface printer";
  value print_implem ?input_file:(_) ?output_file:(_) _ = failwith "No implementation printer";
end;
