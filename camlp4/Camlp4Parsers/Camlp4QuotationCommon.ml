open Camlp4;                                             (* -*- camlp4r -*- *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

module Id = struct
  value name = "Camlp4QuotationCommon";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Sig.Camlp4Syntax)
            (TheAntiquotSyntax : (Sig.Parser Syntax.Ast).SIMPLE)
= struct
  open Sig;
  include Syntax; (* Be careful an AntiquotSyntax module appears here *)

  module MetaLocHere = Ast.Meta.MetaLoc;
  module MetaLoc = struct
    module Ast = Ast;
    value loc_name = ref None;
    value meta_loc_expr _loc loc =
      match loc_name.val with
      [ None -> <:expr< $lid:Loc.name.val$ >>
      | Some "here" -> MetaLocHere.meta_loc_expr _loc loc
      | Some x -> <:expr< $lid:x$ >> ];
    value meta_loc_patt _loc _ = <:patt< _ >>;
  end;
  module MetaAst = Ast.Meta.Make MetaLoc;
  module ME = MetaAst.Expr;
  module MP = MetaAst.Patt;

  value is_antiquot s =
    let len = String.length s in
    len > 2 && s.[0] = '\\' && s.[1] = '$';

  value handle_antiquot_in_string s term parse loc decorate =
    if is_antiquot s then
      let pos = String.index s ':' in
      let name = String.sub s 2 (pos - 2)
      and code = String.sub s (pos + 1) (String.length s - pos - 1) in
      decorate name (parse loc code)
    else term;

  value antiquot_expander = object
    inherit Ast.map as super;
    method patt = fun
      [ <:patt@_loc< $anti:s$ >> | <:patt@_loc< $str:s$ >> as p ->
          let mloc _loc = MetaLoc.meta_loc_patt _loc _loc in
          handle_antiquot_in_string s p TheAntiquotSyntax.parse_patt _loc (fun n p ->
            match n with
            [ "antisig_item" -> <:patt< Ast.SgAnt $mloc _loc$ $p$ >>
            | "antistr_item" -> <:patt< Ast.StAnt $mloc _loc$ $p$ >>
            | "antictyp" -> <:patt< Ast.TyAnt $mloc _loc$ $p$ >>
            | "antipatt" -> <:patt< Ast.PaAnt $mloc _loc$ $p$ >>
            | "antiexpr" -> <:patt< Ast.ExAnt $mloc _loc$ $p$ >>
            | "antimodule_type" -> <:patt< Ast.MtAnt $mloc _loc$ $p$ >>
            | "antimodule_expr" -> <:patt< Ast.MeAnt $mloc _loc$ $p$ >>
            | "anticlass_type" -> <:patt< Ast.CtAnt $mloc _loc$ $p$ >>
            | "anticlass_expr" -> <:patt< Ast.CeAnt $mloc _loc$ $p$ >>
            | "anticlass_sig_item" -> <:patt< Ast.CgAnt $mloc _loc$ $p$ >>
            | "anticlass_str_item" -> <:patt< Ast.CrAnt $mloc _loc$ $p$ >>
            | "antiwith_constr" -> <:patt< Ast.WcAnt $mloc _loc$ $p$ >>
            | "antibinding" -> <:patt< Ast.BiAnt $mloc _loc$ $p$ >>
            | "antirec_binding" -> <:patt< Ast.RbAnt $mloc _loc$ $p$ >>
            | "antimatch_case" -> <:patt< Ast.McAnt $mloc _loc$ $p$ >>
            | "antimodule_binding" -> <:patt< Ast.MbAnt $mloc _loc$ $p$ >>
            | "antiident" -> <:patt< Ast.IdAnt $mloc _loc$ $p$ >>
            | _ -> p ])
      | p -> super#patt p ];
    method expr = fun
      [ <:expr@_loc< $anti:s$ >> | <:expr@_loc< $str:s$ >> as e ->
          let mloc _loc = MetaLoc.meta_loc_expr _loc _loc in
          handle_antiquot_in_string s e TheAntiquotSyntax.parse_expr _loc (fun n e ->
            match n with
            [ "`int" -> <:expr< string_of_int $e$ >>
            | "`int32" -> <:expr< Int32.to_string $e$ >>
            | "`int64" -> <:expr< Int64.to_string $e$ >>
            | "`nativeint" -> <:expr< Nativeint.to_string $e$ >>
            | "`flo" -> <:expr< string_of_float $e$ >>
            | "`str" -> <:expr< Ast.safe_string_escaped $e$ >>
            | "`chr" -> <:expr< Char.escaped $e$ >>
            | "`bool" -> <:expr< Ast.IdUid $mloc _loc$ (if $e$ then "True" else "False") >>
            | "liststr_item" -> <:expr< Ast.stSem_of_list $e$ >>
            | "listsig_item" -> <:expr< Ast.sgSem_of_list $e$ >>
            | "listclass_sig_item" -> <:expr< Ast.cgSem_of_list $e$ >>
            | "listclass_str_item" -> <:expr< Ast.crSem_of_list $e$ >>
            | "listmodule_expr" -> <:expr< Ast.meApp_of_list $e$ >>
            | "listmodule_type" -> <:expr< Ast.mtApp_of_list $e$ >>
            | "listmodule_binding" -> <:expr< Ast.mbAnd_of_list $e$ >>
            | "listbinding" -> <:expr< Ast.biAnd_of_list $e$ >>
            | "listbinding;" -> <:expr< Ast.biSem_of_list $e$ >>
            | "listrec_binding" -> <:expr< Ast.rbSem_of_list $e$ >>
            | "listclass_type" -> <:expr< Ast.ctAnd_of_list $e$ >>
            | "listclass_expr" -> <:expr< Ast.ceAnd_of_list $e$ >>
            | "listident" -> <:expr< Ast.idAcc_of_list $e$ >>
            | "listctypand" -> <:expr< Ast.tyAnd_of_list $e$ >>
            | "listctyp;" -> <:expr< Ast.tySem_of_list $e$ >>
            | "listctyp*" -> <:expr< Ast.tySta_of_list $e$ >>
            | "listctyp|" -> <:expr< Ast.tyOr_of_list $e$ >>
            | "listctyp," -> <:expr< Ast.tyCom_of_list $e$ >>
            | "listctyp&" -> <:expr< Ast.tyAmp_of_list $e$ >>
            | "listwith_constr" -> <:expr< Ast.wcAnd_of_list $e$ >>
            | "listmatch_case" -> <:expr< Ast.mcOr_of_list $e$ >>
            | "listpatt," -> <:expr< Ast.paCom_of_list $e$ >>
            | "listpatt;" -> <:expr< Ast.paSem_of_list $e$ >>
            | "listexpr," -> <:expr< Ast.exCom_of_list $e$ >>
            | "listexpr;" -> <:expr< Ast.exSem_of_list $e$ >>
            | "antisig_item" -> <:expr< Ast.SgAnt $mloc _loc$ $e$ >>
            | "antistr_item" -> <:expr< Ast.StAnt $mloc _loc$ $e$ >>
            | "antictyp" -> <:expr< Ast.TyAnt $mloc _loc$ $e$ >>
            | "antipatt" -> <:expr< Ast.PaAnt $mloc _loc$ $e$ >>
            | "antiexpr" -> <:expr< Ast.ExAnt $mloc _loc$ $e$ >>
            | "antimodule_type" -> <:expr< Ast.MtAnt $mloc _loc$ $e$ >>
            | "antimodule_expr" -> <:expr< Ast.MeAnt $mloc _loc$ $e$ >>
            | "anticlass_type" -> <:expr< Ast.CtAnt $mloc _loc$ $e$ >>
            | "anticlass_expr" -> <:expr< Ast.CeAnt $mloc _loc$ $e$ >>
            | "anticlass_sig_item" -> <:expr< Ast.CgAnt $mloc _loc$ $e$ >>
            | "anticlass_str_item" -> <:expr< Ast.CrAnt $mloc _loc$ $e$ >>
            | "antiwith_constr" -> <:expr< Ast.WcAnt $mloc _loc$ $e$ >>
            | "antibinding" -> <:expr< Ast.BiAnt $mloc _loc$ $e$ >>
            | "antirec_binding" -> <:expr< Ast.RbAnt $mloc _loc$ $e$ >>
            | "antimatch_case" -> <:expr< Ast.McAnt $mloc _loc$ $e$ >>
            | "antimodule_binding" -> <:expr< Ast.MbAnt $mloc _loc$ $e$ >>
            | "antiident" -> <:expr< Ast.IdAnt $mloc _loc$ $e$ >>
            | _ -> e ])
      | e -> super#expr e ];
  end;

  value add_quotation name entry mexpr mpatt =
    let entry_eoi = Gram.Entry.mk (Gram.Entry.name entry) in
    let parse_quot_string entry loc s =
      let q = Camlp4_config.antiquotations.val in
      let () = Camlp4_config.antiquotations.val := True in
      let res = Gram.parse_string entry loc s in
      let () = Camlp4_config.antiquotations.val := q in
      res in
    let expand_expr loc loc_name_opt s =
      let ast = parse_quot_string entry_eoi loc s in
      let () = MetaLoc.loc_name.val := loc_name_opt in
      let meta_ast = mexpr loc ast in
      let exp_ast = antiquot_expander#expr meta_ast in
      exp_ast in
    let expand_str_item loc loc_name_opt s =
      let exp_ast = expand_expr loc loc_name_opt s in
      <:str_item@loc< $exp:exp_ast$ >> in
    let expand_patt _loc loc_name_opt s =
      let ast = parse_quot_string entry_eoi _loc s in
      let meta_ast = mpatt _loc ast in
      let exp_ast = antiquot_expander#patt meta_ast in
      match loc_name_opt with
      [ None -> exp_ast
      | Some name ->
        let rec subst_first_loc =
          fun
          [ <:patt@_loc< Ast.$uid:u$ $_$ >> -> <:patt< Ast.$uid:u$ $lid:name$ >>
          | <:patt@_loc< $a$ $b$ >> -> <:patt< $subst_first_loc a$ $b$ >>
          | p -> p ] in
        subst_first_loc exp_ast ] in
    do {
      EXTEND Gram
        entry_eoi:
          [ [ x = entry; `EOI -> x ] ]
        ;
      END;
      Quotation.add name Quotation.DynAst.expr_tag expand_expr;
      Quotation.add name Quotation.DynAst.patt_tag expand_patt;
      Quotation.add name Quotation.DynAst.str_item_tag expand_str_item;
    };

  add_quotation "sig_item" sig_item_quot ME.meta_sig_item MP.meta_sig_item;
  add_quotation "str_item" str_item_quot ME.meta_str_item MP.meta_str_item;
  add_quotation "ctyp" ctyp_quot ME.meta_ctyp MP.meta_ctyp;
  add_quotation "patt" patt_quot ME.meta_patt MP.meta_patt;
  add_quotation "expr" expr_quot ME.meta_expr MP.meta_expr;
  add_quotation "module_type" module_type_quot ME.meta_module_type MP.meta_module_type;
  add_quotation "module_expr" module_expr_quot ME.meta_module_expr MP.meta_module_expr;
  add_quotation "class_type" class_type_quot ME.meta_class_type MP.meta_class_type;
  add_quotation "class_expr" class_expr_quot ME.meta_class_expr MP.meta_class_expr;
  add_quotation "class_sig_item"
                class_sig_item_quot ME.meta_class_sig_item MP.meta_class_sig_item;
  add_quotation "class_str_item"
                class_str_item_quot ME.meta_class_str_item MP.meta_class_str_item;
  add_quotation "with_constr" with_constr_quot ME.meta_with_constr MP.meta_with_constr;
  add_quotation "binding" binding_quot ME.meta_binding MP.meta_binding;
  add_quotation "rec_binding" rec_binding_quot ME.meta_rec_binding MP.meta_rec_binding;
  add_quotation "match_case" match_case_quot ME.meta_match_case MP.meta_match_case;
  add_quotation "module_binding"
                module_binding_quot ME.meta_module_binding MP.meta_module_binding;
  add_quotation "ident" ident_quot ME.meta_ident MP.meta_ident;

end;
