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

(** This module is suppose to contain nils elimination. *)
module Make (Ast : Sig.Camlp4Ast) = struct

  class clean_ast = object

    inherit Ast.map as super;

    method with_constr wc =
      match super#with_constr wc with
      [ <:with_constr< $ <:with_constr<>> $ and $wc$ >> |
        <:with_constr< $wc$ and $ <:with_constr<>> $ >> -> wc
      | wc -> wc ];

    method expr e =
      match super#expr e with
      [ <:expr< let $rec:_$ $ <:binding<>> $ in $e$ >> |
        <:expr< { ($e$) with $ <:rec_binding<>> $ } >> |
        <:expr< $ <:expr<>> $, $e$ >> |
        <:expr< $e$, $ <:expr<>> $ >> |
        <:expr< $ <:expr<>> $; $e$ >> |
        <:expr< $e$; $ <:expr<>> $ >> -> e
      | e -> e ];

    method patt p =
      match super#patt p with
      [ <:patt< ( $p$ as $ <:patt<>> $ ) >> |
        <:patt< $ <:patt<>> $ | $p$ >> |
        <:patt< $p$ | $ <:patt<>> $ >> |
        <:patt< $ <:patt<>> $, $p$ >> |
        <:patt< $p$, $ <:patt<>> $ >> |
        <:patt< $ <:patt<>> $; $p$ >> |
        <:patt< $p$; $ <:patt<>> $ >> -> p
      | p -> p ];

    method match_case mc =
      match super#match_case mc with
      [ <:match_case< $ <:match_case<>> $ | $mc$ >> |
        <:match_case< $mc$ | $ <:match_case<>> $ >> -> mc
      | mc -> mc ];

    method binding bi =
      match super#binding bi with
      [ <:binding< $ <:binding<>> $ and $bi$ >> |
        <:binding< $bi$ and $ <:binding<>> $ >> -> bi
      | bi -> bi ];

    method rec_binding rb =
      match super#rec_binding rb with
      [ <:rec_binding< $ <:rec_binding<>> $ ; $bi$ >> |
        <:rec_binding< $bi$ ; $ <:rec_binding<>> $ >> -> bi
      | bi -> bi ];

    method module_binding mb =
      match super#module_binding mb with
      [ <:module_binding< $ <:module_binding<>> $ and $mb$ >> |
        <:module_binding< $mb$ and $ <:module_binding<>> $ >> -> mb
      | mb -> mb ];

    method ctyp t =
      match super#ctyp t with
      [ <:ctyp< ! $ <:ctyp<>> $ . $t$ >> |
        <:ctyp< $ <:ctyp<>> $ as $t$ >> |
        <:ctyp< $t$ as $ <:ctyp<>> $ >> |
        <:ctyp< $t$ -> $ <:ctyp<>> $ >> |
        <:ctyp< $ <:ctyp<>> $ -> $t$ >> |
        <:ctyp< $ <:ctyp<>> $ | $t$ >> |
        <:ctyp< $t$ | $ <:ctyp<>> $ >> |
        <:ctyp< $t$ of $ <:ctyp<>> $ >> |
        <:ctyp< $ <:ctyp<>> $ and $t$ >> |
        <:ctyp< $t$ and $ <:ctyp<>> $ >> |
        <:ctyp< $t$; $ <:ctyp<>> $ >> |
        <:ctyp< $ <:ctyp<>> $; $t$ >> |
        <:ctyp< $ <:ctyp<>> $, $t$ >> |
        <:ctyp< $t$, $ <:ctyp<>> $ >> |
        <:ctyp< $t$ & $ <:ctyp<>> $ >> |
        <:ctyp< $ <:ctyp<>> $ & $t$ >> |
        <:ctyp< $ <:ctyp<>> $ * $t$ >> |
        <:ctyp< $t$ * $ <:ctyp<>> $ >> -> t
      | t -> t ];

    method sig_item sg =
      match super#sig_item sg with
      [ <:sig_item< $ <:sig_item<>> $; $sg$ >> |
        <:sig_item< $sg$; $ <:sig_item<>> $ >> -> sg
      | <:sig_item@loc< type $ <:ctyp<>> $ >> -> <:sig_item@loc<>>
      | sg -> sg ];

    method str_item st =
      match super#str_item st with
      [ <:str_item< $ <:str_item<>> $; $st$ >> |
        <:str_item< $st$; $ <:str_item<>> $ >> -> st
      | <:str_item@loc< type $ <:ctyp<>> $ >> -> <:str_item@loc<>>
      | <:str_item@loc< value $rec:_$ $ <:binding<>> $ >> -> <:str_item@loc<>>
      | st -> st ];

    method module_type mt =
      match super#module_type mt with
      [ <:module_type< $mt$ with $ <:with_constr<>> $ >> -> mt
      | mt -> mt ];

    method class_expr ce =
      match super#class_expr ce with
      [ <:class_expr< $ <:class_expr<>> $ and $ce$ >> |
        <:class_expr< $ce$ and $ <:class_expr<>> $ >> -> ce
      | ce -> ce ];

    method class_type ct =
      match super#class_type ct with
      [ <:class_type< $ <:class_type<>> $ and $ct$ >> |
        <:class_type< $ct$ and $ <:class_type<>> $ >> -> ct
      | ct -> ct ];

    method class_sig_item csg =
      match super#class_sig_item csg with
      [ <:class_sig_item< $ <:class_sig_item<>> $; $csg$ >> |
        <:class_sig_item< $csg$; $ <:class_sig_item<>> $ >> -> csg
      | csg -> csg ];

    method class_str_item cst =
      match super#class_str_item cst with
      [ <:class_str_item< $ <:class_str_item<>> $; $cst$ >> |
        <:class_str_item< $cst$; $ <:class_str_item<>> $ >> -> cst
      | cst -> cst ];

  end;

end;
