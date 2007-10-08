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

  class clean_ast = object (self)

    inherit Ast.map as super;

    method with_constr = fun
    [ <:with_constr< $ <:with_constr<>> $ and $wc$ >> |
      <:with_constr< $wc$ and $ <:with_constr<>> $ >> -> self#with_constr wc
    | wc -> super#with_constr wc ];

    method expr = fun
    [ <:expr< let $rec:_$ $ <:binding<>> $ in $e$ >> |
      <:expr< { ($e$) with $ <:rec_binding<>> $ } >> |
      <:expr< $ <:expr<>> $, $e$ >> |
      <:expr< $e$, $ <:expr<>> $ >> |
      <:expr< $ <:expr<>> $; $e$ >> |
      <:expr< $e$; $ <:expr<>> $ >> -> self#expr e
    | e -> super#expr e ];

    method patt = fun
    [ <:patt< ( $p$ as $ <:patt<>> $ ) >> |
      <:patt< $ <:patt<>> $ | $p$ >> |
      <:patt< $p$ | $ <:patt<>> $ >> |
      <:patt< $ <:patt<>> $, $p$ >> |
      <:patt< $p$, $ <:patt<>> $ >> |
      <:patt< $ <:patt<>> $; $p$ >> |
      <:patt< $p$; $ <:patt<>> $ >> -> self#patt p
    | p -> super#patt p ];

    method match_case = fun
    [ <:match_case< $ <:match_case<>> $ | $mc$ >> |
      <:match_case< $mc$ | $ <:match_case<>> $ >> -> self#match_case mc
    | mc -> super#match_case mc ];

    method binding = fun
    [ <:binding< $ <:binding<>> $ and $bi$ >> |
      <:binding< $bi$ and $ <:binding<>> $ >> -> self#binding bi
    | bi -> super#binding bi ];

    method rec_binding = fun
    [ <:rec_binding< $ <:rec_binding<>> $ ; $bi$ >> |
      <:rec_binding< $bi$ ; $ <:rec_binding<>> $ >> -> self#rec_binding bi
    | bi -> super#rec_binding bi ];

    method module_binding = fun
    [ <:module_binding< $ <:module_binding<>> $ and $mb$ >> |
      <:module_binding< $mb$ and $ <:module_binding<>> $ >> ->
        self#module_binding mb
    | mb -> super#module_binding mb ];

    method ctyp = fun
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
      <:ctyp< $t$ * $ <:ctyp<>> $ >> -> self#ctyp t
    | t -> super#ctyp t ];

    method sig_item = fun
    [ <:sig_item< $ <:sig_item<>> $; $sg$ >> |
      <:sig_item< $sg$; $ <:sig_item<>> $ >> -> self#sig_item sg
    | sg -> super#sig_item sg ];

    method str_item = fun
    [ <:str_item< $ <:str_item<>> $; $st$ >> |
      <:str_item< $st$; $ <:str_item<>> $ >> -> self#str_item st
    | st -> super#str_item st ];

    method module_type = fun
    [ <:module_type< $mt$ with $ <:with_constr<>> $ >> -> self#module_type mt
    | mt -> super#module_type mt ];

    method class_expr = fun
    [ <:class_expr< $ <:class_expr<>> $ and $ce$ >> |
      <:class_expr< $ce$ and $ <:class_expr<>> $ >> -> self#class_expr ce
    | ce -> super#class_expr ce ];

    method class_type = fun
    [ <:class_type< $ <:class_type<>> $ and $ct$ >> |
      <:class_type< $ct$ and $ <:class_type<>> $ >> -> self#class_type ct
    | ct -> super#class_type ct ];

    method class_sig_item = fun
    [ <:class_sig_item< $ <:class_sig_item<>> $; $csg$ >> |
      <:class_sig_item< $csg$; $ <:class_sig_item<>> $ >> ->
        self#class_sig_item csg
    | csg -> super#class_sig_item csg ];

    method class_str_item = fun
    [ <:class_str_item< $ <:class_str_item<>> $; $cst$ >> |
      <:class_str_item< $cst$; $ <:class_str_item<>> $ >> ->
        self#class_str_item cst
    | cst -> super#class_str_item cst ];

  end;

end;
