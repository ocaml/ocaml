open Camlp4;                                        (* -*- camlp4r -*- *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 1998-2006 Institut National de Recherche en Informatique et   *)
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


module Id : Sig.Id = struct
  value name = "Camlp4OCamlRevisedParserParser";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  type spat_comp =
    [ SpTrm of Loc.t and Ast.patt and option Ast.expr
    | SpNtr of Loc.t and Ast.patt and Ast.expr
    | SpStr of Loc.t and Ast.patt ]
  ;
  type sexp_comp =
    [ SeTrm of Loc.t and Ast.expr | SeNtr of Loc.t and Ast.expr ]
  ;

  value stream_expr = Gram.Entry.mk "stream_expr";
  value stream_begin = Gram.Entry.mk "stream_begin";
  value stream_end = Gram.Entry.mk "stream_end";
  value stream_quot = Gram.Entry.mk "stream_quot";
  value parser_case = Gram.Entry.mk "parser_case";
  value parser_case_list = Gram.Entry.mk "parser_case_list";

  value strm_n = "__strm";
  value peek_fun _loc = <:expr< Stream.peek >>;
  value junk_fun _loc = <:expr< Stream.junk >>;

  (* Parsers. *)
  (* In syntax generated, many cases are optimisations. *)

  value rec pattern_eq_expression p e =
    match (p, e) with
    [ (<:patt< $lid:a$ >>, <:expr< $lid:b$ >>) -> a = b
    | (<:patt< $uid:a$ >>, <:expr< $uid:b$ >>) -> a = b
    | (<:patt< $p1$ $p2$ >>, <:expr< $e1$ $e2$ >>) ->
        pattern_eq_expression p1 e1 && pattern_eq_expression p2 e2
    | _ -> False ]
  ;

  value is_raise e =
    match e with
    [ <:expr< raise $_$ >> -> True
    | _ -> False ]
  ;

  value is_raise_failure e =
    match e with
    [ <:expr< raise Stream.Failure >> -> True
    | _ -> False ]
  ;

  value rec handle_failure e =
    match e with
    [ <:expr< try $_$ with [ Stream.Failure -> $e$] >> ->
        handle_failure e
    | <:expr< match $me$ with [ $a$ ] >> ->
        let rec match_case_handle_failure =
          fun
          [ <:match_case< $a1$ | $a2$ >> ->
              match_case_handle_failure a1 && match_case_handle_failure a2
          | <:match_case< $pat:_$ -> $e$ >> -> handle_failure e
          | _ -> False ]
        in handle_failure me && match_case_handle_failure a
    | <:expr< let $bi$ in $e$ >> ->
        let rec binding_handle_failure =
          fun
          [ <:binding< $b1$ and $b2$ >> ->
              binding_handle_failure b1 && binding_handle_failure b2
          | <:binding< $_$ = $e$ >> -> handle_failure e
          | _ -> False ]
        in binding_handle_failure bi && handle_failure e
    | <:expr< $lid:_$ >> | <:expr< $int:_$ >> | <:expr< $str:_$ >> |
      <:expr< $chr:_$ >> | <:expr< fun [ $_$ ] >> | <:expr< $uid:_$ >> ->
        True
    | <:expr< raise $e$ >> ->
        match e with
        [ <:expr< Stream.Failure >> -> False
        | _ -> True ]
    | <:expr< $f$ $x$ >> ->
        is_constr_apply f && handle_failure f && handle_failure x
    | _ -> False ]
  and is_constr_apply =
    fun
    [ <:expr< $uid:_$ >> -> True
    | <:expr< $lid:_$ >> -> False
    | <:expr< $x$ $_$ >> -> is_constr_apply x
    | _ -> False ]
  ;

  value rec subst v e =
    let _loc = Ast.loc_of_expr e in
    match e with
    [ <:expr< $lid:x$ >> ->
        let x = if x = v then strm_n else x in
        <:expr< $lid:x$ >>
    | <:expr< $uid:_$ >> -> e
    | <:expr< $int:_$ >> -> e
    | <:expr< $chr:_$ >> -> e
    | <:expr< $str:_$ >> -> e
    | <:expr< $_$ . $_$ >> -> e
    | <:expr< let $rec:rf$ $bi$ in $e$ >> ->
        <:expr< let $rec:rf$ $subst_binding v bi$ in $subst v e$ >>
    | <:expr< $e1$ $e2$ >> -> <:expr< $subst v e1$ $subst v e2$ >>
    | <:expr< ( $tup:e$ ) >> -> <:expr< ( $tup:subst v e$ ) >>
    | <:expr< $e1$, $e2$ >> -> <:expr< $subst v e1$, $subst v e2$ >>
    | _ -> raise Not_found ]
  and subst_binding v =
    fun
    [ <:binding@_loc< $b1$ and $b2$ >> ->
        <:binding< $subst_binding v b1$ and $subst_binding v b2$ >>
    | <:binding@_loc< $lid:v'$ = $e$ >> ->
        <:binding< $lid:v'$ = $if v = v' then e else subst v e$ >>
    | _ -> raise Not_found ];

  value stream_pattern_component skont ckont =
    fun
    [ SpTrm _loc p None ->
        <:expr< match $peek_fun _loc$ $lid:strm_n$ with
                [ Some $p$ ->
                    do { $junk_fun _loc$ $lid:strm_n$; $skont$ }
                | _ -> $ckont$ ] >>
    | SpTrm _loc p (Some w) ->
        <:expr< match $peek_fun _loc$ $lid:strm_n$ with
                [ Some $p$ when $w$ ->
                    do { $junk_fun _loc$ $lid:strm_n$; $skont$ }
                | _ -> $ckont$ ] >>
    | SpNtr _loc p e ->
        let e =
          match e with
          [ <:expr< fun [ ($lid:v$ : Stream.t _) -> $e$ ] >> when v = strm_n -> e
          | _ -> <:expr< $e$ $lid:strm_n$ >> ]
        in
        if pattern_eq_expression p skont then
          if is_raise_failure ckont then e
          else if handle_failure e then e
          else <:expr< try $e$ with [ Stream.Failure -> $ckont$ ] >>
        else if is_raise_failure ckont then
          <:expr< let $p$ = $e$ in $skont$ >>
        else if pattern_eq_expression <:patt< Some $p$ >> skont then
          <:expr< try Some $e$ with [ Stream.Failure -> $ckont$ ] >>
        else if is_raise ckont then
          let tst =
            if handle_failure e then e
            else <:expr< try $e$ with [ Stream.Failure -> $ckont$ ] >>
          in
          <:expr< let $p$ = $tst$ in $skont$ >>
        else
          <:expr< match try Some $e$ with [ Stream.Failure -> None ] with
                  [ Some $p$ -> $skont$
                  | _ -> $ckont$ ] >>
    | SpStr _loc p ->
        try
          match p with
          [ <:patt< $lid:v$ >> -> subst v skont
          | _ -> raise Not_found ]
        with
        [ Not_found -> <:expr< let $p$ = $lid:strm_n$ in $skont$ >> ] ]
  ;

  value rec stream_pattern _loc epo e ekont =
    fun
    [ [] ->
        match epo with
        [ Some ep -> <:expr< let $ep$ = Stream.count $lid:strm_n$ in $e$ >>
        | _ -> e ]
    | [(spc, err) :: spcl] ->
        let skont =
          let ekont err =
            let str =
              match err with
              [ Some estr -> estr
              | _ -> <:expr< "" >> ]
            in
            <:expr< raise (Stream.Error $str$) >>
          in
          stream_pattern _loc epo e ekont spcl
        in
        let ckont = ekont err in stream_pattern_component skont ckont spc ]
  ;

  value stream_patterns_term _loc ekont tspel =
    let pel =
      List.fold_right
        (fun (p, w, _loc, spcl, epo, e) acc ->
          let p = <:patt< Some $p$ >> in
          let e =
            let ekont err =
              let str =
                match err with
                [ Some estr -> estr
                | _ -> <:expr< "" >> ]
              in
              <:expr< raise (Stream.Error $str$) >>
            in
            let skont = stream_pattern _loc epo e ekont spcl in
            <:expr< do { $junk_fun _loc$ $lid:strm_n$; $skont$ } >>
          in
          match w with
          [ Some w -> <:match_case< $pat:p$ when $w$ -> $e$ | $acc$ >>
          | None -> <:match_case< $pat:p$ -> $e$ | $acc$ >> ])
        tspel <:match_case<>>
    in
    <:expr< match $peek_fun _loc$ $lid:strm_n$ with [ $pel$ | _ -> $ekont ()$ ] >>
  ;

  value rec group_terms =
    fun
    [ [([(SpTrm _loc p w, None) :: spcl], epo, e) :: spel] ->
        let (tspel, spel) = group_terms spel in
        ([(p, w, _loc, spcl, epo, e) :: tspel], spel)
    | spel -> ([], spel) ]
  ;

  value rec parser_cases _loc =
    fun
    [ [] -> <:expr< raise Stream.Failure >>
    | spel ->
        match group_terms spel with
        [ ([], [(spcl, epo, e) :: spel]) ->
            stream_pattern _loc epo e (fun _ -> parser_cases _loc spel) spcl
        | (tspel, spel) ->
            stream_patterns_term _loc (fun _ -> parser_cases _loc spel) tspel ] ]
  ;

  value cparser _loc bpo pc =
    let e = parser_cases _loc pc in
    let e =
      match bpo with
      [ Some bp -> <:expr< let $bp$ = Stream.count $lid:strm_n$ in $e$ >>
      | None -> e ]
    in
    let p = <:patt< ($lid:strm_n$ : Stream.t _) >> in
    <:expr< fun $p$ -> $e$ >>
  ;

  value cparser_match _loc me bpo pc =
    let pc = parser_cases _loc pc in
    let e =
      match bpo with
      [ Some bp -> <:expr< let $bp$ = Stream.count $lid:strm_n$ in $pc$ >>
      | None -> pc ]
    in
    let me =
      match me with
      [ <:expr@_loc< $_$; $_$ >> as e -> <:expr< do { $e$ } >>
      | e -> e ]
    in
    match me with
    [ <:expr< $lid:x$ >> when x = strm_n -> e
    | _ -> <:expr< let ($lid:strm_n$ : Stream.t _) = $me$ in $e$ >> ]
  ;

  (* streams *)

  value rec not_computing =
    fun
    [ <:expr< $lid:_$ >> | <:expr< $uid:_$ >> | <:expr< $int:_$ >> |
      <:expr< $flo:_$ >> | <:expr< $chr:_$ >> | <:expr< $str:_$ >> -> True
    | <:expr< $x$ $y$ >> -> is_cons_apply_not_computing x && not_computing y
    | _ -> False ]
  and is_cons_apply_not_computing =
    fun
    [ <:expr< $uid:_$ >> -> True
    | <:expr< $lid:_$ >> -> False
    | <:expr< $x$ $y$ >> -> is_cons_apply_not_computing x && not_computing y
    | _ -> False ]
  ;

  value slazy _loc e =
    match e with
    [ <:expr< $f$ () >> ->
        match f with
        [ <:expr< $lid:_$ >> -> f
        | _ -> <:expr< fun _ -> $e$ >> ]
    | _ -> <:expr< fun _ -> $e$ >> ]
  ;

  value rec cstream gloc =
    fun
    [ [] -> let _loc = gloc in <:expr< Stream.sempty >>
    | [SeTrm _loc e] ->
        if not_computing e then <:expr< Stream.ising $e$ >>
        else <:expr< Stream.lsing $slazy _loc e$ >>
    | [SeTrm _loc e :: secl] ->
        if not_computing e then <:expr< Stream.icons $e$ $cstream gloc secl$ >>
        else <:expr< Stream.lcons $slazy _loc e$ $cstream gloc secl$ >>
    | [SeNtr _loc e] ->
        if not_computing e then e else <:expr< Stream.slazy $slazy _loc e$ >>
    | [SeNtr _loc e :: secl] ->
        if not_computing e then <:expr< Stream.iapp $e$ $cstream gloc secl$ >>
        else <:expr< Stream.lapp $slazy _loc e$ $cstream gloc secl$ >> ]
  ;
  (* Syntax extensions in Revised Syntax grammar *)

  EXTEND Gram
    GLOBAL: expr stream_expr stream_begin stream_end stream_quot
      parser_case parser_case_list;
    expr: LEVEL "top"
      [ [ "parser"; po = OPT parser_ipatt; pcl = parser_case_list ->
            <:expr< $cparser _loc po pcl$ >>
        | "match"; e = sequence; "with"; "parser"; po = OPT parser_ipatt;
          pcl = parser_case_list ->
            <:expr< $cparser_match _loc e po pcl$ >>
      ] ]
    ;
    parser_case_list:
      [ [ "["; pcl = LIST0 parser_case SEP "|"; "]" -> pcl
        | pc = parser_case -> [pc]
      ] ]
    ;
    parser_case:
      [ [ stream_begin; sp = stream_patt; stream_end; po = OPT parser_ipatt; "->"; e = expr ->
            (sp, po, e) ] ]
    ;
    stream_begin:
      [ [ "[:" -> () ] ]
    ;
    stream_end:
      [ [ ":]" -> () ] ]
    ;
    stream_quot:
      [ [ "`" -> () ] ]
    ;
    stream_expr:
      [ [ e = expr -> e ] ]
    ;
    stream_patt:
      [ [ spc = stream_patt_comp -> [(spc, None)]
        | spc = stream_patt_comp; ";"; sp = stream_patt_comp_err_list ->
            [(spc, None) :: sp]
        | -> [] ] ]
    ;
    stream_patt_comp_err:
      [ [ spc = stream_patt_comp; eo = OPT [ "??"; e = stream_expr -> e ] ->
            (spc, eo) ] ]
    ;
    stream_patt_comp_err_list:
      [ [ spc = stream_patt_comp_err -> [spc]
        | spc = stream_patt_comp_err; ";" -> [spc]
        | spc = stream_patt_comp_err; ";"; sp = stream_patt_comp_err_list ->
            [spc :: sp] ] ]
    ;
    stream_patt_comp:
      [ [ stream_quot; p = patt; eo = OPT [ "when"; e = stream_expr -> e ] -> SpTrm _loc p eo
        | p = patt; "="; e = stream_expr -> SpNtr _loc p e
        | p = patt -> SpStr _loc p ] ]
    ;
    parser_ipatt:
      [ [ i = a_LIDENT -> <:patt< $lid:i$ >>
        | "_" -> <:patt< _ >>
      ] ]
    ;
    expr: LEVEL "simple"
      [ [ stream_begin; stream_end -> <:expr< $cstream _loc []$ >>
        | stream_begin; sel = stream_expr_comp_list; stream_end ->
            <:expr< $cstream _loc sel$ >> ] ]
    ;
    stream_expr_comp_list:
      [ [ se = stream_expr_comp; ";"; sel = stream_expr_comp_list -> [se :: sel]
        | se = stream_expr_comp; ";" -> [se]
        | se = stream_expr_comp -> [se] ] ]
    ;
    stream_expr_comp:
      [ [ stream_quot; e = stream_expr -> SeTrm _loc e
        | e = stream_expr -> SeNtr _loc e ] ]
    ;
  END;

end;

module M = Register.OCamlSyntaxExtension Id Make;
