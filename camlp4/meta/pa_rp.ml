(* camlp4r pa_extend.cmo q_MLast.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Pcaml;

type spat_comp =
  [ SpTrm of MLast.loc and MLast.patt and option MLast.expr
  | SpNtr of MLast.loc and MLast.patt and MLast.expr
  | SpStr of MLast.loc and MLast.patt ]
;
type sexp_comp =
  [ SeTrm of MLast.loc and MLast.expr | SeNtr of MLast.loc and MLast.expr ]
;

value strm_n = "strm__";
value peek_fun loc = <:expr< Stream.peek >>;
value junk_fun loc = <:expr< Stream.junk >>;

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
  [ <:expr< try $te$ with [ Stream.Failure -> $e$] >> -> handle_failure e
  | <:expr< match $me$ with [ $list:pel$ ] >> ->
      handle_failure me &&
      List.for_all
        (fun
         [ (_, None, e) -> handle_failure e
         | _ -> False ])
        pel
  | <:expr< let $list:pel$ in $e$ >> ->
      List.for_all (fun (p, e) -> handle_failure e) pel && handle_failure e
  | <:expr< $lid:_$ >> | <:expr< $int:_$ >> | <:expr< $str:_$ >> |
    <:expr< $chr:_$ >> | <:expr< fun [ $list:_$ ] >> | <:expr< $uid:_$ >> ->
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
  let loc = MLast.loc_of_expr e in
  match e with
  [ <:expr< $lid:x$ >> ->
      let x = if x = v then strm_n else x in <:expr< $lid:x$ >>
  | <:expr< $uid:_$ >> -> e
  | <:expr< $int:_$ >> -> e
  | <:expr< $chr:_$ >> -> e
  | <:expr< $str:_$ >> -> e
  | <:expr< $_$.$_$ >> -> e
  | <:expr< let $opt:rf$ $list:pel$ in $e$ >> ->
      <:expr< let $opt:rf$ $list:List.map (subst_pe v) pel$ in $subst v e$ >>
  | <:expr< $e1$ $e2$ >> -> <:expr< $subst v e1$ $subst v e2$ >>
  | <:expr< ( $list:el$ ) >> -> <:expr< ( $list:List.map (subst v) el$ ) >>
  | _ -> raise Not_found ]
and subst_pe v (p, e) =
  match p with
  [ <:patt< $lid:v'$ >> when v <> v' -> (p, subst v e)
  | _ -> raise Not_found ]
;

value stream_pattern_component skont ckont =
  fun
  [ SpTrm loc p wo ->
      <:expr< match $peek_fun loc$ $lid:strm_n$ with
              [ Some $p$ $when:wo$ ->
                  do { $junk_fun loc$ $lid:strm_n$; $skont$ }
              | _ -> $ckont$ ] >>
  | SpNtr loc p e ->
      let e =
        match e with
        [ <:expr< fun [ ($lid:v$ : Stream.t _) -> $e$ ] >> when v = strm_n -> e
        | _ -> <:expr< $e$ $lid:strm_n$ >> ]
      in
      if pattern_eq_expression p skont then
        if is_raise_failure ckont then e
        else if handle_failure e then e
        else <:expr< try $e$ with [ Stream.Failure -> $ckont$ ] >>
      else if is_raise_failure ckont then <:expr< let $p$ = $e$ in $skont$ >>
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
  | SpStr loc p ->
      try
        match p with
        [ <:patt< $lid:v$ >> -> subst v skont
        | _ -> raise Not_found ]
      with
      [ Not_found -> <:expr< let $p$ = $lid:strm_n$ in $skont$ >> ] ]
;

value rec stream_pattern loc epo e ekont =
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
        stream_pattern loc epo e ekont spcl
      in
      let ckont = ekont err in stream_pattern_component skont ckont spc ]
;

value stream_patterns_term loc ekont tspel =
  let pel =
    List.map
      (fun (p, w, loc, spcl, epo, e) ->
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
           let skont = stream_pattern loc epo e ekont spcl in
           <:expr< do { $junk_fun loc$ $lid:strm_n$; $skont$ } >>
         in
         (p, w, e))
      tspel
  in
  let pel = pel @ [(<:patt< _ >>, None, ekont ())] in
  <:expr< match $peek_fun loc$ $lid:strm_n$ with [ $list:pel$ ] >>
;

value rec group_terms =
  fun
  [ [([(SpTrm loc p w, None) :: spcl], epo, e) :: spel] ->
      let (tspel, spel) = group_terms spel in
      ([(p, w, loc, spcl, epo, e) :: tspel], spel)
  | spel -> ([], spel) ]
;

value rec parser_cases loc =
  fun
  [ [] -> <:expr< raise Stream.Failure >>
  | spel ->
      match group_terms spel with
      [ ([], [(spcl, epo, e) :: spel]) ->
          stream_pattern loc epo e (fun _ -> parser_cases loc spel) spcl
      | (tspel, spel) ->
          stream_patterns_term loc (fun _ -> parser_cases loc spel) tspel ] ]
;

value cparser loc bpo pc =
  let e = parser_cases loc pc in
  let e =
    match bpo with
    [ Some bp -> <:expr< let $bp$ = Stream.count $lid:strm_n$ in $e$ >>
    | None -> e ]
  in
  let p = <:patt< ($lid:strm_n$ : Stream.t _) >> in <:expr< fun $p$ -> $e$ >>
;

value cparser_match loc me bpo pc =
  let pc = parser_cases loc pc in
  let e =
    match bpo with
    [ Some bp -> <:expr< let $bp$ = Stream.count $lid:strm_n$ in $pc$ >>
    | None -> pc ]
  in
  match me with
  [ <:expr< $lid:x$ >> when x = strm_n -> e
  | _ -> <:expr< let ($lid:strm_n$ : Stream.t _) = $me$ in $e$ >> ]
;

(* streams *)

value rec not_computing =
  fun
  [ <:expr< $lid:_$ >> | <:expr< $uid:_$ >> | <:expr< $int:_$ >> |
    <:expr< $flo:_$ >> | <:expr< $chr:_$ >> | <:expr< $str:_$ >> ->
      True
  | <:expr< $x$ $y$ >> -> is_cons_apply_not_computing x && not_computing y
  | _ -> False ]
and is_cons_apply_not_computing =
  fun
  [ <:expr< $uid:_$ >> -> True
  | <:expr< $lid:_$ >> -> False
  | <:expr< $x$ $y$ >> -> is_cons_apply_not_computing x && not_computing y
  | _ -> False ]
;

value slazy loc e =
  match e with
  [ <:expr< $f$ () >> ->
      match f with
      [ <:expr< $lid:_$ >> -> f
      | _ -> <:expr< fun _ -> $e$ >> ]
  | _ -> <:expr< fun _ -> $e$ >> ]
;

value rec cstream gloc =
  fun
  [ [] -> let loc = gloc in <:expr< Stream.sempty >>
  | [SeTrm loc e] ->
      if not_computing e then <:expr< Stream.ising $e$ >>
      else <:expr< Stream.lsing $slazy loc e$ >>
  | [SeTrm loc e :: secl] ->
      if not_computing e then <:expr< Stream.icons $e$ $cstream gloc secl$ >>
      else <:expr< Stream.lcons $slazy loc e$ $cstream gloc secl$ >>
  | [SeNtr loc e] ->
      if not_computing e then e else <:expr< Stream.slazy $slazy loc e$ >>
  | [SeNtr loc e :: secl] ->
      if not_computing e then <:expr< Stream.iapp $e$ $cstream gloc secl$ >>
      else <:expr< Stream.lapp $slazy loc e$ $cstream gloc secl$ >> ]
;

(* Syntax extensions in Revised Syntax grammar *)

EXTEND
  GLOBAL: expr;
  expr: LEVEL "top"
    [ [ "parser"; po = OPT ipatt; "["; pcl = LIST0 parser_case SEP "|"; "]" ->
          <:expr< $cparser loc po pcl$ >>
      | "parser"; po = OPT ipatt; pc = parser_case ->
          <:expr< $cparser loc po [pc]$ >>
      | "match"; e = SELF; "with"; "parser"; po = OPT ipatt; "[";
        pcl = LIST0 parser_case SEP "|"; "]" ->
          <:expr< $cparser_match loc e po pcl$ >>
      | "match"; e = SELF; "with"; "parser"; po = OPT ipatt;
        pc = parser_case ->
          <:expr< $cparser_match loc e po [pc]$ >> ] ]
  ;
  parser_case:
    [ [ "[:"; sp = stream_patt; ":]"; po = OPT ipatt; "->"; e = expr ->
          (sp, po, e) ] ]
  ;
  stream_patt:
    [ [ spc = stream_patt_comp -> [(spc, None)]
      | spc = stream_patt_comp; ";";
        sp = LIST1 stream_patt_comp_err SEP ";" ->
          [(spc, None) :: sp]
      | -> [] ] ]
  ;
  stream_patt_comp_err:
    [ [ spc = stream_patt_comp; eo = OPT [ "?"; e = expr -> e ] ->
          (spc, eo) ] ]
  ;
  stream_patt_comp:
    [ [ "`"; p = patt; eo = OPT [ "when"; e = expr -> e ] -> SpTrm loc p eo
      | p = patt; "="; e = expr -> SpNtr loc p e
      | p = patt -> SpStr loc p ] ]
  ;
  ipatt:
    [ [ i = LIDENT -> <:patt< $lid:i$ >> ] ]
  ;
  expr: LEVEL "simple"
    [ [ "[:"; se = LIST0 stream_expr_comp SEP ";"; ":]" ->
          <:expr< $cstream loc se$ >> ] ]
  ;
  stream_expr_comp:
    [ [ "`"; e = expr -> SeTrm loc e | e = expr -> SeNtr loc e ] ]
  ;
END;
