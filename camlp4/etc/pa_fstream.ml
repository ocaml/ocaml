(* camlp4r pa_extend.cmo q_MLast.cmo *)
(* $Id$ *)

open Pcaml;

type spat_comp =
  [ SpTrm of MLast.loc and MLast.patt and option MLast.expr
  | SpNtr of MLast.loc and MLast.patt and MLast.expr
  | SpStr of MLast.loc and MLast.patt ]
;
type sexp_comp =
  [ SeTrm of MLast.loc and MLast.expr
  | SeNtr of MLast.loc and MLast.expr ]
;

(* parsers *)

value strm_n = "strm__";
value next_fun loc = <:expr< Fstream.next >>;

value rec pattern_eq_expression p e =
  match (p, e) with
  [ (<:patt< $lid:a$ >>, <:expr< $lid:b$ >>) -> a = b
  | (<:patt< $uid:a$ >>, <:expr< $uid:b$ >>) -> a = b
  | (<:patt< $p1$ $p2$ >>, <:expr< $e1$ $e2$ >>) ->
      pattern_eq_expression p1 e1 && pattern_eq_expression p2 e2
  | (<:patt< ($list:pl$) >>, <:expr< ($list:el$) >>) ->
      loop pl el where rec loop pl el =
        match (pl, el) with
        [ ([p :: pl], [e :: el]) ->
            pattern_eq_expression p e && loop pl el
        | ([], []) -> True
        | _ -> False ]
  | _ -> False ]
;

value stream_pattern_component skont =
  fun
  [ SpTrm loc p wo ->
      let p = <:patt< Some ($p$, $lid:strm_n$) >> in
      if wo = None && pattern_eq_expression p skont then
        <:expr< $next_fun loc$ $lid:strm_n$ >>
      else
        <:expr< match $next_fun loc$ $lid:strm_n$ with
                [ $p$ $when:wo$ -> $skont$
                | _ -> None ] >>
  | SpNtr loc p e ->
      let p = <:patt< Some ($p$, $lid:strm_n$) >> in
      if pattern_eq_expression p skont then <:expr< $e$ $lid:strm_n$ >>
      else
        <:expr< match $e$ $lid:strm_n$ with
                [ $p$ -> $skont$
                | _ -> None ] >>
  | SpStr loc p ->
      <:expr< let $p$ = $lid:strm_n$ in $skont$ >> ]
;

value rec stream_pattern loc epo e =
  fun
  [ [] ->
      let e =
        match epo with
        [ Some ep -> <:expr< let $ep$ = Fstream.count $lid:strm_n$ in $e$ >>
        | None -> e ]
      in
      <:expr< Some ($e$, $lid:strm_n$) >>
  | [spc :: spcl] ->
      let skont = stream_pattern loc epo e spcl in
      stream_pattern_component skont spc ]
;

value rec parser_cases loc =
  fun
  [ [] -> <:expr< None >>
  | [(spcl, epo, e) :: spel] ->
      match parser_cases loc spel with
      [ <:expr< None >> -> stream_pattern loc epo e spcl
      | pc ->
          <:expr< match $stream_pattern loc epo e spcl$ with
                  [ Some _ as x -> x
                  | None -> $pc$ ] >> ] ]
;

value cparser_match loc me bpo pc =
  let pc = parser_cases loc pc in
  let e =
    match bpo with
    [ Some bp -> <:expr< let $bp$ = Stream.count $lid:strm_n$ in $pc$ >>
    | None -> pc ]
  in
  <:expr< let ($lid:strm_n$ : Stream.t _) = $me$ in $e$ >>
;

value cparser loc bpo pc =
  let e = parser_cases loc pc in
  let e =
    match bpo with
    [ Some bp -> <:expr< let $bp$ = Fstream.count $lid:strm_n$ in $e$ >>
    | None -> e ]
  in
  let p = <:patt< ($lid:strm_n$ : Fstream.t _) >> in <:expr< fun $p$ -> $e$ >>
;

(* streams *)

value slazy loc x = <:expr< fun () -> $x$ >>;

value rec cstream loc =
  fun
  [ [] -> <:expr< Fstream.nil >>
  | [SeTrm loc e :: sel] ->
      let e2 = cstream loc sel in
      let x = <:expr< Fstream.cons $e$ $e2$ >> in
      <:expr< Fstream.flazy $slazy loc x$ >>
  | [SeNtr loc e] ->
      e
  | [SeNtr loc e :: sel] ->
      let e2 = cstream loc sel in
      let x = <:expr< Fstream.app $e$ $e2$ >> in
      <:expr< Fstream.flazy $slazy loc x$ >> ]
;

EXTEND
  GLOBAL: expr;
  expr: LEVEL "top"
    [ [ "fparser"; po = OPT ipatt; "["; pcl = LIST0 parser_case SEP "|"; "]" ->
          <:expr< $cparser loc po pcl$ >>
      | "fparser"; po = OPT ipatt; pc = parser_case ->
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
    [ [ spc = stream_patt_comp -> [spc]
      | spc = stream_patt_comp; ";"; sp = LIST1 stream_patt_comp SEP ";" ->
          [spc :: sp]
      | -> [] ] ]
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
    [ [ "fstream"; "[:"; se = LIST0 stream_expr_comp SEP ";"; ":]" ->
          <:expr< $cstream loc se$ >> ] ]
  ;
  stream_expr_comp:
    [ [ "`"; e = expr -> SeTrm loc e
      | e = expr -> SeNtr loc e ] ]
  ;
END;
