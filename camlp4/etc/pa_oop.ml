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

value stream_pattern_component skont =
  fun
  [ SpTrm loc p wo ->
      (<:expr< $peek_fun loc$ $lid:strm_n$ >>, p, wo,
       <:expr< do { $junk_fun loc$ $lid:strm_n$; $skont$ } >>)
  | SpNtr loc p e ->
      (<:expr< try Some ($e$ $lid:strm_n$) with
               [ Stream.Failure -> None ] >>,
       p, None, skont)
  | SpStr loc p ->
      (<:expr< Some $lid:strm_n$ >>, p, None, skont) ]
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
      let (tst, p, wo, e) = stream_pattern_component skont spc in
      let ckont = ekont err in
      <:expr< match $tst$ with
              [ Some $p$ $when:wo$ -> $e$ | _ -> $ckont$ ] >> ]
;

value rec parser_cases loc =
  fun
  [ [] -> <:expr< raise Stream.Failure >>
  | [(spcl, epo, e) :: spel] ->
      stream_pattern loc epo e (fun _ -> parser_cases loc spel) spcl ]
;

value cparser loc bpo pc =
  let e = parser_cases loc pc in
  let e =
    match bpo with
    [ Some bp -> <:expr< let $bp$ = Stream.count $lid:strm_n$ in $e$ >>
    | None -> e ]
  in
  let p = <:patt< ($lid:strm_n$ : Stream.t _) >> in
  <:expr< fun $p$ -> $e$ >>
;

value cparser_match loc me bpo pc =
  let pc = parser_cases loc pc in
  let e =
    match bpo with
    [ Some bp -> <:expr< let $bp$ = Stream.count $lid:strm_n$ in $pc$ >>
    | None -> pc ]
  in
  <:expr< let $lid:strm_n$ = $me$ in $e$ >>
;

(* streams *)

value slazy loc e = <:expr< fun _ -> $e$ >>;

value rec cstream gloc =
  fun
  [ [] -> let loc = gloc in <:expr< Stream.sempty >>
  | [SeTrm loc e :: secl] ->
      <:expr< Stream.lcons $slazy loc e$ $cstream gloc secl$ >>
  | [SeNtr loc e :: secl] ->
      <:expr< Stream.lapp $slazy loc e$ $cstream gloc secl$ >> ]
;

(* Syntax extensions in Ocaml grammar *)


EXTEND
  GLOBAL: expr;
  expr: LEVEL "expr1"
    [ [ "parser"; po = OPT ipatt; OPT "|"; pcl = LIST1 parser_case SEP "|" ->
          <:expr< $cparser loc po pcl$ >>
      | "match"; e = expr; "with"; "parser"; po = OPT ipatt; OPT "|";
        pcl = LIST1 parser_case SEP "|" ->
          <:expr< $cparser_match loc e po pcl$ >> ] ]
  ;
  parser_case:
    [ [ "[<"; sp = stream_patt; ">]"; po = OPT ipatt; "->"; e = expr ->
          (sp, po, e) ] ]
  ;
  stream_patt:
    [ [ spc = stream_patt_comp -> [(spc, None)]
      | spc = stream_patt_comp; ";"; sp = LIST1 stream_patt_comp_err SEP ";" ->
          [(spc, None) :: sp]
      | (* empty *) -> [] ] ]
  ;
  stream_patt_comp_err:
    [ [ spc = stream_patt_comp;
        eo = OPT [ "??"; e = expr LEVEL "expr1" -> e ] ->
          (spc, eo) ] ]
  ;
  stream_patt_comp:
    [ [ "'"; p = patt; eo = OPT [ "when"; e = (expr LEVEL "expr1") -> e ] ->
         SpTrm loc p eo
     | p = patt; "="; e = (expr LEVEL "expr1") -> SpNtr loc p e
     | p = patt -> SpStr loc p ] ]
  ;
  ipatt:
    [ [ i = LIDENT -> <:patt< $lid:i$ >> ] ]
  ;

  expr: LEVEL "simple"
    [ [ "[<"; se = LIST0 stream_expr_comp SEP ";"; ">]" ->
          <:expr< $cstream loc se$ >> ] ]
  ;
  stream_expr_comp:
    [ [ "'"; e = expr LEVEL "expr1" -> SeTrm loc e
      | e = expr LEVEL "expr1" -> SeNtr loc e ] ]
  ;
END;
