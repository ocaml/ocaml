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



open Pcaml;

type spat_comp =
  [ SpTrm of Loc.t and MLast.patt and option MLast.expr
  | SpNtr of Loc.t and MLast.patt and MLast.expr
  | SpStr of Loc.t and MLast.patt ]
;
type sexp_comp =
  [ SeTrm of Loc.t and MLast.expr | SeNtr of Loc.t and MLast.expr ]
;

value strm_n = "__strm";
value peek_fun _loc = <:expr< Stream.peek >>;
value junk_fun _loc = <:expr< Stream.junk >>;

(* Parsers. *)

value stream_pattern_component skont =
  fun
  [ SpTrm _loc p wo ->
      (<:expr< $peek_fun _loc$ $lid:strm_n$ >>, p, wo,
       <:expr< do { $junk_fun _loc$ $lid:strm_n$; $skont$ } >>)
  | SpNtr _loc p e ->
      (<:expr< try Some ($e$ $lid:strm_n$) with
               [ Stream.Failure -> None ] >>,
       p, None, skont)
  | SpStr _loc p ->
      (<:expr< Some $lid:strm_n$ >>, p, None, skont) ]
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
      let (tst, p, wo, e) = stream_pattern_component skont spc in
      let ckont = ekont err in
      <:expr< match $tst$ with
              [ Some $p$ $when:wo$ -> $e$ | _ -> $ckont$ ] >> ]
;

value rec parser_cases _loc =
  fun
  [ [] -> <:expr< raise Stream.Failure >>
  | [(spcl, epo, e) :: spel] ->
      stream_pattern _loc epo e (fun _ -> parser_cases _loc spel) spcl ]
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
  <:expr< let $lid:strm_n$ = $me$ in $e$ >>
;

(* streams *)

value slazy _loc e = <:expr< fun _ -> $e$ >>;

value rec cstream gloc =
  fun
  [ [] -> let _loc = gloc in <:expr< Stream.sempty >>
  | [SeTrm _loc e :: secl] ->
      <:expr< Stream.lcons $slazy _loc e$ $cstream gloc secl$ >>
  | [SeNtr _loc e :: secl] ->
      <:expr< Stream.lapp $slazy _loc e$ $cstream gloc secl$ >> ]
;

(* Syntax extensions in OCaml grammar *)


EXTEND
  GLOBAL: expr;
  expr: LEVEL "expr1"
    [ [ "parser"; po = OPT ipatt; OPT "|"; pcl = LIST1 parser_case SEP "|" ->
          <:expr< $cparser _loc po pcl$ >>
      | "match"; e = expr; "with"; "parser"; po = OPT ipatt; OPT "|";
        pcl = LIST1 parser_case SEP "|" ->
          <:expr< $cparser_match _loc e po pcl$ >> ] ]
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
         SpTrm _loc p eo
     | p = patt; "="; e = (expr LEVEL "expr1") -> SpNtr _loc p e
     | p = patt -> SpStr _loc p ] ]
  ;
  ipatt:
    [ [ i = LIDENT -> <:patt< $lid:i$ >> ] ]
  ;

  expr: LEVEL "simple"
    [ [ "[<"; se = LIST0 stream_expr_comp SEP ";"; ">]" ->
          <:expr< $cstream _loc se$ >> ] ]
  ;
  stream_expr_comp:
    [ [ "'"; e = expr LEVEL "expr1" -> SeTrm _loc e
      | e = expr LEVEL "expr1" -> SeNtr _loc e ] ]
  ;
END;
