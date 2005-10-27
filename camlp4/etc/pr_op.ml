(* camlp4r q_MLast.cmo ./pa_extfun.cmo *)
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
open Spretty;

value expr e dg k = pr_expr.pr_fun "top" e dg k;
value patt e dg k = pr_patt.pr_fun "top" e dg k;

value spatt p dg k =
  match p with
  [ <:patt< $lid:s$ >> ->
      if String.length s >= 2 && s.[1] == ''' then
        HVbox [: `S LR (" " ^ s); k :]
      else patt p dg k
  | _ -> patt p dg k ]
;

(* Streams *)

value stream e _ k =
  let rec get =
    fun
    [ <:expr< Stream.iapp $x$ $y$ >> -> [(False, x) :: get y]
    | <:expr< Stream.icons $x$ $y$ >> -> [(True, x) :: get y]
    | <:expr< Stream.ising $x$ >> -> [(True, x)]
    | <:expr< Stream.lapp (fun _ -> $x$) $y$ >> -> [(False, x) :: get y]
    | <:expr< Stream.lcons (fun _ -> $x$) $y$ >> -> [(True, x) :: get y]
    | <:expr< Stream.lsing (fun _ -> $x$) >> -> [(True, x)]
    | <:expr< Stream.sempty >> -> []
    | <:expr< Stream.slazy (fun _ -> $x$) >> -> [(False, x)]
    | <:expr< Stream.slazy $x$ >> -> [(False, <:expr< $x$ () >>)]
    | e -> [(False, e)] ]
  in
  let elem e dg k =
    match e with
    [ (True, e) -> [: `HOVbox [: `S LO "'"; `expr e dg k :] :]
    | (False, e) -> [: `expr e dg k :] ]
  in
  let rec glop e k =
    match e with
    [ [] -> k
    | [e] -> [: elem e "" k :]
    | [e :: el] -> [: elem e ";" [: `S RO ";" :]; glop el k :] ]
  in
  HOVbox [: `S LR "[<"; glop (get e) [: `S LR ">]"; k :] :]
;

(* Parsers *)

type spc =
  [ SPCterm of (MLast.patt * option MLast.expr)
  | SPCnterm of MLast.patt and MLast.expr
  | SPCsterm of MLast.patt ]
;

exception NotImpl;

value rec subst v e =
  match e with
  [ <:expr< $lid:x$ >> -> if x = "strm__" then <:expr< $lid:v$ >> else e
  | <:expr< $uid:_$ >> -> e
  | <:expr< $int:_$ >> -> e
  | <:expr< $chr:_$ >> -> e
  | <:expr< $str:_$ >> -> e
  | <:expr< $e1$ . $lab$ >> -> <:expr< $subst v e1$ . $lab$ >>
  | <:expr< $x$ $y$ >> -> <:expr< $subst v x$ $subst v y$ >>
  | <:expr< let $lid:s1$ = $e1$ in $e2$ >> ->
      if s1 = v then <:expr< let $lid:s1$ = $subst v e1$ in $e2$ >>
      else <:expr< let $lid:s1$ = $subst v e1$ in $subst v e2$ >>
  | <:expr< let _ = $e1$ in $e2$ >> ->
      <:expr< let _ = $subst v e1$ in $subst v e2$ >>
  | <:expr< ($list:el$) >> -> <:expr< ($list:List.map (subst v) el$) >>
  | _ -> raise NotImpl ]
;

value rec is_free v =
  fun
  [ <:expr< $lid:x$ >> -> x <> v
  | <:expr< $uid:_$ >> -> True
  | <:expr< $int:_$ >> -> True
  | <:expr< $chr:_$ >> -> True
  | <:expr< $str:_$ >> -> True
  | <:expr< $e$ . $_$ >> -> is_free v e
  | <:expr< $x$ $y$ >> -> is_free v x && is_free v y
  | <:expr< let $lid:s1$ = $e1$ in $e2$ >> ->
      is_free v e1 && (s1 = v || is_free v e2)
  | <:expr< let _ = $e1$ in $e2$ >> -> is_free v e1 && is_free v e2
  | <:expr< ($list:el$) >> -> List.for_all (is_free v) el
  | _ -> raise NotImpl ]
;

value free_var_in_expr c e =
  let rec loop_alpha v =
    let x = String.make 1 v in
    if is_free x e then Some x
    else if v = 'z' then None
    else loop_alpha (Char.chr (Char.code v + 1))
  in
  let rec loop_count cnt =
    let x = String.make 1 c ^ string_of_int cnt in
    if is_free x e then x else loop_count (succ cnt)
  in
  try
    match loop_alpha c with
    [ Some v -> v
    | None -> loop_count 1 ]
  with
  [ NotImpl -> "\\%a" ]
;

value parserify =
  fun
  [ <:expr< $e$ strm__ >> -> e
  | e -> <:expr< fun strm__ -> $e$ >> ]
;

value is_raise_failure =
  fun
  [ <:expr< raise Stream.Failure >> -> True
  | _ -> False ]
;

value is_raise_error =
  fun
  [ <:expr< raise (Stream.Error $_$) >> -> True
  | _ -> False ]
;

value semantic e =
  try
    if is_free "strm__" e then e
    else
      let v = free_var_in_expr 's' e in
      <:expr< let $lid:v$ = strm__ in $subst v e$ >>
  with
  [ NotImpl -> e ]
;

value rewrite_parser =
  rewrite True where rec rewrite top ge =
    match ge with
    [ <:expr< let $p$ = try $e$ with [ Stream.Failure -> raise $exc$ ] in
               $sp_kont$ >> ->
        let f = parserify e in
        <:expr<
          match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
          [ Some $p$ -> $rewrite False sp_kont$
          | _ -> raise $exc$ ]
        >>
    | <:expr< let $p$ = Stream.count strm__ in $f$ >> ->
        try
          if is_free "strm__" f then ge
          else
            let v = free_var_in_expr 's' f in
            <:expr<
              let $lid:v$ = strm__ in
              let $p$ = Stream.count strm__ in $subst v f$
            >>
        with
        [ NotImpl -> ge ]
    | <:expr< let $p$ = strm__ in $e$ >> ->
        <:expr< let $p$ = strm__ in $rewrite False e$ >>
    | <:expr< let $p$ = $f$ strm__ in $sp_kont$ >> when top ->
        <:expr<
          match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
          [ Some $p$ -> $rewrite False sp_kont$
          | _ -> raise Stream.Failure ]
        >>
    | <:expr< let $p$ = $e$ in $sp_kont$ >> ->
        if match e with
           [ <:expr< match try Some $_$ with [ Stream.Failure -> None ] with
                      [ $list:_$ ] >>
           | <:expr< match Stream.peek strm__ with [ $list:_$ ] >>
           | <:expr< try $_$ with [ Stream.Failure -> $_$ ] >>
           | <:expr< let $_$ = Stream.count strm__ in $_$ >> -> True
           | _ -> False ]
        then
          let f = rewrite True <:expr< fun strm__ -> $e$ >> in
          let exc =
            if top then <:expr< Stream.Failure >>
            else <:expr< Stream.Error "" >>
          in
          <:expr<
            match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
            [ Some $p$ -> $rewrite False sp_kont$
            | _ -> raise $exc$ ]
          >>
        else semantic ge
    | <:expr< match try Some $e$ with [ Stream.Failure -> None ] with
               [ Some $p$ -> $sp_kont$
               | _ -> $p_kont$ ] >> ->
        let f = parserify e in
        if not top && is_raise_failure p_kont then semantic ge
        else
          let (p, f, sp_kont, p_kont) =
            if top || is_raise_error p_kont then
              (p, f, rewrite False sp_kont, rewrite top p_kont)
            else
              let f =
                <:expr<
                  fun strm__ ->
                    match
                      try Some ($f$ strm__) with [ Stream.Failure -> None ]
                    with
                    [ Some $p$ -> $rewrite False sp_kont$
                    | _ -> $rewrite top p_kont$ ]
                >>
              in
              (<:patt< a >>, f, <:expr< a >>,
               <:expr< raise (Stream.Error "") >>)
          in
          <:expr<
            match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
            [ Some $p$ -> $sp_kont$
            | _ -> $p_kont$ ]
          >>
    | <:expr< match Stream.peek strm__ with [ $list:pel$ ] >> ->
        let rec iter pel =
          match pel with
          [ [(<:patt< Some $p$ >>, eo,
              <:expr< do { Stream.junk strm__; $sp_kont$ } >>);
             (<:patt< _ >>, None, p_kont) :: _] ->
               <:expr<
                 match Stream.peek strm__ with
                 [ Some $p$ $when:eo$ ->
                     do { Stream.junk strm__; $rewrite False sp_kont$ }
                 | _ -> $rewrite top p_kont$ ]
               >>
          | [(<:patt< Some $p$ >>, eo,
              <:expr< do { Stream.junk strm__; $sp_kont$ } >>) :: pel] ->
               let p_kont = iter pel in
               <:expr<
                 match Stream.peek strm__ with
                 [ Some $p$ $when:eo$ ->
                     do { Stream.junk strm__; $rewrite False sp_kont$ }
                 | _ -> $p_kont$ ]
               >>
          | _ ->
              <:expr< match Stream.peek strm__ with [ $list:pel$ ] >> ]
        in
        iter pel
    | <:expr< try Some $e$ with [ Stream.Failure -> $p_kont$ ] >> ->
        let f = parserify e in
        let e =
          <:expr<
            match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
            [ Some a -> Some a
            | _ -> $p_kont$ ]
          >>
        in
        rewrite top e
    | <:expr< try $e$ with [ Stream.Failure -> $p_kont$ ] >> ->
        let f = parserify e in
        let e =
          <:expr<
            match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
            [ Some a -> a
            | _ -> $rewrite top p_kont$ ]
          >>
        in
        rewrite top e
    | <:expr< $f$ strm__ >> ->
        if top then
          <:expr<
            match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
            [ Some a -> a
            | _  -> raise Stream.Failure ]
          >>
        else
          let v = free_var_in_expr 's' f in
          <:expr< let $lid:v$ = strm__ in $f$ $lid:v$ >>
    | e -> semantic e ]
;

value parser_of_expr =
  let rec parser_cases e =
    match e with
    [ <:expr<
        match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
        [ Some $p$ -> $sp_kont$
        | _ -> $p_kont$ ]
      >> ->
        let spc = (SPCnterm p f, None) in
        let (sp, epo, e) = kont sp_kont in
        [([spc :: sp], epo, e) :: parser_cases p_kont]
    | <:expr<
        match Stream.peek strm__ with
        [ Some $p$ $when:wo$ -> do { Stream.junk strm__; $sp_kont$ }
        | _ -> $p_kont$ ]
      >> ->
        let spc = (SPCterm (p, wo), None) in
        let (sp, epo, e) = kont sp_kont in
        [([spc :: sp], epo, e) :: parser_cases p_kont]
    | <:expr< let $p$ = strm__ in $sp_kont$ >> ->
        let spc = (SPCsterm p, None) in
        let (sp, epo, e) = kont sp_kont in
        [([spc :: sp], epo, e)]
    | <:expr< let $p$ = Stream.count strm__ in $e$ >> -> [([], Some p, e)]
    | <:expr< raise Stream.Failure >> -> []
    | _ -> [([], None, e)] ]
  and kont e =
    match e with
    [ <:expr<
        match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
        [ Some $p$ -> $sp_kont$
        | _ -> raise (Stream.Error $err$) ]
      >> ->
        let err =
          match err with
          [ <:expr< "" >> -> None
          | _ -> Some err ]
        in
        let spc = (SPCnterm p f, err) in
        let (sp, epo, e) = kont sp_kont in
        ([spc :: sp], epo, e)
    | <:expr<
        match Stream.peek strm__ with
        [ Some $p$ $when:wo$ -> do { Stream.junk strm__; $sp_kont$ }
        | _ -> raise (Stream.Error $err$) ]
      >> ->
        let err =
          match err with
          [ <:expr< "" >> -> None
          | _ -> Some err ]
        in
        let spc = (SPCterm (p, wo), err) in
        let (sp, epo, e) = kont sp_kont in
        ([spc :: sp], epo, e)
    | <:expr< let $p$ = strm__ in $sp_kont$ >> ->
        let spc = (SPCsterm p, None) in
        let (sp, epo, e) = kont sp_kont in
        ([spc :: sp], epo, e)
    | <:expr< let $p$ = Stream.count strm__ in $e$ >> -> ([], Some p, e)
    | _ -> ([], None, e) ]
  in
  parser_cases
;

value parser_cases b spel dg k =
  let rec parser_cases b spel dg k =
    match spel with
    [ [] -> [: `HVbox [: b; k :] :]
    | [(sp, epo, e)] -> [: `parser_case b sp epo e dg k :]
    | [(sp, epo, e) :: spel] ->
        [: `parser_case b sp epo e "|" [: :];
           parser_cases [: `S LR "|" :] spel dg k :] ]
  and parser_case b sp epo e dg k =
    let epo =
      match epo with
      [ Some p -> [: `patt p "" [: `S LR "->" :] :]
      | _ -> [: `S LR "->" :] ]
    in
    HVbox
      [: b;
         `HOVbox
            [: `HOVbox
                 [: `S LR "[<";
                    stream_patt [: :] sp [: `S LR ">]"; epo :] :];
               `expr e dg k :] :]
  and stream_patt b sp k =
    match sp with
    [ [] -> [: `HVbox [: b; k :] :]
    | [(spc, None)] -> [: `stream_patt_comp b spc "" k :]
    | [(spc, Some e)] ->
        [: `HVbox
              [: `stream_patt_comp b spc "" [: :];
                 `HVbox [: `S LR "??"; `expr e "" k :] :] :]
    | [(spc, None) :: spcl] ->
        [: `stream_patt_comp b spc ";" [: `S RO ";" :];
           stream_patt [: :] spcl k :]
    | [(spc, Some e) :: spcl] ->
        [: `HVbox
              [: `stream_patt_comp b spc "" [: :];
                 `HVbox [: `S LR "??"; `expr e ";" [: `S RO ";" :] :] :];
           stream_patt [: :] spcl k :] ]
  and stream_patt_comp b spc dg k =
    match spc with
    [ SPCterm (p, w) ->
        HVbox [: b; `S LO "'"; `spatt p "" (when_opt w k) :]
    | SPCnterm p e ->
        HVbox [: b; `HVbox [: `patt p "" [: `S LR "=" :]; `expr e dg k :] :]
    | SPCsterm p -> HVbox [: b; `patt p "" k :] ]
  and when_opt wo k =
    match wo with
    [ Some e -> [: `S LR "when"; `expr e "" k :]
    | _ -> k ]
  in
  parser_cases b spel dg k
;

value parser_body e dg k =
  let (bp, e) =
    match e with
    [ <:expr< let $bp$ = Stream.count strm__ in $e$ >> -> (Some bp, e)
    | e -> (None, e) ]
  in
  let e = rewrite_parser e in
  match parser_of_expr e with
  [ [] ->
      let spe = ([], None, <:expr< raise Stream.Failure >>) in
      HVbox
        [: `HVbox
              [: `S LR "parser";
                 match bp with
                 [ Some p -> [: `patt p "" [: :] :]
                 | _ -> [: :] ] :];
           parser_cases [: :] [spe] dg k :]
  | spel ->
      BEVbox
        [: `HVbox
              [: `S LR "parser";
                 match bp with
                 [ Some p -> [: `patt p "" [: :] :]
                 | _ -> [: :] ] :];
           parser_cases [: :] spel dg k :] ]
;

value pmatch e dg k =
  let (me, e) =
    match e with
    [ <:expr< let (strm__ : Stream.t _) = $me$ in $e$ >> -> (me, e)
    | _ -> failwith "Pr_op.pmatch" ]
  in
  let (bp, e) =
    match e with
    [ <:expr< let $bp$ = Stream.count strm__ in $e$ >> -> (Some bp, e)
    | e -> (None, e) ]
  in
  let e = rewrite_parser e in
  let spel = parser_of_expr e in
  Vbox
    [: `HVbox [: :];
       `HVbox
          [: `S LR "match"; `expr me "" [: `S LR "with" :]; `S LR "parser";
             match bp with
             [ Some p -> [: `patt p "" [: :] :]
             | _ -> [: :] ] :];
       `BEbox [: `HVbox [: :]; parser_cases [: :] spel dg k :] :]
;

(* Printer extensions *)

pr_expr_fun_args.val :=
  extfun pr_expr_fun_args.val with
  [ <:expr< fun strm__ -> $_$ >> as ge -> ([], ge)
  | <:expr< fun [(strm__ : $_$) -> $_$] >> as ge -> ([], ge) ];

let lev = find_pr_level "expr1" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< let (strm__ : Stream.t _) = $_$ in $_$ >> as e ->
      fun curr next dg k ->
        if not (List.mem dg ["|"; ";"]) then [: `pmatch e dg k :]
        else [: `S LO "("; `pmatch e "" [: `S RO ")"; k :] :]
  | <:expr< fun strm__ -> $x$ >> ->
      fun curr next dg k ->
        if not (List.mem dg ["|"; ";"]) then [: `parser_body x dg k :]
        else [: `S LO "("; `parser_body x "" [: `S RO ")"; k :] :]
  | <:expr< fun [ (strm__ : $_$) -> $x$ ] >> ->
      fun curr next dg k ->
        if not (List.mem dg ["|"; ";"]) then [: `parser_body x dg k :]
        else [: `S LO "("; `parser_body x "" [: `S RO ")"; k :] :] ];

let lev = find_pr_level "apply" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< Stream.iapp $_$ $_$ >> | <:expr< Stream.icons $_$ $_$ >> |
    <:expr< Stream.ising $_$ >> | <:expr< Stream.lapp (fun _ -> $_$) $_$ >> |
    <:expr< Stream.lcons (fun _ -> $_$) $_$ >> |
    <:expr< Stream.lsing (fun _ -> $_$) >> | <:expr< Stream.sempty >> |
    <:expr< Stream.slazy $_$ >> as e ->
      fun curr next dg k -> [: `next e "" k :] ];

let lev = find_pr_level "dot" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< Stream.sempty >> as e ->
      fun curr next dg k -> [: `next e "" k :] ];

let lev = find_pr_level "simple" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< Stream.iapp $_$ $_$ >> | <:expr< Stream.icons $_$ $_$ >> |
    <:expr< Stream.ising $_$ >> | <:expr< Stream.lapp (fun _ -> $_$) $_$ >> |
    <:expr< Stream.lcons (fun _ -> $_$) $_$ >> |
    <:expr< Stream.lsing (fun _ -> $_$) >> | <:expr< Stream.sempty >> |
    <:expr< Stream.slazy $_$ >> as e ->
      fun curr next dg k ->
        [: `stream e "" k :] ];
