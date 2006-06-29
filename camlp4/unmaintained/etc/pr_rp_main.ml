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

value _loc = Loc.mk "FIXME pr_rp_main.ml";

value expr e dg k = pr_expr.pr_fun "top" e dg k;
value patt e dg k = pr_patt.pr_fun "top" e dg k;

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
  let elem e k =
    match e with
    [ (True, e) -> [: `HOVbox [: `S LO "`"; `expr e "" k :] :]
    | (False, e) -> [: `expr e "" k :] ]
  in
  let rec glop e k =
    match e with
    [ [] -> k
    | [e] -> [: elem e k :]
    | [e :: el] -> [: elem e [: `S RO ";" :]; glop el k :] ]
  in
  HOVbox [: `S LR "[:"; glop (get e) [: `S LR ":]"; k :] :]
;

(* Parsers *)

open Parserify;

value parser_cases b spel k =
  let rec parser_cases b spel k =
    match spel with
    [ [] -> [: `HVbox [: b; k :] :]
    | [(sp, epo, e)] -> [: `parser_case b sp epo e k :]
    | [(sp, epo, e) :: spel] ->
        [: `parser_case b sp epo e [: :];
           parser_cases [: `S LR "|" :] spel k :] ]
  and parser_case b sp epo e k =
    let epo =
      match epo with
      [ Some p -> [: `patt p "" [: `S LR "->" :] :]
      | _ -> [: `S LR "->" :] ]
    in
    HVbox
      [: b;
         `HOVbox
            [: `HOVbox
                  [: `S LR "[:";
                     stream_patt [: :] sp [: `S LR ":]"; epo :] :];
               `expr e "" k :] :]
  and stream_patt b sp k =
    match sp with
    [ [] -> [: `HVbox [: b; k :] :]
    | [(spc, None)] -> [: `stream_patt_comp b spc k :]
    | [(spc, Some e)] ->
        [: `HVbox
              [: `stream_patt_comp b spc [: :];
                 `HVbox [: `S LR "?"; `expr e "" k :] :] :]
    | [(spc, None) :: spcl] ->
        [: `stream_patt_comp b spc [: `S RO ";" :];
           stream_patt [: :] spcl k :]
    | [(spc, Some e) :: spcl] ->
        [: `HVbox
              [: `stream_patt_comp b spc [: :];
                 `HVbox [: `S LR "?"; `expr e "" [: `S RO ";" :] :] :];
           stream_patt [: :] spcl k :] ]
  and stream_patt_comp b spc k =
    match spc with
    [ SPCterm (p, w) ->
        HVbox [: b; `S LO "`"; `patt p "" [: :]; `HVbox [: when_opt w k :] :]
    | SPCnterm p e ->
        HVbox [: b; `HVbox [: `patt p "" [: `S LR "=" :]; `expr e "" k :] :]
    | SPCsterm p -> HVbox [: b; `patt p "" k :] ]
  and when_opt wo k =
    match wo with
    [ Some e -> [: `S LR "when"; `expr e "" k :]
    | _ -> k ]
  in
  parser_cases b spel k
;

value parser_body e _ k =
  let (bp, e) =
    match e with
    [ <:expr< let $bp$ = Stream.count __strm in $e$ >> -> (Some bp, e)
    | e -> (None, e) ]
  in
  match parser_of_expr e with
  [ [] ->
      HVbox
        [: `HVbox
              [: `S LR "parser";
                 match bp with
                 [ Some p -> [: `patt p "" [: :] :]
                 | _ -> [: :] ] :];
           `HVbox [: `S LR "[]"; k :] :]
  | [spe] ->
      HVbox
        [: `HVbox
              [: `S LR "parser";
                 match bp with
                 [ Some p -> [: `patt p "" [: :] :]
                 | _ -> [: :] ] :];
           parser_cases [: :] [spe] k :]
  | spel ->
      Vbox
        [: `HVbox [: :];
           `HVbox
              [: `S LR "parser";
                 match bp with
                 [ Some p -> [: `patt p "" [: :] :]
                 | _ -> [: :] ] :];
           parser_cases [: `S LR "[" :] spel [: `S LR "]"; k :] :] ]
;

value pmatch e _ k =
  let (me, e) =
    match e with
    [ <:expr< let (__strm : Stream.t _) = $me$ in $e$ >> -> (me, e)
    | _ -> failwith "Pr_rp.pmatch" ]
  in
  let (bp, e) =
    match e with
    [ <:expr< let $bp$ = Stream.count __strm in $e$ >> -> (Some bp, e)
    | e -> (None, e) ]
  in
  let spel = parser_of_expr e in
  Vbox
    [: `HVbox [: :];
       `HVbox
          [: `S LR "match"; `expr me "" [: `S LR "with" :]; `S LR "parser";
             match bp with
             [ Some p -> [: `patt p "" [: :] :]
             | _ -> [: :] ] :];
       parser_cases [: `S LR "[" :] spel [: `S LR "]"; k :] :]
;

(* Printer extensions *)

pr_expr_fun_args.val :=
  extfun pr_expr_fun_args.val with
  [ <:expr< fun __strm -> $_$ >> as ge -> ([], ge)
  | <:expr< fun [(__strm : $_$) -> $_$] >> as ge -> ([], ge) ];

let lev = find_pr_level "top" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< let (__strm : Stream.t _) = $_$ in $_$ >> as e ->
      fun _ _ _ k -> [: `pmatch e "" k :]
  | <:expr< fun __strm -> $x$ >> ->
      fun _ _ _ k -> [: `parser_body x "" k :]
  | <:expr< fun (__strm : $_$) -> $x$ >> ->
      fun _ _ _ k -> [: `parser_body x "" k :] ];

let lev = find_pr_level "apply" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< Stream.iapp $_$ $_$ >> | <:expr< Stream.icons $_$ $_$ >> |
    <:expr< Stream.ising $_$ >> | <:expr< Stream.lapp (fun _ -> $_$) $_$ >> |
    <:expr< Stream.lcons (fun _ -> $_$) $_$ >> |
    <:expr< Stream.lsing (fun _ -> $_$) >> | <:expr< Stream.sempty >> |
    <:expr< Stream.slazy $_$ >> as e ->
      fun _ next _ k -> [: `next e "" k :] ];

let lev = find_pr_level "dot" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< Stream.sempty >> as e ->
      fun _ next _ k -> [: `next e "" k :] ];

let lev = find_pr_level "simple" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< Stream.iapp $_$ $_$ >> | <:expr< Stream.icons $_$ $_$ >> |
    <:expr< Stream.ising $_$ >> | <:expr< Stream.lapp (fun _ -> $_$) $_$ >> |
    <:expr< Stream.lcons (fun _ -> $_$) $_$ >> |
    <:expr< Stream.lsing (fun _ -> $_$) >> | <:expr< Stream.sempty >> |
    <:expr< Stream.slazy $_$ >> as e ->
      fun _ _ _ k -> [: `stream e "" k :] ];
