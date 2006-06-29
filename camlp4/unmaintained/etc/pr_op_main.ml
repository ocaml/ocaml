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

value _loc = Loc.mk "FIXME pr_op_main.ml";

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

open Parserify;

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
    [ <:expr< let $bp$ = Stream.count __strm in $e$ >> -> (Some bp, e)
    | e -> (None, e) ]
  in
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
    [ <:expr< let (__strm : Stream.t _) = $me$ in $e$ >> -> (me, e)
    | _ -> failwith "Pr_op.pmatch" ]
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
       `BEbox [: `HVbox [: :]; parser_cases [: :] spel dg k :] :]
;

(* Printer extensions *)

pr_expr_fun_args.val :=
  extfun pr_expr_fun_args.val with
  [ <:expr< fun __strm -> $_$ >> as ge -> ([], ge)
  | <:expr< fun [(__strm : $_$) -> $_$] >> as ge -> ([], ge) ];

let lev = find_pr_level "expr1" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< let (__strm : Stream.t _) = $_$ in $_$ >> as e ->
      fun _ _ dg k ->
        if not (List.mem dg ["|"; ";"]) then [: `pmatch e dg k :]
        else [: `S LO "("; `pmatch e "" [: `S RO ")"; k :] :]
  | <:expr< fun __strm -> $x$ >> ->
      fun _ _ dg k ->
        if not (List.mem dg ["|"; ";"]) then [: `parser_body x dg k :]
        else [: `S LO "("; `parser_body x "" [: `S RO ")"; k :] :]
  | <:expr< fun [ (__strm : $_$) -> $x$ ] >> ->
      fun _ _ dg k ->
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
      fun _ _ _ k ->
        [: `stream e "" k :] ];
