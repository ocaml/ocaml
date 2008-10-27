(* pa_r.cmo q_MLast.cmo pa_extfun.cmo pr_dump.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                               Camlp4                                *)
(*                                                                     *)
(*    Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file                 *)
(*   ../../../LICENSE.                                                 *)
(*                                                                     *)
(***********************************************************************)


open Format;
open Pcaml;
open Parserify;

value nok = Pr_scheme.nok;
value ks = Pr_scheme.ks;
value patt = Pr_scheme.patt;
value expr = Pr_scheme.expr;
value find_pr_level = Pr_scheme.find_pr_level;
value pr_expr = Pr_scheme.pr_expr;
type printer_t 'a = Pr_scheme.printer_t 'a ==
  { pr_fun : mutable string -> Pr_scheme.next 'a;
    pr_levels : mutable list (pr_level 'a) }
and pr_level 'a = Pr_scheme.pr_level 'a ==
  { pr_label : string;
    pr_box : formatter -> (formatter -> unit) -> 'a -> unit;
    pr_rules : mutable Pr_scheme.pr_rule 'a }
;

(* extensions for rebuilding syntax of parsers *)

value parser_cases ppf (spel, k) =
  let rec parser_cases ppf (spel, k) =
    match spel with
    [ [] -> fprintf ppf "[: `HVbox [: b; k :] :]"
    | [(sp, epo, e)] -> parser_case ppf (sp, epo, e, k)
    | [(sp, epo, e) :: spel] ->
        fprintf ppf "%a@ %a" parser_case (sp, epo, e, nok)
          parser_cases (spel, k) ]
  and parser_case ppf (sp, epo, e, k) =
    fprintf ppf "(@[@[(%a)%t@]@ %a@]" stream_patt (sp, nok)
      (fun ppf ->
         match epo with
         [ Some p -> fprintf ppf "@ %a" patt (p, nok)
         | None -> () ])
      expr (e, ks ")" k)
  and stream_patt ppf (sp, k) =
    match sp with
    [ [] -> k ppf
    | [(spc, None)] -> fprintf ppf "%a" stream_patt_comp (spc, k)
    | [(spc, Some e)] ->
        fprintf ppf "(@[? %a@ %a@]" stream_patt_comp (spc, nok)
          expr (e, ks ")" k)
    | [(spc, None) :: spcl] ->
        fprintf ppf "%a@ %a" stream_patt_comp (spc, nok) stream_patt (spcl, k)
    | [(spc, Some e) :: spcl] ->
        fprintf ppf "(@[? %a@ %a@]@ %a" stream_patt_comp (spc, nok)
          expr (e, ks ")" nok) stream_patt (spcl, k) ]
  and stream_patt_comp ppf (spc, k) =
    match spc with
    [ SPCterm (p, w) ->
        match w with
        [ Some e ->
            fprintf ppf "(` %a@ %a" patt (p, nok) expr (e, ks ")" k)
        | None -> fprintf ppf "(` %a" patt (p, ks ")" k) ]
    | SPCnterm p e ->
        fprintf ppf "(@[%a %a@]" patt (p, nok) expr (e, ks ")" k)
    | SPCsterm p -> fprintf ppf "%a" patt (p, k)  ]
  in
  parser_cases ppf (spel, k)
;

value parser_body ppf (e, k) =
  let (bp, e) =
    match e with
    [ <:expr< let $bp$ = Stream.count __strm in $e$ >> -> (Some bp, e)
    | e -> (None, e) ]
  in
  match parser_of_expr e with
  [ [] ->
      fprintf ppf "(parser%t%t"
        (fun ppf ->
           match bp with
           [ Some p -> fprintf ppf "@ %a" patt (p, nok)
           | _ -> ()])
        (ks ")" k)
  | spel ->
      fprintf ppf "(@[<v>@[parser%t@]@ @[<v 0>%a@]@]"
        (fun ppf ->
           match bp with
           [ Some p -> fprintf ppf "@ %a" patt (p, nok)
           | _ -> ()])
        parser_cases (spel, ks ")" k) ]
;

value pmatch ppf (e, k) =
  let (me, e) =
    match e with
    [ <:expr< let (__strm : Stream.t _) = $me$ in $e$ >> -> (me, e)
    | _ -> failwith "Pr_schp_main.pmatch" ]
  in
  let (bp, e) =
    match e with
    [ <:expr< let $bp$ = Stream.count __strm in $e$ >> -> (Some bp, e)
    | e -> (None, e) ]
  in
  let spel = parser_of_expr e in
  fprintf ppf "(@[@[match_with_parser@ %a@]%t@ @[<v 0>%a@]@]" expr (me, nok)
    (fun ppf ->
       match bp with
       [ Some p -> fprintf ppf "@ %a" patt (p, nok)
       | _ -> () ])
    parser_cases (spel, ks ")" k)
;

pr_expr_fun_args.val :=
  extfun pr_expr_fun_args.val with
  [ <:expr< fun [(__strm : $_$) -> $_$] >> as ge -> ([], ge) ];

let lev = find_pr_level "top" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< fun (__strm : $_$) -> $x$ >> ->
      fun ppf curr next dg k -> fprintf ppf "%a" parser_body (x, k)
  | <:expr< let (__strm : Stream.t _) = $_$ in $_$ >> as e ->
      fun ppf curr next dg k -> fprintf ppf "%a" pmatch (e, k) ];
