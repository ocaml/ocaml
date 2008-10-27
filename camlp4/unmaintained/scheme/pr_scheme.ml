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


open Pcaml;
open Format;

type printer_t 'a =
  { pr_fun : mutable string -> next 'a;
    pr_levels : mutable list (pr_level 'a) }
and pr_level 'a =
  { pr_label : string;
    pr_box : formatter -> (formatter -> unit) -> 'a -> unit;
    pr_rules : mutable pr_rule 'a }
and pr_rule 'a =
  Extfun.t 'a (formatter -> curr 'a -> next 'a -> string -> kont -> unit)
and curr 'a = formatter -> ('a * string * kont) -> unit
and next 'a = formatter -> ('a * string * kont) -> unit
and kont = formatter -> unit;

value not_impl name x ppf k =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  fprintf ppf "<pr_scheme: not impl: %s; %s>%t" name desc k
;

value pr_fun name pr lab =
  loop False pr.pr_levels where rec loop app =
    fun
    [ [] -> fun ppf (x, dg, k) -> failwith ("unable to print " ^ name)
    | [lev :: levl] ->
        if app || lev.pr_label = lab then
          let next = loop True levl in
          let rec curr ppf (x, dg, k) =
            Extfun.apply lev.pr_rules x ppf curr next dg k
          in
          fun ppf ((x, _, _) as n) -> lev.pr_box ppf (fun ppf -> curr ppf n) x
        else loop app levl ]
;

value rec find_pr_level lab =
  fun
  [ [] -> failwith ("level " ^ lab ^ " not found")
  | [lev :: levl] ->
      if lev.pr_label = lab then lev else find_pr_level lab levl ]
;

value pr_constr_decl = {pr_fun = fun []; pr_levels = []};
value constr_decl ppf (x, k) = pr_constr_decl.pr_fun "top" ppf (x, "", k);
pr_constr_decl.pr_fun := pr_fun "constr_decl" pr_constr_decl;

value pr_ctyp = {pr_fun = fun []; pr_levels = []};
pr_ctyp.pr_fun := pr_fun "ctyp" pr_ctyp;
value ctyp ppf (x, k) = pr_ctyp.pr_fun "top" ppf (x, "", k);

value pr_expr = {pr_fun = fun []; pr_levels = []};
pr_expr.pr_fun := pr_fun "expr" pr_expr;
value expr ppf (x, k) = pr_expr.pr_fun "top" ppf (x, "", k);

value pr_label_decl = {pr_fun = fun []; pr_levels = []};
value label_decl ppf (x, k) = pr_label_decl.pr_fun "top" ppf (x, "", k);
pr_label_decl.pr_fun := pr_fun "label_decl" pr_label_decl;

value pr_let_binding = {pr_fun = fun []; pr_levels = []};
pr_let_binding.pr_fun := pr_fun "let_binding" pr_let_binding;
value let_binding ppf (x, k) = pr_let_binding.pr_fun "top" ppf (x, "", k);

value pr_match_assoc = {pr_fun = fun []; pr_levels = []};
pr_match_assoc.pr_fun := pr_fun "match_assoc" pr_match_assoc;
value match_assoc ppf (x, k) = pr_match_assoc.pr_fun "top" ppf (x, "", k);

value pr_mod_ident = {pr_fun = fun []; pr_levels = []};
pr_mod_ident.pr_fun := pr_fun "mod_ident" pr_mod_ident;
value mod_ident ppf (x, k) = pr_mod_ident.pr_fun "top" ppf (x, "", k);

value pr_module_binding = {pr_fun = fun []; pr_levels = []};
pr_module_binding.pr_fun := pr_fun "module_binding" pr_module_binding;
value module_binding ppf (x, k) =
  pr_module_binding.pr_fun "top" ppf (x, "", k);

value pr_module_expr = {pr_fun = fun []; pr_levels = []};
pr_module_expr.pr_fun := pr_fun "module_expr" pr_module_expr;
value module_expr ppf (x, k) = pr_module_expr.pr_fun "top" ppf (x, "", k);

value pr_module_type = {pr_fun = fun []; pr_levels = []};
pr_module_type.pr_fun := pr_fun "module_type" pr_module_type;
value module_type ppf (x, k) = pr_module_type.pr_fun "top" ppf (x, "", k);

value pr_patt = {pr_fun = fun []; pr_levels = []};
pr_patt.pr_fun := pr_fun "patt" pr_patt;
value patt ppf (x, k) = pr_patt.pr_fun "top" ppf (x, "", k);

value pr_sig_item = {pr_fun = fun []; pr_levels = []};
pr_sig_item.pr_fun := pr_fun "sig_item" pr_sig_item;
value sig_item ppf (x, k) = pr_sig_item.pr_fun "top" ppf (x, "", k);

value pr_str_item = {pr_fun = fun []; pr_levels = []};
pr_str_item.pr_fun := pr_fun "str_item" pr_str_item;
value str_item ppf (x, k) = pr_str_item.pr_fun "top" ppf (x, "", k);

value pr_type_decl = {pr_fun = fun []; pr_levels = []};
value type_decl ppf (x, k) = pr_type_decl.pr_fun "top" ppf (x, "", k);
pr_type_decl.pr_fun := pr_fun "type_decl" pr_type_decl;

value pr_type_params = {pr_fun = fun []; pr_levels = []};
value type_params ppf (x, k) = pr_type_params.pr_fun "top" ppf (x, "", k);
pr_type_params.pr_fun := pr_fun "type_params" pr_type_params;

value pr_with_constr = {pr_fun = fun []; pr_levels = []};
value with_constr ppf (x, k) = pr_with_constr.pr_fun "top" ppf (x, "", k);
pr_with_constr.pr_fun := pr_fun "with_constr" pr_with_constr;

(* general functions *)

value nok ppf = ();
value ks s k ppf = fprintf ppf "%s%t" s k;

value rec list f ppf (l, k) =
  match l with
  [ [] -> k ppf
  | [x] -> f ppf (x, k)
  | [x :: l] -> fprintf ppf "%a@ %a" f (x, nok) (list f) (l, k) ]
;

value rec listwb b f ppf (l, k) =
  match l with
  [ [] -> k ppf
  | [x] -> f ppf ((b, x), k)
  | [x :: l] -> fprintf ppf "%a@ %a" f ((b, x), nok) (listwb "" f) (l, k) ]
;

(* specific functions *)

value rec is_irrefut_patt =
  fun
  [ <:patt< $lid:_$ >> -> True
  | <:patt< () >> -> True
  | <:patt< _ >> -> True
  | <:patt< ($x$ as $y$) >> -> is_irrefut_patt x && is_irrefut_patt y
  | <:patt< { $list:fpl$ } >> ->
      List.for_all (fun (_, p) -> is_irrefut_patt p) fpl
  | <:patt< ($p$ : $_$) >> -> is_irrefut_patt p
  | <:patt< ($list:pl$) >> -> List.for_all is_irrefut_patt pl
  | <:patt< ? $_$ : ( $p$ ) >> -> is_irrefut_patt p
  | <:patt< ? $_$ : ($p$ = $_$) >> -> is_irrefut_patt p
  | <:patt< ~ $_$ >> -> True
  | <:patt< ~ $_$ : $p$ >> -> is_irrefut_patt p
  | _ -> False ]
;

value expr_fun_args ge = Extfun.apply pr_expr_fun_args.val ge;

pr_expr_fun_args.val :=
  extfun Extfun.empty with
  [ <:expr< fun [$p$ -> $e$] >> as ge ->
      if is_irrefut_patt p then
        let (pl, e) = expr_fun_args e in
        ([p :: pl], e)
      else ([], ge)
  | ge -> ([], ge) ];

value sequence ppf (e, k) =
  match e with
  [ <:expr< do { $list:el$ } >> ->
      fprintf ppf "@[<hv>%a@]" (list expr) (el, k)
  | _ -> expr ppf (e, k) ]
;

value string ppf (s, k) = fprintf ppf "\"%s\"%t" s k;

value int_repr s =
  if String.length s > 2 && s.[0] = '0' then
    match s.[1] with
    [ 'b' | 'o' | 'x' | 'B' | 'O' | 'X' ->
        "#" ^ String.sub s 1 (String.length s - 1)
    | _ -> s ]
  else s  
;

value assoc_left_parsed_op_list = ["+"; "*"; "land"; "lor"; "lxor"];
value assoc_right_parsed_op_list = ["and"; "or"; "^"; "@"];
value and_by_couple_op_list = ["="; "<>"; "<"; ">"; "<="; ">="; "=="; "!="];

(* extensible pretty print functions *)

pr_constr_decl.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ (loc, c, []) ->
          fun ppf curr next dg k -> fprintf ppf "(@[<hv>%s%t@]" c (ks ")" k)
      | (loc, c, tl) ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[<hv>%s@ %a@]" c (list ctyp) (tl, ks ")" k) ]}];

pr_ctyp.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ <:ctyp< [ $list:cdl$ ] >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[<hv>sum@ %a@]" (list constr_decl) (cdl, ks ")" k)
      | <:ctyp< { $list:cdl$ } >> ->
          fun ppf curr next dg k ->
            fprintf ppf "{@[<hv>%a@]" (list label_decl) (cdl, ks "}" k)
      | <:ctyp< ( $list:tl$ ) >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[* @[<hv>%a@]@]" (list ctyp) (tl, ks ")" k)
      | <:ctyp< $t1$ -> $t2$ >> ->
          fun ppf curr next dg k ->
            let tl =
              loop t2 where rec loop =
                fun
                [ <:ctyp< $t1$ -> $t2$ >> -> [t1 :: loop t2]
                | t -> [t] ]
            in
            fprintf ppf "(@[-> @[<hv>%a@]@]" (list ctyp)
              ([t1 :: tl], ks ")" k)
      | <:ctyp< $t1$ $t2$ >> ->
          fun ppf curr next dg k ->
            let (t, tl) =
              loop [t2] t1 where rec loop tl =
                fun
                [ <:ctyp< $t1$ $t2$ >> -> loop [t2 :: tl] t1
                | t1 -> (t1, tl) ]
            in
            fprintf ppf "(@[%a@ %a@]" ctyp (t, nok) (list ctyp) (tl, ks ")" k)
      | <:ctyp< $t1$ . $t2$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "%a.%a" ctyp (t1, nok) ctyp (t2, k)
      | <:ctyp< $lid:s$ >> | <:ctyp< $uid:s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "%s%t" s k
      | <:ctyp< ' $s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "'%s%t" s k
      | <:ctyp< _ >> ->
          fun ppf curr next dg k -> fprintf ppf "_%t" k
      | x ->
          fun ppf curr next dg k -> not_impl "ctyp" x ppf k ]}];

pr_expr.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< fun [] >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(lambda%t" (ks ")" k)
      | <:expr< fun $lid:s$ -> $e$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(lambda@ %s@;<1 1>%a" s expr (e, ks ")" k)
      | <:expr< fun [ $list:pwel$ ] >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[<hv>lambda_match@ %a@]" (list match_assoc)
              (pwel, ks ")" k)
      | <:expr< match $e$ with [ $list:pwel$ ] >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[<hv>@[<b 2>match@ %a@]@ %a@]" expr (e, nok)
              (list match_assoc) (pwel, ks ")" k)
      | <:expr< try $e$ with [ $list:pwel$ ] >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[<hv>@[<b 2>try@ %a@]@ %a@]" expr (e, nok)
              (list match_assoc) (pwel, ks ")" k)
      | <:expr< let $p1$ = $e1$ in $e2$ >> ->
          fun ppf curr next dg k ->
            let (pel, e) =
              loop [(p1, e1)] e2 where rec loop pel =
                fun
                [ <:expr< let $p1$ = $e1$ in $e2$ >> ->
                    loop [(p1, e1) :: pel] e2
                | e -> (List.rev pel, e) ]
            in
            let b =
              match pel with
              [ [_] -> "let"
              | _ -> "let*" ]
            in
            fprintf ppf "(@[@[%s (@[<v>%a@]@]@;<1 2>%a@]" b
              (listwb "" let_binding) (pel, ks ")" nok)
                 sequence (e, ks ")" k)
      | <:expr< let $opt:rf$ $list:pel$ in $e$ >> ->
          fun ppf curr next dg k ->
            let b = if rf then "letrec" else "let" in
            fprintf ppf "(@[<hv>%s@ (@[<hv>%a@]@ %a@]" b
              (listwb "" let_binding) (pel, ks ")" nok) expr (e, ks ")" k)
      | <:expr< if $e1$ then $e2$ else () >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(if @[%a@;<1 0>%a@]" expr (e1, nok)
              expr (e2, ks ")" k)
      | <:expr< if $e1$ then $e2$ else $e3$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(if @[%a@ %a@ %a@]" expr (e1, nok)
              expr (e2, nok) expr (e3, ks ")" k)
      | <:expr< do { $list:el$ } >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(begin@;<1 1>@[<hv>%a@]" (list expr) (el, ks ")" k)
      | <:expr< for $i$ = $e1$ to $e2$ do { $list:el$ } >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[for %s@ %a@ %a %a@]" i expr (e1, nok)
              expr (e2, nok) (list expr) (el, ks ")" k)
      | <:expr< ($e$ : $t$) >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(:@ %a@ %a" expr (e, nok) ctyp (t, ks ")" k)
      | <:expr< ($list:el$) >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(values @[%a@]" (list expr) (el, ks ")" k)
      | <:expr< { $list:fel$ } >> ->
          fun ppf curr next dg k ->
            let record_binding ppf ((p, e), k) =
              fprintf ppf "(@[%a@ %a@]" patt (p, nok) expr (e, ks ")" k)
            in
            fprintf ppf "{@[<hv>%a@]" (list record_binding) (fel, ks "}" k)
      | <:expr< { ($e$) with $list:fel$ } >> ->
          fun ppf curr next dg k ->
            let record_binding ppf ((p, e), k) =
              fprintf ppf "(@[%a@ %a@]" patt (p, nok) expr (e, ks ")" k)
            in
            fprintf ppf "{@[@[with@ %a@]@ @[%a@]@]" expr (e, nok)
              (list record_binding) (fel, ks "}" k)
      | <:expr< $e1$ := $e2$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(:=@;<1 1>%a@;<1 1>%a" expr (e1, nok)
              expr (e2, ks ")" k)
      | <:expr< [$_$ :: $_$] >> as e ->
          fun ppf curr next dg k ->
            let (el, c) =
              make_list e where rec make_list e =
                match e with
                [ <:expr< [$e$ :: $y$] >> ->
                    let (el, c) = make_list y in
                    ([e :: el], c)
                | <:expr< [] >> -> ([], None)
                | x -> ([], Some e) ]
            in
            match c with
            [ None ->
                fprintf ppf "[%a" (list expr) (el, ks "]" k)
            | Some x ->
                fprintf ppf "[%a@ %a" (list expr) (el, ks " ." nok)
                  expr (x, ks "]" k) ]
      | <:expr< lazy ($x$) >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[lazy@ %a@]" expr (x, ks ")" k)
      | <:expr< $lid:s$ $e1$ $e2$ >>
        when List.mem s assoc_right_parsed_op_list ->
          fun ppf curr next dg k ->
            let el =
              loop [e1] e2 where rec loop el =
                fun
                [ <:expr< $lid:s1$ $e1$ $e2$ >> when s1 = s ->
                    loop [e1 :: el] e2
                | e -> List.rev [e :: el] ]
            in
            fprintf ppf "(@[%s %a@]" s (list expr) (el, ks ")" k)
      | <:expr< $e1$ $e2$ >> ->
          fun ppf curr next dg k ->
            let (f, el) =
              loop [e2] e1 where rec loop el =
                fun
                [ <:expr< $e1$ $e2$ >> -> loop [e2 :: el] e1
                | e1 -> (e1, el) ]
            in
            fprintf ppf "(@[%a@ %a@]" expr (f, nok) (list expr) (el, ks ")" k)
      | <:expr< ~ $s$ : ($e$) >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(~%s@ %a" s expr (e, ks ")" k)
      | <:expr< $e1$ .[ $e2$ ] >> ->
          fun ppf curr next dg k ->
            fprintf ppf "%a.[%a" expr (e1, nok) expr (e2, ks "]" k)
      | <:expr< $e1$ .( $e2$ ) >> ->
          fun ppf curr next dg k ->
            fprintf ppf "%a.(%a" expr (e1, nok) expr (e2, ks ")" k)
      | <:expr< $e1$ . $e2$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "%a.%a" expr (e1, nok) expr (e2, k)
      | <:expr< $int:s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "%s%t" (int_repr s) k
      | <:expr< $lid:s$ >> | <:expr< $uid:s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "%s%t" s k
      | <:expr< ` $s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "`%s%t" s k
      | <:expr< $str:s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "\"%s\"%t" s k
      | <:expr< $chr:s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "'%s'%t" s k
      | x ->
          fun ppf curr next dg k -> not_impl "expr" x ppf k ]}];

pr_label_decl.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ (loc, f, m, t) ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[<hv>%s%t@ %a@]" f
              (fun ppf -> if m then fprintf ppf "@ mutable" else ())
              ctyp (t, ks ")" k) ]}];

pr_let_binding.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ (b, (p, e)) ->
          fun ppf curr next dg k ->
            let (pl, e) = expr_fun_args e in
            match pl with
            [ [] ->
                fprintf ppf "(@[<b 1>%s%s%a@ %a@]" b
                  (if b = "" then "" else " ") patt (p, nok)
                  sequence (e, ks ")" k)
            | _ ->
                fprintf ppf "(@[<b 1>%s%s(%a)@ %a@]" b
                  (if b = "" then "" else " ") (list patt) ([p :: pl], nok)
                  sequence (e, ks ")" k) ] ]}];

pr_match_assoc.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ (p, we, e) ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[%t@ %a@]"
              (fun ppf ->
                 match we with
                 [ Some e ->
                     fprintf ppf "(when@ %a@ %a" patt (p, nok)
                       expr (e, ks ")" nok)
                 | None -> patt ppf (p, nok) ])
              sequence (e, ks ")" k) ]}];

pr_mod_ident.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ [s] ->
          fun ppf curr next dg k ->
            fprintf ppf "%s%t" s k
      | [s :: sl] ->
          fun ppf curr next dg k ->
            fprintf ppf "%s.%a" s curr (sl, "", k)
      | x ->
          fun ppf curr next dg k -> not_impl "mod_ident" x ppf k ]}];

pr_module_binding.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ (b, s, me) ->
          fun ppf curr next dg k ->
            fprintf ppf "%s@ %s@ %a" b s module_expr (me, k) ]}];

pr_module_expr.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ <:module_expr< functor ($i$ : $mt$) -> $me$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[@[@[functor@ %s@]@ %a@]@ %a@]"
              i module_type (mt, nok) module_expr (me, ks ")" k)
      | <:module_expr< struct $list:sil$ end >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[struct@ @[<hv>%a@]@]" (list str_item)
              (sil, ks ")" k)
      | <:module_expr< $me1$ $me2$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[%a@ %a@]" module_expr (me1, nok)
              module_expr (me2, ks ")" k)
      | <:module_expr< $uid:s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "%s%t" s k
      | x ->
          fun ppf curr next dg k -> not_impl "module_expr" x ppf k ]}];

pr_module_type.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ <:module_type< functor ($i$ : $mt1$) -> $mt2$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[@[@[functor@ %s@]@ %a@]@ %a@]"
              i module_type (mt1, nok) module_type (mt2, ks ")" k)
      | <:module_type< sig $list:sil$ end >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[sig@ @[<hv>%a@]@]" (list sig_item) (sil, ks ")" k)
      | <:module_type< $mt$ with $list:wcl$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[with@;<1 2>@[%a@ (%a@]@]" module_type (mt, nok)
              (list with_constr) (wcl, ks "))" k)
      | <:module_type< $uid:s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "%s%t" s k
      | x ->
          fun ppf curr next dg k -> not_impl "module_type" x ppf k ]}];

pr_patt.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< $p1$ | $p2$ >> ->
          fun ppf curr next dg k ->
            let (f, pl) =
              loop [p2] p1 where rec loop pl =
                fun
                [ <:patt< $p1$ | $p2$ >> -> loop [p2 :: pl] p1
                | p1 -> (p1, pl) ]
            in
            fprintf ppf "(@[or@ %a@ %a@]" patt (f, nok) (list patt)
              (pl, ks ")" k)
      | <:patt< ($p1$ as $p2$) >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[as@ %a@ %a@]" patt (p1, nok) patt (p2, ks ")" k)
      | <:patt< $p1$ .. $p2$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[range@ %a@ %a@]" patt (p1, nok) patt (p2, ks ")" k)
      | <:patt< [$_$ :: $_$] >> as p ->
          fun ppf curr next dg k ->
            let (pl, c) =
              make_list p where rec make_list p =
                match p with
                [ <:patt< [$p$ :: $y$] >> ->
                    let (pl, c) = make_list y in
                    ([p :: pl], c)
                | <:patt< [] >> -> ([], None)
                | x -> ([], Some p) ]
            in
            match c with
            [ None ->
                fprintf ppf "[%a" (list patt) (pl, ks "]" k)
            | Some x ->
                fprintf ppf "[%a@ %a" (list patt) (pl, ks " ." nok)
                  patt (x, ks "]" k) ]
      | <:patt< $p1$ $p2$ >> ->
          fun ppf curr next dg k ->
            let pl =
              loop [p2] p1 where rec loop pl =
                fun
                [ <:patt< $p1$ $p2$ >> -> loop [p2 :: pl] p1
                | p1 -> [p1 :: pl] ]
            in
            fprintf ppf "(@[%a@]" (list patt) (pl, ks ")" k)
      | <:patt< ($p$ : $t$) >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(:@ %a@ %a" patt (p, nok) ctyp (t, ks ")" k)
      | <:patt< ($list:pl$) >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(values @[%a@]" (list patt) (pl, ks ")" k)
      | <:patt< { $list:fpl$ } >> ->
          fun ppf curr next dg k ->
            let record_binding ppf ((p1, p2), k) =
              fprintf ppf "(@[%a@ %a@]" patt (p1, nok) patt (p2, ks ")" k)
            in
            fprintf ppf "(@[<hv>{}@ %a@]" (list record_binding) (fpl, ks ")" k)
      | <:patt< ? $x$ >> ->
          fun ppf curr next dg k -> fprintf ppf "?%s%t" x k
      | <:patt< ? ($lid:x$ = $e$) >> ->
          fun ppf curr next dg k -> fprintf ppf "(?%s@ %a" x expr (e, ks ")" k)
      | <:patt< $p1$ . $p2$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "%a.%a" patt (p1, nok) patt (p2, k)
      | <:patt< $lid:s$ >> | <:patt< $uid:s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "%s%t" s k
      | <:patt< $str:s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "\"%s\"%t" s k
      | <:patt< $chr:s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "'%s'%t" s k
      | <:patt< $int:s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "%s%t" (int_repr s) k
      | <:patt< $flo:s$ >> ->
          fun ppf curr next dg k -> fprintf ppf "%s%t" s k
      | <:patt< _ >> ->
          fun ppf curr next dg k -> fprintf ppf "_%t" k
      | x ->
          fun ppf curr next dg k -> not_impl "patt" x ppf k ]}];

pr_sig_item.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ <:sig_item< type $list:tdl$ >> ->
          fun ppf curr next dg k ->
            match tdl with
            [ [td] -> fprintf ppf "(%a" type_decl (("type", td), ks ")" k)
            | tdl ->
                fprintf ppf "(@[<hv>type@ %a@]" (listwb "" type_decl)
                  (tdl, ks ")" k) ]
      | <:sig_item< exception $c$ of $list:tl$ >> ->
          fun ppf curr next dg k ->
            match tl with
            [ [] -> fprintf ppf "(@[exception@ %s%t@]" c (ks ")" k)
            | tl ->
                fprintf ppf "(@[@[exception@ %s@]@ %a@]" c
                  (list ctyp) (tl, ks ")" k) ]
      | <:sig_item< value $i$ : $t$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[@[value %s@]@ %a@]" i ctyp (t, ks ")" k)
      | <:sig_item< external $i$ : $t$ = $list:pd$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[@[external@ %s@]@ %a@ %a@]" i ctyp (t, nok)
              (list string) (pd, ks ")" k)
      | <:sig_item< module $s$ : $mt$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[@[module@ %s@]@ %a@]" s
              module_type (mt, ks ")" k)
      | <:sig_item< module type $s$ = $mt$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[@[moduletype@ %s@]@ %a@]" s
              module_type (mt, ks ")" k)
      | <:sig_item< declare $list:s$ end >> ->
          fun ppf curr next dg k ->
            if s = [] then fprintf ppf "; ..."
            else fprintf ppf "%a" (list sig_item) (s, k)
      | MLast.SgUse _ _ _ ->
          fun ppf curr next dg k -> ()
      | x ->
          fun ppf curr next dg k -> not_impl "sig_item" x ppf k ]}];

pr_str_item.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ <:str_item< open $i$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(open@ %a" mod_ident (i, ks ")" k)
      | <:str_item< type $list:tdl$ >> ->
          fun ppf curr next dg k ->
            match tdl with
            [ [td] -> fprintf ppf "(%a" type_decl (("type", td), ks ")" k)
            | tdl ->
                fprintf ppf "(@[<hv>type@ %a@]" (listwb "" type_decl)
                  (tdl, ks ")" k) ]
      | <:str_item< exception $c$ of $list:tl$ >> ->
          fun ppf curr next dg k ->
            match tl with
            [ [] -> fprintf ppf "(@[exception@ %s%t@]" c (ks ")" k)
            | tl ->
                fprintf ppf "(@[@[exception@ %s@]@ %a@]" c
                  (list ctyp) (tl, ks ")" k) ]
      | <:str_item< value $opt:rf$ $list:pel$ >> ->
          fun ppf curr next dg k ->
            let b = if rf then "definerec" else "define" in
            match pel with
            [ [(p, e)] ->
                fprintf ppf "%a" let_binding ((b, (p, e)), k)
            | pel ->
                fprintf ppf "(@[<hv 1>%s*@ %a@]" b (listwb "" let_binding)
                  (pel, ks ")" k) ]
      | <:str_item< module $s$ = $me$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(%a" module_binding (("module", s, me), ks ")" k)
      | <:str_item< module type $s$ = $mt$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[@[moduletype@ %s@]@ %a@]" s
              module_type (mt, ks ")" k)
      | <:str_item< external $i$ : $t$ = $list:pd$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "(@[external@ %s@ %a@ %a@]" i ctyp (t, nok)
              (list string) (pd, ks ")" k)
      | <:str_item< $exp:e$ >> ->
          fun ppf curr next dg k ->
            fprintf ppf "%a" expr (e, k)
      | <:str_item< # $s$ $opt:x$ >> ->
          fun ppf curr next dg k ->
            match x with
            [ Some e -> fprintf ppf "; # (%s %a" s expr (e, ks ")" k)
            | None -> fprintf ppf "; # (%s%t" s (ks ")" k) ]
      | <:str_item< declare $list:s$ end >> ->
          fun ppf curr next dg k ->
            if s = [] then fprintf ppf "; ..."
            else fprintf ppf "%a" (list str_item) (s, k)
      | MLast.StUse _ _ _ ->
          fun ppf curr next dg k -> ()
      | x ->
          fun ppf curr next dg k -> not_impl "str_item" x ppf k ]}];

pr_type_decl.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ (b, ((_, tn), tp, te, cl)) ->
          fun ppf curr next dg k ->
            fprintf ppf "%t%t@;<1 1>%a"
              (fun ppf ->
                 if b <> "" then fprintf ppf "%s@ " b
                 else ())
              (fun ppf ->
                 match tp with
                 [ [] -> fprintf ppf "%s" tn
                 | tp -> fprintf ppf "(%s%a)" tn type_params (tp, nok) ])
               ctyp (te, k) ]}];

pr_type_params.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ [(s, vari) :: tpl] ->
          fun ppf curr next dg k ->
            fprintf ppf "@ '%s%a" s type_params (tpl, k)
      | [] ->
          fun ppf curr next dg k -> () ]}];

pr_with_constr.pr_levels :=
  [{pr_label = "top";
    pr_box ppf f x = fprintf ppf "@[%t@]" f;
    pr_rules =
      extfun Extfun.empty with
      [ MLast.WcTyp _ m tp te ->
          fun ppf curr next dg k ->
            fprintf ppf "(type@ %t@;<1 1>%a"
              (fun ppf ->
                 match tp with
                 [ [] -> fprintf ppf "%a" mod_ident (m, nok)
                 | tp ->
                     fprintf ppf "(%a@ %a)" mod_ident (m, nok)
                       type_params (tp, nok) ])
               ctyp (te, ks ")" k)
      | x ->
          fun ppf curr next dg k -> not_impl "with_constr" x ppf k ]}];

(* main *)

value output_string_eval ppf s =
  loop 0 where rec loop i =
    if i == String.length s then ()
    else if i == String.length s - 1 then pp_print_char ppf s.[i]
    else
      match (s.[i], s.[i + 1]) with
      [ ('\\', 'n') -> do { pp_print_char ppf '\n'; loop (i + 2) }
      | (c, _) -> do { pp_print_char ppf c; loop (i + 1) } ]
;

value sep = Pcaml.inter_phrases;

value input_source ic len =
  let buff = Buffer.create 20 in
  try
    let rec loop i =
      if i >= len then Buffer.contents buff
      else do { let c = input_char ic in Buffer.add_char buff c; loop (i + 1) }
    in
    loop 0
  with
  [ End_of_file ->
      let s = Buffer.contents buff in
      if s = "" then
        match sep.val with
        [ Some s -> s
        | None -> "\n" ]
      else s ]
;

value copy_source ppf (ic, first, bp, ep) =
  match sep.val with
  [ Some str ->
      if first then ()
      else if ep == in_channel_length ic then pp_print_string ppf "\n"
      else output_string_eval ppf str
  | None ->
      do {
        seek_in ic bp;
        let s = input_source ic (ep - bp) in pp_print_string ppf s
      } ]
;

value copy_to_end ppf (ic, first, bp) =
  let ilen = in_channel_length ic in
  if bp < ilen then copy_source ppf (ic, first, bp, ilen)
  else pp_print_string ppf "\n"
;

value apply_printer printer ast =
  let ppf = std_formatter in
  if Pcaml.input_file.val <> "-" && Pcaml.input_file.val <> "" then do {
    let ic = open_in_bin Pcaml.input_file.val in
    try
      let (first, last_pos) =
        List.fold_left
          (fun (first, last_pos) (si, (bp, ep)) ->
             do {
               fprintf ppf "@[%a@]@?" copy_source (ic, first, last_pos.Lexing.pos_cnum, bp.Lexing.pos_cnum);
               fprintf ppf "@[%a@]@?" printer (si, nok);
               (False, ep)
             })
          (True, Token.nowhere) ast
      in
      fprintf ppf "@[%a@]@?" copy_to_end (ic, first, last_pos.Lexing.pos_cnum)
    with x ->
      do { fprintf ppf "@."; close_in ic; raise x };
    close_in ic;
  }
  else failwith "not implemented"
;

Pcaml.print_interf.val := apply_printer sig_item;
Pcaml.print_implem.val := apply_printer str_item;

Pcaml.add_option "-l" (Arg.Int (fun x -> set_margin x))
  "<length> Maximum line length for pretty printing.";

Pcaml.add_option "-sep" (Arg.String (fun x -> sep.val := Some x))
  "<string> Use this string between phrases instead of reading source.";
