(* camlp4r q_MLast.cmo ./pa_extfun.cmo *)
(* $Id$ *)

open Pcaml;
open Spretty;

value _loc = Loc.mk "FIXME pr_extfun.ml";

value expr e dg k = pr_expr.pr_fun "top" e dg k;
value patt e dg k = pr_patt.pr_fun "top" e dg k;

value rec un_extfun rpel =
  fun
  [ <:expr< [ ($_$, $_$, fun [ $list:pel$ ]) :: $el$ ] >> ->
      let (p, wo, e) =
        match pel with
        [ [(p, wo, <:expr< Some $e$ >>);
           (<:patt< _ >>, None, <:expr< None >>)] ->
            (p, wo, e)
        | [(p, wo, <:expr< Some $e$ >>)] -> (p, wo, e)
        | _ -> raise Not_found ]
      in
      let rpel =
        match rpel with
        [ [(p1, wo1, e1) :: pel] ->
            if wo1 = wo && e1 = e then
              let p =
                match (p1, p) with
                [ (<:patt< ($x1$ as $x2$) >>, <:patt< ($y1$ as $y2$) >>) ->
                    if x2 = y2 then <:patt< ($x1$ | $y1$ as $x2$) >>
                    else <:patt< $p1$ | $p$ >>
                | _ -> <:patt< $p1$ | $p$ >> ]
              in
              [(p, wo, e) :: pel]
            else [(p, wo, e) :: rpel]
        | [] -> [(p, wo, e)] ]
      in
      un_extfun rpel el
  | <:expr< [] >> -> List.rev rpel
  | _ -> raise Not_found ]
;

value rec listwbws elem b sep el k =
  match el with
  [ [] -> [: b; k :]
  | [x] -> [: `elem b x k :]
  | [x :: l] -> [: `elem b x [: :]; listwbws elem [: `sep :] sep l k :] ]
;

value rec match_assoc_list pwel k =
  match pwel with
  [ [pwe] -> match_assoc [: `S LR "[" :] pwe [: `S LR "]"; k :]
  | pel ->
      Vbox
        [: `HVbox [: :];
           listwbws match_assoc [: `S LR "[" :] (S LR "|") pel
             [: `S LR "]"; k :] :] ]
and match_assoc b (p, w, e) k =
  let s =
    let (p, k) =
      match p with
      [ <:patt< ($p$ as $p2$) >> -> (p, [: `S LR "as"; `patt p2 "" [: :] :])
      | _ -> (p, [: :]) ]
    in
    match w with
    [ Some e1 ->
        [: `HVbox
              [: `HVbox [: :]; `patt p "" k;
                 `HVbox [: `S LR "when"; `expr e1 "" [: `S LR "->" :] :] :] :]
    | _ -> [: `patt p "" [: k; `S LR "->" :] :] ]
  in
  HVbox [: b; `HVbox [: `HVbox s; `expr e "" k :] :]
;

let lev = find_pr_level "top" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< Extfun.extend $e$ $list$ >> as ge ->
      fun _ next dg k ->
        try
          let pel = un_extfun [] list in
          [: `HVbox [: :];
             `BEbox [: `S LR "extfun"; `expr e "" [: :]; `S LR "with" :];
             `match_assoc_list pel k :]
        with
        [ Not_found -> [: `next ge dg k :] ] ];

let lev = find_pr_level "apply" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< Extfun.extend $_$ $_$ >> as ge ->
      fun _ next dg k -> [: `next ge dg k :] ];
