(* camlp4r q_MLast.cmo ./pa_extfun.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Pcaml;
open Spretty;

value no_slist = ref False;

value expr e dg k = pr_expr.pr_fun "top" e dg k;
value patt e dg k = pr_patt.pr_fun "top" e dg k;

(* Utilities *)

value rec list elem el k =
  match el with
  [ [] -> k
  | [x] -> [: `elem x k :]
  | [x :: l] -> [: `elem x [: :]; list elem l k :] ]
;

value rec listws elem sep el k =
  match el with
  [ [] -> k
  | [x] -> [: `elem x k :]
  | [x :: l] -> [: `elem x [: `sep :]; listws elem sep l k :] ]
;

value rec listwbws elem b sep el dg k =
  match el with
  [ [] -> [: b; k :]
  | [x] -> [: `elem b x dg k :]
  | [x :: l] ->
      let sdg =
        match sep with
        [ S _ x -> x
        | _ -> "" ]
      in
      [: `elem b x sdg [: :]; listwbws elem [: `sep :] sep l dg k :] ]
;

(* Extracting *)

value rec get_globals =
  fun
  [ [(<:patt< _ >>, <:expr< ($e$ : $uid:gmod1$.Entry.e '$_$) >>) :: pel] ->
      let (gmod, gl) = get_globals pel in
      if gmod = "" || gmod = gmod1 then (gmod1, [e :: gl])
      else raise Not_found
  | [] -> ("", [])
  | _ -> raise Not_found ]
;

value rec get_locals =
  fun
  [ [(<:patt< $_$ >>,
      <:expr< (grammar_entry_create $_$ : $_$) >>) :: pel] ->
        get_locals pel
  | [] -> ()
  | _ -> raise Not_found ]
;

value unposition =
  fun
  [ <:expr< None >> -> None
  | <:expr< Some Gramext.First >> -> Some Gramext.First
  | <:expr< Some Gramext.Last >> -> Some Gramext.Last
  | <:expr< Some (Gramext.Before $str:s$) >> -> Some (Gramext.Before s)
  | <:expr< Some (Gramext.After $str:s$) >> -> Some (Gramext.After s)
  | <:expr< Some (Gramext.Level $str:s$) >> -> Some (Gramext.Level s)
  | _ -> raise Not_found ]
;

value unlabel =
  fun
  [ <:expr< None >> -> None
  | <:expr< Some $str:s$ >> -> Some s
  | _ -> raise Not_found ]
;

value unassoc =
  fun
  [ <:expr< None >> -> None
  | <:expr< Some Gramext.NonA >> -> Some Gramext.NonA
  | <:expr< Some Gramext.LeftA >> -> Some Gramext.LeftA
  | <:expr< Some Gramext.RightA >> -> Some Gramext.RightA
  | _ -> raise Not_found ]
;

value rec unaction =
  fun
  [ <:expr< fun ($lid:locp$ : (Lexing.position * Lexing.position)) -> ($a$ : $_$) >>
    when locp = Stdpp.loc_name.val ->
      let ao =
        match a with
        [ <:expr< () >> -> None
        | _ -> Some a ]
      in
      ([], ao)
  | <:expr< fun ($p$ : $_$) -> $e$ >> ->
      let (pl, a) = unaction e in ([p :: pl], a)
  | <:expr< fun _ -> $e$ >> ->
      let (pl, a) = unaction e in
      (let _loc = (Token.nowhere, Token.nowhere) in [<:patt< _ >> :: pl], a)
  | _ -> raise Not_found ]
;

value untoken =
  fun
  [ <:expr< ($str:x$, $str:y$) >> -> (x, y)
  | _ -> raise Not_found ]
;

type symbol =
  [ Snterm of MLast.expr
  | Snterml of MLast.expr and string
  | Slist0 of symbol
  | Slist0sep of symbol and symbol
  | Slist1 of symbol
  | Slist1sep of symbol and symbol
  | Sopt of symbol
  | Sself
  | Snext
  | Stoken of Token.pattern
  | Srules of list (list (option MLast.patt * symbol) * option MLast.expr) ]
;

value rec unsymbol =
  fun
  [ <:expr< Gramext.Snterm ($uid:_$.Entry.obj ($e$ : $_$)) >> -> Snterm e
  | <:expr< Gramext.Snterml ($uid:_$.Entry.obj ($e$ : $_$)) $str:s$ >> ->
      Snterml e s
  | <:expr< Gramext.Snterml ($uid:_$.Entry.obj ($e$ : $_$), $str:s$) >> ->
      Snterml e s
  | <:expr< Gramext.Slist0 $e$ >> -> Slist0 (unsymbol e)
  | <:expr< Gramext.Slist0sep $e1$ $e2$ >> ->
      Slist0sep (unsymbol e1) (unsymbol e2)
  | <:expr< Gramext.Slist0sep ($e1$, $e2$) >> ->
      Slist0sep (unsymbol e1) (unsymbol e2)
  | <:expr< Gramext.Slist1 $e$ >> -> Slist1 (unsymbol e)
  | <:expr< Gramext.Slist1sep $e1$ $e2$ >> ->
      Slist1sep (unsymbol e1) (unsymbol e2)
  | <:expr< Gramext.Slist1sep ($e1$, $e2$) >> ->
      Slist1sep (unsymbol e1) (unsymbol e2)
  | <:expr< Gramext.Sopt $e$ >> -> Sopt (unsymbol e)
  | <:expr< Gramext.Sself >> -> Sself
  | <:expr< Gramext.Snext >> -> Snext
  | <:expr< Gramext.Stoken $e$ >> -> Stoken (untoken e)
  | <:expr< Gramext.srules $e$ >> -> Srules (unrule_list [] e)
  | _ -> raise Not_found ]
and unpsymbol_list pl e =
  match (pl, e) with
  [ ([], <:expr< [] >>) -> []
  | ([p :: pl], <:expr< [$e$ :: $el$] >>) ->
      let op =
        match p with
        [ <:patt< _ >> -> None
        | _ -> Some p ]
      in
      [(op, unsymbol e) :: unpsymbol_list pl el]
  | _ -> raise Not_found ]
and unrule =
  fun
  [ <:expr< ($e1$, Gramext.action $e2$) >> ->
      let (pl, a) =
        match unaction e2 with
        [ ([], None) -> let _loc = (Token.nowhere, Token.nowhere) in ([], Some <:expr< () >>)
        | x -> x ]
      in
      let sl = unpsymbol_list (List.rev pl) e1 in
      (sl, a)
  | _ -> raise Not_found ]
and unrule_list rl =
  fun
  [ <:expr< [$e$ :: $el$] >> -> unrule_list [unrule e :: rl] el
  | <:expr< [] >> -> rl
  | _ -> raise Not_found ]
;

value unlevel =
  fun
  [ <:expr< ($e1$, $e2$, $e3$) >> ->
      (unlabel e1, unassoc e2, unrule_list [] e3)
  | _ -> raise Not_found ]
;

value rec unlevel_list =
  fun
  [ <:expr< [$e$ :: $el$] >> -> [unlevel e :: unlevel_list el]
  | <:expr< [] >> -> []
  | _ -> raise Not_found ]
;

value unentry =
  fun
  [ <:expr< (Grammar.Entry.obj ($e$ : Grammar.Entry.e '$_$), $pos$, $ll$) >> ->
      (e, unposition pos, unlevel_list ll)
  | _ -> raise Not_found ]
;

value rec unentry_list =
  fun
  [ <:expr< [$e$ :: $el$] >> -> [unentry e :: unentry_list el]
  | <:expr< [] >> -> []
  | _ -> raise Not_found ]
;

value unextend_body e =
  let ((_, globals), e) =
    match e with
    [ <:expr< let $list:pel$ in $e1$ >> ->
        try (get_globals pel, e1) with
        [ Not_found -> (("", []), e) ]
    | _ -> (("", []), e) ]
  in
  let e =
    match e with
    [ <:expr<
        let grammar_entry_create s =
          Grammar.Entry.create (Grammar.of_entry $_$) s
        in
        $e$ >> ->
       let e =
         match e with
         [ <:expr< let $list:pel$ in $e1$ >> ->
             try let _ = get_locals pel in e1 with
             [ Not_found -> e ]
         | _ -> e ]
       in
       e
    | _ -> e ]
  in
  let el = unentry_list e in
  (globals, el)
;

value ungextend_body e =
  let e =
    match e with
    [ <:expr<
        let grammar_entry_create = Gram.Entry.create in
        let $list:ll$ in $e$
      >> ->
        let _ = get_locals ll in e
    | _ -> e ]
  in
  match e with
  [ <:expr< do { $list:el$ } >> ->
      List.map
        (fun
         [ <:expr< $uid:_$.extend ($e$ : $uid:_$.Entry.e '$_$) $pos$ $ll$ >> ->
             (e, unposition pos, unlevel_list ll)
         | _ -> raise Not_found ])
        el
  | _ -> raise Not_found ]
;

(* Printing *)

value ident s k = HVbox [: `S LR s; k :];
value string s k = HVbox [: `S LR ("\"" ^ s ^ "\""); k :];

value position =
  fun
  [ None -> [: :]
  | Some Gramext.First -> [: `S LR "FIRST" :]
  | Some Gramext.Last -> [: `S LR "LAST" :]
  | Some (Gramext.Before s) -> [: `S LR "BEFORE"; `string s [: :] :]
  | Some (Gramext.After s) -> [: `S LR "AFTER"; `string s [: :] :]
  | Some (Gramext.Level s) -> [: `S LR "LEVEL"; `string s [: :] :] ]
;

value action expr a dg k =
  expr a dg k
;

value token (con, prm) k =
  if con = "" then string prm k
  else if prm = "" then HVbox [: `S LR con; k :]
  else HVbox [: `S LR con; `string prm k :]
;

value simplify_rules rl =
  try
    List.map
      (fun
       [ ([(Some <:patt< $lid:x$ >>, s)], Some <:expr< $lid:y$ >>) ->
           if x = y then ([(None, s)], None) else raise Exit
       | ([], _) as r -> r
       | _ -> raise Exit ])
      rl
  with
  [ Exit -> rl ]
;

value rec symbol s k =
  match s with
  [ Snterm e -> expr e "" k
  | Snterml e s -> HVbox [: `expr e "" [: :]; `S LR "LEVEL"; `string s k :]
  | Slist0 s -> HVbox [: `S LR "LIST0"; `symbol s k :]
  | Slist0sep s sep ->
      HVbox
        [: `S LR "LIST0"; `symbol s [: :]; `S LR "SEP";
           `symbol sep k :]
  | Slist1 s -> HVbox [: `S LR "LIST1"; `symbol s k :]
  | Slist1sep s sep ->
      HVbox
        [: `S LR "LIST1"; `symbol  s [: :]; `S LR "SEP";
           `symbol  sep k :]
  | Sopt s -> HVbox [: `S LR "OPT"; `symbol  s k :]
  | Sself -> HVbox [: `S LR "SELF"; k :]
  | Snext -> HVbox [: `S LR "NEXT"; k :]
  | Stoken tok -> token tok k
  | Srules
      [([(Some <:patt< a >>, Snterm <:expr< a_list >>)], Some <:expr< a >>);
       ([(Some <:patt< a >>,
          ((Slist0 _ | Slist1 _ | Slist0sep _ _ | Slist1sep _ _) as s))],
          Some <:expr< Qast.List a >>)]
    when not no_slist.val
    ->
      match s with
      [ Slist0 s -> HVbox [: `S LR "SLIST0"; `simple_symbol s k :]
      | Slist1 s -> HVbox [: `S LR "SLIST1"; `simple_symbol s k :]
      | Slist0sep s sep ->
          HVbox
            [: `S LR "SLIST0"; `simple_symbol s [: :]; `S LR "SEP";
               `symbol sep k :]
      | Slist1sep s sep ->
          HVbox
            [: `S LR "SLIST1"; `simple_symbol s [: :]; `S LR "SEP";
               `simple_symbol sep k :]
      | _ -> assert False ]
  | Srules
      [([(Some <:patt< a >>, Snterm <:expr< a_opt >>)], Some <:expr< a >>);
       ([(Some <:patt< a >>, Sopt s)], Some <:expr< Qast.Option a >>)]
    when not no_slist.val
    ->
      let s =
        match s with
        [ Srules
            [([(Some <:patt< x >>, Stoken ("", str))],
              Some <:expr< Qast.Str x >>)] ->
            Stoken ("", str)
        | s -> s ]
      in
      HVbox [: `S LR "SOPT"; `simple_symbol s k :]
  | Srules rl ->
      let rl = simplify_rules rl in
      HVbox [: `HVbox [: :]; rule_list  rl k :] ]
and simple_symbol s k =
  match s with
  [ Snterml _ _ -> HVbox [: `S LO "("; `symbol s [: `S RO ")"; k :] :]
  | s -> symbol s k ]
and psymbol (p, s) k =
  match p with
  [ None -> symbol s k
  | Some p -> HVbox [: `patt p "" [: `S LR "=" :]; `symbol  s k :] ]
and psymbol_list sl k =
  listws psymbol (S RO ";") sl k
and rule  b (sl, a) dg k =
  match a with
  [ None -> HVbox [: b; `HOVbox [: psymbol_list sl k :] :]
  | Some a ->
      HVbox
        [: b;
           `HOVbox
              [: `HOVbox
                   [: `HVbox [: :];
                      psymbol_list  sl [: `S LR "->" :] :];
                 `action expr a dg k :] :] ]
and rule_list ll k =
  listwbws rule [: `S LR "[" :] (S LR "|") ll ""
    [: `S LR "]"; k :]
;

value label =
  fun
  [ Some s -> [: `S LR ("\"" ^ s ^ "\"") :]
  | None -> [: :] ]
;

value intloc loc = ((fst loc).Lexing.pos_cnum, (snd loc).Lexing.pos_cnum);

value assoc =
  fun
  [ Some Gramext.NonA -> [: `S LR "NONA" :]
  | Some Gramext.LeftA -> [: `S LR "LEFTA" :]
  | Some Gramext.RightA -> [: `S LR "RIGHTA" :]
  | None -> [: :] ]
;

value level b (lab, ass, rl) dg k =
  let s =
    if rl = [] then [: `S LR "[ ]"; k :]
    else [: `Vbox [: `HVbox [: :]; rule_list rl k :] :]
  in
  match (lab, ass) with
  [ (None, None) -> HVbox [: b; s :]
  | _ ->
      Vbox
        [: `HVbox [: b; label lab; assoc ass :];
           `HVbox [: `HVbox [: :]; s :] :] ]
;

value level_list ll k =
  Vbox
    [: `HVbox [: :];
       listwbws level [: `S LR "[" :] (S LR "|") ll ""
         [: `S LR "]"; k :] :]
;

value entry (e, pos, ll) k =
  BEbox
    [: `LocInfo (intloc(MLast.loc_of_expr e))
          (HVbox [: `expr e "" [: `S RO ":" :]; position pos :]);
       `level_list  ll [: :];
       `HVbox [: `S RO ";"; k :] :]
;

value entry_list el k =
  Vbox [: `HVbox [: :]; list entry el k :]
;

value extend_body (globals, e) k =
  let s = entry_list e k in
  match globals with
  [ [] -> s
  | sl ->
      HVbox
        [: `HVbox [: :];
           `HOVbox
             [: `S LR "GLOBAL"; `S RO ":";
                list (fun e k -> HVbox [: `expr e "" k :]) sl
                  [: `S RO ";" :] :];
           `s :] ]
;

value extend e dg k =
  match e with
  [ <:expr< Grammar.extend $e$ >> ->
      try
        let ex = unextend_body e in
        BEbox
          [: `S LR "EXTEND"; `extend_body ex [: :];
             `HVbox [: `S LR "END"; k :] :]
      with
      [ Not_found ->
          HVbox
            [: `S LR "Grammar.extend";
               `HOVbox
                  [: `S LO "(";
                     `expr e "" [: `HVbox [: `S RO ")"; k :] :] :] :] ]
  | _ -> expr e "" k ]
;

value get_gextend =
  fun
  [ <:expr< let $list:gl$ in $e$ >> ->
      try
        let (gmod, gl) = get_globals gl in
        let el = ungextend_body e in
        Some (gmod, gl, el)
      with
      [ Not_found -> None ]
  | _ -> None ]
;

value gextend e dg k =
  match get_gextend e with
  [ Some (gmod, gl, el) ->
      BEbox
        [: `HVbox [: `S LR "GEXTEND"; `S LR gmod :];
           `extend_body (gl, el) [: :];
           `HVbox [: `S LR "END"; k :] :]
  | None -> expr e "" k ]
;

value is_gextend e = get_gextend e <> None;

(* Printer extensions *)

let lev =
  try find_pr_level "expr1" pr_expr.pr_levels with
  [ Failure _ -> find_pr_level "top" pr_expr.pr_levels ]
in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< let $list:_$ in $_$ >> as e when is_gextend e ->
      fun curr next _ k -> [: `next e "" k :] ];

let lev = find_pr_level "apply" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< Grammar.extend $_$ >> as e ->
      fun curr next _ k -> [: `next e "" k :] ];

let lev = find_pr_level "simple" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< Grammar.extend $_$ >> as e ->
      fun curr next _ k -> [: `extend e "" k :]
  | <:expr< let $list:_$ in $_$ >> as e when is_gextend e ->
      fun curr next _ k -> [: `gextend e "" k :] ];

Pcaml.add_option "-no_slist" (Arg.Set no_slist)
  "Don't reconstruct SLIST and SOPT";
