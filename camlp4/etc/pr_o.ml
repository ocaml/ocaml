(* camlp4r q_MLast.cmo ./pa_extfun.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Pcaml;
open Spretty;
open Stdpp;

value no_ss = ref False;

value not_impl name x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  HVbox [: `S NO ("<pr_o: not impl: " ^ name ^ "; " ^ desc ^ ">") :]
;

value apply_it l f =
  apply_it_f l where rec apply_it_f =
    fun
    [ [] -> f
    | [a :: l] -> a (apply_it_f l) ]
;

value rec list elem =
  fun
  [ [] -> fun _ k -> k
  | [x] -> fun dg k -> [: `elem x dg k :]
  | [x :: l] -> fun dg k -> [: `elem x "" [: :]; list elem l dg k :] ]
;

value rec listws elem sep el dg k =
  match el with
  [ [] -> k
  | [x] -> [: `elem x dg k :]
  | [x :: l] ->
      let sdg =
        match sep with
        [ S _ x -> x
        | _ -> "" ]
      in
      [: `elem x sdg [: `sep :]; listws elem sep l dg k :] ]
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

value level box elem next e dg k =
  let rec curr e dg k = elem curr next e dg k in
  box (curr e dg k)
;

value is_infix =
  let infixes = Hashtbl.create 73 in
  do {
    List.iter (fun s -> Hashtbl.add infixes s True)
      ["=="; "!="; "+"; "+."; "-"; "-."; "*"; "*."; "/"; "/."; "**"; "**.";
       "="; "=."; "<>"; "<>."; "<"; "<."; ">"; ">."; "<="; "<=."; ">="; ">=.";
       "^"; "@"; "asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or";
       "quo"; "&&"; "||"; "~-"; "~-."];
    fun s -> try Hashtbl.find infixes s with [ Not_found -> False ]
  }
;

value is_keyword =
  let keywords = Hashtbl.create 301 in
  do {
    List.iter (fun s -> Hashtbl.add keywords s True)
      ["<>"; "<="; "struct"; "asr"; "<-"; ";;"; ":="; "type"; "::"; "true";
       "for"; "to"; "and"; "false"; "rec"; "or"; "of"; "with"; "while";
       "module"; "when"; "exception"; "lsr"; "lsl"; "done"; "function"; "/.";
       ".."; "->"; "in"; "-."; "if"; "lor"; "external"; "sig"; "+."; "then";
       "*."; "**"; "match"; "try"; "do"; "else"; "land"; "&&"; "as"; "open";
       "}"; "|"; "end"; "{"; "lxor"; "_"; "^"; "]"; "["; "let"; "!="; "||";
       "@"; ">"; "="; "<"; ";"; ":"; "mutable"; "/"; "[|"; "."; "-"; ","; "+";
       "begin"; "downto"; "*"; ")"; "|]"; "("; "'"; "&"; "functor"; ">="; "#";
       "~-."; "!"; "~-"; "fun"; "mod"; "=="; "val"];
    fun s -> try Hashtbl.find keywords s with [ Not_found -> False ]
  }
;

value has_special_chars v =
  match v.[0] with
  [ 'a'..'z' | 'A'..'Z' | '_' -> False
  | _ ->
      if String.length v >= 2 && v.[0] == '<' &&
         (v.[1] == '<' || v.[1] == ':')
      then
        False
      else True ]
;

value var_escaped v =
  if has_special_chars v || is_infix v then "( " ^ v ^ " )"
  else if is_keyword v then v ^ "__"
  else v
;

value conv_con =
  fun
  [ "True" -> "true"
  | "False" -> "false"
  | x -> x ]
;
value conv_lab =
  fun
  [ "val" -> "contents"
  | x -> x ]
;

(* default global loc *)

value loc = (0, 0);

value id_var s =
  if has_special_chars s || is_infix s then
    HVbox [: `S LR "("; `S LR s; `S LR ")" :]
  else if is_keyword s then HVbox [: `S LR (s ^ "__") :]
  else HVbox [: `S LR s :]
;

value virtual_flag =
  fun
  [ True -> [: `S LR "virtual" :]
  | _ -> [: :] ]
;

value rec_flag =
  fun
  [ True -> [: `S LR "rec" :]
  | _ -> [: :] ]
;

(* extensible printers *)

value sig_item x dg k =
  let k = if no_ss.val then k else [: `S RO ";;"; k :] in
  pr_sig_item.pr_fun "top" x "" k
;
value str_item x dg k =
  let k = if no_ss.val then k else [: `S RO ";;"; k :] in
  pr_str_item.pr_fun "top" x "" k
;
value expr e dg k = pr_expr.pr_fun "top" e dg k;
value patt e dg k = pr_patt.pr_fun "top" e dg k;
value expr1 e dg k = pr_expr.pr_fun "expr1" e dg k;
value simple_expr e dg k = pr_expr.pr_fun "simple" e dg k;
value patt1 e dg k = pr_patt.pr_fun "patt1" e dg k;
value simple_patt e dg k = pr_patt.pr_fun "simple" e dg k;
value expr_fun_args ge = Extfun.apply pr_expr_fun_args.val ge;

(* type core *)

value ctyp_f = ref (fun []);
value simple_ctyp_f = ref (fun []);

value ctyp t _ k = ctyp_f.val t "" k;
value simple_ctyp t _ k = simple_ctyp_f.val t "" k;

value mutable_flag =
  fun
  [ True -> [: `S LR "mutable" :]
  | _ -> [: :] ]
;

value private_flag =
  fun
  [ True -> [: `S LR "private" :]
  | _ -> [: :] ]
;
  
value rec labels b vl _ k = [: b; listws label (S RO ";") vl "" k :]
and label (f, m, t) _ k =
  HVbox
    [: mutable_flag m; `HVbox [: `S LR (conv_lab f); `S LR ":" :];
       `ctyp t "" k :]
;

value rec ctyp_list tel _ k = listws simple_ctyp (S LR "*") tel "" k;

value rec variants b vl _ k = listwbws variant b (S LR "|") vl "" k
and variant b (c, tl) _ k =
  match tl with
  [ [] -> HVbox [: b; `HOVbox [: `S LR c; k :] :]
  | _ -> HVbox [: b; `HOVbox [: `S LR c; `S LR "of"; ctyp_list tl "" k :] :] ]
;

value rec row_fields b rfl _ k = listwbws row_field b (S LR "|") rfl "" k
and row_field b rf _ k =
  match rf with
  [ MLast.RfTag c ao tl ->
      let c = "`" ^ c in
      match tl with
      [ [] -> HVbox [: b; `HOVbox [: `S LR c; k :] :]
      | _ ->
          let ao = if ao then [: `S LR "&" :] else [: :] in
          HVbox
            [: b; `HOVbox [: `S LR c; `S LR "of"; ao; ctyp_list tl "" k :] :] ]
  | MLast.RfInh t ->
      HVbox [: b; `ctyp t "" k :] ]
;

value rec get_type_args t tl =
  match t with
  [ <:ctyp< $t1$ $t2$ >> -> get_type_args t1 [t2 :: tl]
  | _ -> (t, tl) ]
;

value module_pref =
  apply_it
    [level (fun x -> HOVbox x)
       (fun curr next t _ k ->
          match t with
          [ <:ctyp< $t1$ $t2$ >> ->
              let (t, tl) = get_type_args t1 [t2] in
              [: curr t "" [: :];
                 list
                   (fun t _ k ->
                      HOVbox [: `S NO "("; curr t "" [: :]; `S RO ")"; k :])
                   tl "" k :]
          | <:ctyp< $t1$ . $t2$ >> ->
              [: curr t1 "" [: `S NO "." :]; `next t2 "" k :]
          | _ -> [: `next t "" k :] ])]
    simple_ctyp
;

value rec class_longident sl dg k =
  match sl with
  [ [i] -> HVbox [: `S LR i; k :]
  | [m :: sl] -> HVbox [: `S LR m; `S NO "."; `class_longident sl dg k :]
  | _ -> HVbox [: `not_impl "class_longident" sl; k :] ]
;

value rec clty_longident sl dg k =
  match sl with
  [ [i] -> HVbox [: `S LR i; k :]
  | [m :: sl] -> HVbox [: `S LR m; `S NO "."; `clty_longident sl dg k :]
  | _ -> HVbox [: `not_impl "clty_longident" sl; k :] ]
;

value rec meth_list (ml, v) dg k =
  match (ml, v) with
  [ ([f], False) -> [: `field f dg k :]
  | ([], _) -> [: `S LR ".."; k :]
  | ([f :: ml], v) ->
      [: `field f "" [: `S RO ";" :]; meth_list (ml, v) dg k :] ]
and field (lab, t) dg k = HVbox [: `S LR lab; `S LR ":"; `ctyp t dg k :];

simple_ctyp_f.val :=
  apply_it
    [level (fun x -> HOVbox x)
       (fun curr next t _ k ->
          match t with
          [ <:ctyp< $t1$ == $t2$ >> ->
              [: curr t1 "=" [: `S LR "=" :]; `next t2 "" k :]
          | t -> [: `next t "" k :] ]);
     level (fun x -> HOVbox x)
       (fun curr next t _ k ->
          match t with
          [ <:ctyp< ? $lab$ : $t$ >> ->
              [: `S LO "?"; `S LR lab; `S RO ":"; `next t "" k :]
          | <:ctyp< ~ $lab$ : $t$ >> -> [: `S LO (lab ^ ":"); `next t "" k :]
          | t -> [: `next t "" k :] ]);
     level (fun x -> HOVbox x)
       (fun curr next t _ k ->
          match t with
          [ <:ctyp< $t1$ $t2$ >> ->
              let (t, tl) = get_type_args t1 [t2] in
              match tl with
              [ [<:ctyp< $_$ $_$ >>] -> [: curr t2 "" [: :]; curr t1 "" k :]
              | [_] -> [: `next t2 "" [: :]; curr t1 "" k :]
              | _ ->
                  [: `S LO "(";
                     listws (fun x _ k -> HOVbox [: curr x "" k :]) (S RO ",")
                       tl "" [: `S RO ")" :];
                     curr t "" k :] ]
          | MLast.TyXnd _ c t -> [: `next t "" [: :]; `S LR ("Xnd_" ^ c) :]
          | t -> [: `next t "" k :] ]);
     level (fun x -> HOVbox x)
       (fun curr next t _ k ->
          match t with
          [ <:ctyp< $t1$ . $t2$ >> ->
              [: `module_pref t1 "" [: `S NO "." :]; `next t2 "" k :]
          | t -> [: `next t "" k :] ]);
     level (fun x -> HOVbox x)
       (fun curr next t _ k ->
          match t with
          [ <:ctyp< '$s$ >> -> [: `S LO "'"; `S LR (var_escaped s); k :]
          | <:ctyp< $lid:s$ >> -> [: `S LR s; k :]
          | <:ctyp< $uid:s$ >> -> [: `S LR s; k :]
          | <:ctyp< _ >> -> [: `S LR "_"; k :]
          | <:ctyp< { $list:ftl$ } >> ->
              [: `HVbox
                    [: labels [: `S LR "{" :] ftl "" [: `S LR "}" :]; k :] :]
          | <:ctyp< [ $list:ctl$ ] >> ->
              [: `BEbox [: `HVbox [: :]; variants [: :] ctl "" [: :]; k :] :]
          | <:ctyp< [| $list:rfl$ |] >> ->
              [: `HVbox
                    [: `HVbox [: :];
                       row_fields [: `S LR "[" :] rfl "" [: `S LR "]" :];
                       k :] :]
          | <:ctyp< [| > $list:rfl$ |] >> ->
              [: `HVbox
                    [: `HVbox [: :];
                       row_fields [: `S LR "[>" :] rfl "" [: `S LR "]" :];
                       k :] :]
          | <:ctyp< [| < $list:rfl$ > $list:sl$ |] >> ->
              let k1 = [: `S LR "]" :] in
              let k1 =
                match sl with
                [ [] -> k1
                | l ->
                    [: `S LR ">";
                       list (fun x _ k -> HVbox [: `S LR x; k :]) l "" k1 :] ]
              in
              [: `HVbox
                    [: `HVbox [: :]; row_fields [: `S LR "[<" :] rfl "" k1;
                       k :] :]
          | <:ctyp< $_$ -> $_$ >> | <:ctyp< $_$ $_$ >> |
            <:ctyp< $_$ == $_$ >> | <:ctyp< $_$ . $_$ >> |
            <:ctyp< ($list:_$) >> | <:ctyp< $_$ as $_$ >> |
            <:ctyp< ~ $_$ : $_$ >> | <:ctyp< ? $_$ : $_$ >> |
            MLast.TyXnd _ _ _ as t ->
              [: `S LO "("; `ctyp t "" [: `HVbox [: `S RO ")"; k :] :] :]
          | MLast.TyCls _ id -> [: `S LO "#"; `class_longident id "" k :]
          | MLast.TyObj _ [] False -> [: `S LR "<>"; k :]
          | MLast.TyObj _ ml v ->
              [: `S LR "<"; meth_list (ml, v) "" [: `S LR ">"; k :] :] ])]
    (fun t _ k -> not_impl "ctyp" t);

ctyp_f.val :=
  apply_it
    [level (fun x -> HOVbox x)
       (fun curr next t _ k ->
          match t with
          [ <:ctyp< $x$ as $y$ >> ->
              [: curr x "" [: `S LR "as" :]; `next y "" k :]
          | t -> [: `next t "" k :] ]);
     level (fun x -> HOVbox x)
       (fun curr next t _ k ->
          match t with
          [ <:ctyp< $x$ -> $y$ >> ->
              [: `next x "" [: `S LR "->" :]; curr y "" k :]
          | t -> [: `next t "" k :] ]);
     level (fun x -> HOVbox x)
       (fun curr next t _ k ->
          match t with
          [ <:ctyp< ? $lab$ : $t$ >> ->
              [: `S LO "?"; `S LR lab; `S RO ":"; `next t "" k :]
          | t -> [: `next t "" k :] ]);
     level (fun x -> HOVbox x)
       (fun curr next t _ k ->
          match t with
          [ <:ctyp< ($list:tl$) >> -> listws next (S LR "*") tl "" k
          | t -> [: `next t "" k :] ])]
    simple_ctyp;

(* patterns *)

value rec get_patt_args a al =
  match a with
  [ <:patt< $a1$ $a2$ >> -> get_patt_args a1 [a2 :: al]
  | _ -> (a, al) ]
;

value rec is_irrefut_patt =
  fun
  [ <:patt< $lid:_$ >> -> True
  | <:patt< () >> -> True
  | <:patt< _ >> -> True
  | <:patt< ($x$ as $_$) >> -> is_irrefut_patt x
  | <:patt< { $list:fpl$ } >> ->
      List.for_all (fun (_, p) -> is_irrefut_patt p) fpl
  | <:patt< ($p$ : $_$) >> -> is_irrefut_patt p
  | <:patt< ($list:pl$) >> -> List.for_all is_irrefut_patt pl
  | <:patt< ? $_$ : $p$ >> -> is_irrefut_patt p
  | <:patt< ? $_$ : ($p$ = $_$) >> -> is_irrefut_patt p
  | <:patt< ~ $_$ : $p$ >> -> is_irrefut_patt p
  | _ -> False ]
;

(* expressions *)

pr_expr_fun_args.val :=
  extfun Extfun.empty with
  [ <:expr< fun [$p$ -> $e$] >> as ge ->
      if is_irrefut_patt p then
        let (pl, e) = expr_fun_args e in
        ([p :: pl], e)
      else ([], ge)
  | ge -> ([], ge) ];

value raise_match_failure (bp, ep) k =
  HOVbox
    [: `S LR "raise"; `S LO "("; `S LR "Match_failure"; `S LO "(";
       `S LR ("\"" ^ Pcaml.input_file.val ^ "\""); `S RO ",";
       `S LR (string_of_int bp); `S RO ","; `S LR (string_of_int ep);
       `S RO ")"; `S RO ")"; k :]
;

value rec bind_list b pel _ k =
  match pel with
  [ [pe] -> let_binding b pe "" k
  | pel ->
      Vbox [: `HVbox [: :]; listwbws let_binding b (S LR "and") pel "" k :] ]
and let_binding b (p, e) _ k =
  let (pl, e) =
    match p with
    [ <:patt< ($_$ : $_$) >> -> ([], e)
    | _ -> expr_fun_args e ]
  in
  match (p, e) with
  [ (<:patt< $lid:_$ >>, <:expr< ($e$ : $t$) >>) ->
      BEbox
        [: `HVbox
              [: `HVbox b;
                 `HVbox (list simple_patt [p :: pl] "" [: `S LR ":" :]);
                 `ctyp t "" [: `S LR "=" :] :];
           `expr e "" [: :]; k :]
  | _ ->
      BEbox
        [: `HVbox
              [: `HVbox b;
                 `HVbox (list simple_patt [p :: pl] "" [: `S LR "=" :]) :];
           `expr e "" [: :]; k :] ]
and match_assoc_list loc pel dg k =
  match pel with
  [ [] ->
      HVbox
        [: `HVbox [: `S LR "_"; `S LR "->" :]; `raise_match_failure loc k :]
  | _ ->
      BEVbox
        [: `HVbox [: :]; listwbws match_assoc [: :] (S LR "|") pel "" k :] ]
and match_assoc b (p, w, e) dg k =
  let s =
    match w with
    [ Some e1 ->
        [: `HVbox
              [: `HVbox [: :]; `patt p "" [: :];
                 `HVbox [: `S LR "when"; `expr e1 "" [: `S LR "->" :] :] :] :]
    | _ -> [: `patt p "" [: `S LR "->" :] :] ]
  in
  HVbox [: b; `HVbox [: `HVbox s; `expr e dg k :] :]
;

value rec get_expr_args a al =
  match a with
  [ <:expr< $a1$ $a2$ >> -> get_expr_args a1 [a2 :: al]
  | _ -> (a, al) ]
;

value label lab = S LR lab;

value field_expr (lab, e) dg k =
  HVbox [: `label lab; `S LR "="; `expr e dg k :]
;

value type_params sl _ k =
  match sl with
  [ [] -> k
  | [(s, _)] -> [: `S LO "'"; `S LR s; k :]
  | sl ->
      [: `S LO "(";
         listws (fun (s, _) _ k -> HVbox [: `S LO "'"; `S LR s; k :])
           (S RO ",") sl "" [: `S RO ")"; k :] :] ]
;

value constrain (t1, t2) _ k =
  HVbox [: `S LR "constraint"; `ctyp t1 "" [: `S LR "=" :]; `ctyp t2 "" k :]
;

value type_list b tdl _ k =
  HVbox
    [: `HVbox [: :];
       listwbws
         (fun b ((_, tn), tp, te, cl) _ k ->
            let cstr = list constrain cl "" k in
            match te with
            [ <:ctyp< '$s$ >> when not (List.mem_assoc s tp) ->
                HVbox [: b; type_params tp "" [: :]; `S LR tn; cstr :]
            | <:ctyp< [ $list:[]$ ] >> ->
                HVbox [: b; type_params tp "" [: :]; `S LR tn; cstr :]
            | _ ->
                HVbox
                  [: `HVbox
                        [: b; type_params tp "" [: :]; `S LR tn; `S LR "=" :];
                     `ctyp te "" [: :]; cstr :] ])
         b (S LR "and") tdl "" [: :];
       k :]
;

value external_def (s, t, pl) _ k =
  let ls =
    list (fun s _ k -> HVbox [: `S LR ("\"" ^ s ^ "\""); k :]) pl "" k
  in
  HVbox
    [: `HVbox [: `S LR "external"; `S LR (var_escaped s); `S LR ":" :];
       `ctyp t "" [: `S LR "="; ls :] :]
;

value value_description (s, t) _ k =
  HVbox
    [: `HVbox [: `S LR "val"; `S LR (var_escaped s); `S LR ":" :];
       `ctyp t "" k :]
;

value rec mod_ident sl _ k =
  match sl with
  [ [] -> k
  | [s] -> [: `S LR s; k :]
  | [s :: sl] -> [: `S LR s; `S NO "."; mod_ident sl "" k :] ]
;

value rec module_type mt k =
  let next = module_type1 in
  match mt with
  [ <:module_type< functor ( $s$ : $mt1$ ) -> $mt2$ >> ->
      let head =
        HVbox
          [: `S LR "functor"; `S LO "("; `S LR s; `S LR ":";
             `module_type mt1 [: `S RO ")" :]; `S LR "->" :]
      in
      HVbox [: `head; `module_type mt2 k :]
  | _ -> next mt k ]
and module_type1 mt k =
  let curr = module_type1 in
  let next = module_type2 in
  match mt with
  [ <:module_type< $mt$ with $list:icl$ >> ->
      HVbox
        [: `curr mt [: :]; `with_constraints [: `S LR "with" :] icl "" k :]
  | _ -> next mt k ]
and module_type2 mt k =
  let curr = module_type2 in
  let next = module_type3 in
  match mt with
  [ <:module_type< sig $list:s$ end >> ->
      BEbox
        [: `S LR "sig"; `HVbox [: `HVbox [: :]; list sig_item s "" [: :] :];
           `HVbox [: `S LR "end"; k :] :]
  | _ -> next mt k ]
and module_type3 mt k =
  let curr = module_type3 in
  let next = module_type5 in
  match mt with
  [ <:module_type< $mt1$ $mt2$ >> ->
      HVbox [: `curr mt1 [: :]; `S LO "("; `next mt2 [: `S RO ")"; k :] :]
  | <:module_type< $mt1$ . $mt2$ >> ->
      HVbox [: `curr mt1 [: `S NO "." :]; `next mt2 k :]
  | _ -> next mt k ]
and module_type5 mt k =
  match mt with
  [ <:module_type< $lid:s$ >> -> HVbox [: `S LR s; k :]
  | <:module_type< $uid:s$ >> -> HVbox [: `S LR s; k :]
  | _ -> HVbox [: `S LO "("; `module_type mt [: `S RO ")"; k :] :] ]
and modtype_declaration (s, mt) _ k =
  HVbox
    [: `HVbox [: :];
       `HVbox
          [: `HVbox [: `S LR "module"; `S LR "type"; `S LR s; `S LR "=" :];
             `module_type mt [: :] :];
       k :]
and with_constraints b icl _ k =
  HVbox [: `HVbox [: :]; listwbws with_constraint b (S LR "and") icl "" k :]
and with_constraint b wc _ k =
  match wc with
  [ MLast.WcTyp _ p al e ->
      let params =
        match al with
        [ [] -> [: :]
        | [s] -> [: `S LO "'"; `S LR (fst s) :]
        | sl -> [: `S LO "("; type_params sl "" [: `S RO ")" :] :] ]
      in
      HVbox
        [: `HVbox
              [: `HVbox b; `S LR "type"; params;
                 mod_ident p "" [: `S LR "=" :] :];
           `ctyp e "" k :]
  | MLast.WcMod _ sl mt ->
      HVbox
        [: b; `S LR "module"; mod_ident sl "" [: `S LR "=" :];
           `module_type mt k :] ]
and module_expr me _ k =
  match me with
  [ <:module_expr< struct $list:s$ end >> ->
      let s = HVbox [: `S LR "struct"; list str_item s "" [: :] :] in
      HVbox [: `HVbox [: :]; `s; `HVbox [: `S LR "end"; k :] :]
  | <:module_expr< functor ($s$ : $mt$) -> $me$ >> ->
      let head =
        HVbox
          [: `S LR "functor"; `S LO "("; `S LR s; `S LR ":";
             `module_type mt [: `S RO ")" :]; `S LR "->" :]
      in
      HVbox [: `head; `module_expr me "" k :]
  | _ -> module_expr1 me "" k ]
and module_expr1 me _ k =
  let curr = module_expr1 in
  let next = module_expr2 in
  match me with
  [ <:module_expr< $me1$ $me2$ >> ->
      HVbox
        [: `curr me1 "" [: :]; `S LO "("; `next me2 "" [: `S RO ")"; k :] :]
  | _ -> next me "" k ]
and module_expr2 me _ k =
  let curr = module_expr2 in
  let next = module_expr3 in
  match me with
  [ <:module_expr< $me1$ . $me2$ >> ->
      HVbox [: `curr me1 "" [: `S NO "." :]; `next me2 "" k :]
  | _ -> next me "" k ]
and module_expr3 me _ k =
  match me with
  [ <:module_expr< $uid:s$ >> -> HVbox [: `S LR s; k :]
  | <:module_expr< ( $me$ : $mt$ ) >> ->
      HVbox
        [: `S LO "("; `module_expr me "" [: `S LR ":" :];
           `module_type mt [: `S RO ")"; k :] :]
  | <:module_expr< struct $list:_$ end >> ->
      HVbox [: `S LO "("; `module_expr me "" [: `S RO ")"; k :] :]
  | x -> not_impl "module_expr2" x ]
and module_binding b me k =
  match me with
  [ <:module_expr< functor ($s$ : $mt$) -> $mb$ >> ->
      module_binding
        [: `HVbox
              [: b; `S LO "("; `S LR s; `S LR ":";
                 `module_type mt [: `S RO ")" :] :] :]
        mb k
  | <:module_expr< ( $me$ : $mt$ ) >> ->
      HVbox
        [: `HVbox [: :];
           `HVbox
              [: `HVbox
                    [: `HVbox [: b; `S LR ":" :];
                       `module_type mt [: `S LR "=" :] :];
                 `module_expr me "" [: :] :];
           k :]
  | _ ->
      HVbox
        [: `HVbox [: :];
           `HVbox [: `HVbox [: b; `S LR "=" :]; `module_expr me "" [: :] :];
           k :] ]
and class_declaration b ci _ k =
  class_fun_binding
    [: b; virtual_flag ci.MLast.ciVir; class_type_parameters ci.MLast.ciPrm;
       `S LR ci.MLast.ciNam :]
    ci.MLast.ciExp k
and class_fun_binding b ce k =
  match ce with
  [ MLast.CeFun _ p cfb ->
      class_fun_binding [: b; `simple_patt p "" [: :] :] cfb k
  | ce -> HVbox [: `HVbox [: b; `S LR "=" :]; `class_expr ce k :] ]
and class_type_parameters (loc, tpl) =
  match tpl with
  [ [] -> [: :]
  | tpl ->
      [: `S LO "[";
         listws type_parameter (S RO ",") tpl "" [: `S RO "]" :] :] ]
and type_parameter tp dg k = HVbox [: `S LO "'"; `S LR (fst tp); k :]
and class_expr ce k =
  match ce with
  [ MLast.CeFun _ p ce ->
      HVbox
        [: `S LR "fun"; `simple_patt p "" [: `S LR "->" :];
           `class_expr ce k :]
  | MLast.CeLet _ rf lb ce ->
      HVbox
        [: `HVbox [: :];
           `bind_list [: `S LR "let"; rec_flag rf :] lb "" [: `S LR "in" :];
           `class_expr ce k :]
  | ce -> class_expr1 ce k ]
and class_expr1 ce k =
  match ce with
  [ MLast.CeApp _ ce sel ->
      HVbox [: `class_expr1 ce [: :]; list simple_expr sel "" k :]
  | ce -> class_expr2 ce k ]
and class_expr2 ce k =
  match ce with
  [ MLast.CeCon _ ci [] -> class_longident ci "" k
  | MLast.CeCon _ ci ctcl ->
      HVbox
        [: `S LO "["; listws ctyp (S RO ",") ctcl "" [: `S RO "]" :];
           `class_longident ci "" k :]
  | MLast.CeStr _ csp cf ->
      class_structure [: `S LR "object"; `class_self_patt_opt csp :] cf
        [: `S LR "end"; k :]
  | MLast.CeTyc _ ce ct ->
      HVbox
        [: `S LO "("; `class_expr ce [: `S LR ":" :];
           `class_type ct [: `S RO ")"; k :] :]
  | MLast.CeFun _ _ _ ->
      HVbox [: `S LO "("; `class_expr ce [: `S RO ")"; k :] :]
  | _ -> HVbox [: `not_impl "class_expr" ce; k :] ]
and class_structure b cf k =
  BEbox
    [: `HVbox b; `HVbox [: `HVbox [: :]; list class_str_item cf "" [: :] :];
       `HVbox k :]
and class_self_patt_opt csp =
  match csp with
  [ Some p -> HVbox [: `S LO "("; `patt p "" [: `S RO ")" :] :]
  | None -> HVbox [: :] ]
and class_str_item cf dg k =
  match cf with
  [ MLast.CrDcl _ s ->
      HVbox [: `HVbox [: :]; list class_str_item s "" [: :] :]
  | MLast.CrInh _ ce pb ->
      HVbox
        [: `S LR "inherit"; `class_expr ce [: :];
           match pb with
           [ Some i -> [: `S LR "as"; `S LR i :]
           | _ -> [: :] ];
           k :]
  | MLast.CrVal _ lab mf e -> HVbox [: `S LR "val"; `cvalue (lab, mf, e) k :]
  | MLast.CrVir _ lab pf t ->
      HVbox
        [: `S LR "method"; `S LR "virtual"; private_flag pf; `label lab;
           `S LR ":"; `ctyp t "" k :]
  | MLast.CrMth _ lab pf fb ->
      fun_binding [: `S LR "method"; private_flag pf; `label lab :] fb k
  | MLast.CrCtr _ t1 t2 ->
      HVbox
        [: `HVbox [: `S LR "constraint"; `ctyp t1 "" [: `S LR "=" :] :];
           `ctyp t2 "" k :]
  | MLast.CrIni _ se -> HVbox [: `S LR "initializer"; `expr se "" k :] ]
and cvalue (lab, mf, e) k =
  HVbox [: mutable_flag mf; `label lab; `S LR "="; `expr e "" k :]
and fun_binding b fb k =
  match fb with
  [ <:expr< fun $p$ -> $e$ >> ->
      fun_binding [: b; `simple_patt p "" [: :] :] e k
  | e -> HVbox [: `HVbox [: b; `S LR "=" :]; `expr e "" k :] ]
and class_type ct k =
  match ct with
  [ MLast.CtFun _ t ct ->
      HVbox [: `ctyp t "" [: `S LR "->" :]; `class_type ct k :]
  | _ -> class_signature ct k ]
and class_signature cs k =
  match cs with
  [ MLast.CtCon _ id [] -> clty_longident id "" k
  | MLast.CtCon _ id tl ->
      HVbox
        [: `S LO "["; listws ctyp (S RO ",") tl "" [: `S RO "]" :];
           `clty_longident id "" k :]
  | MLast.CtSig _ cst csf ->
      class_self_type [: `S LR "object" :] cst
        [: `HVbox [: `HVbox [: :]; list class_sig_item csf "" [: :] :];
           `HVbox [: `S LR "end"; k :] :]
  | _ -> HVbox [: `not_impl "class_signature" cs; k :] ]
and class_self_type b cst k =
  BEbox
    [: `HVbox
          [: b;
             match cst with
             [ None -> [: :]
             | Some t -> [: `S LO "("; `ctyp t "" [: `S RO ")" :] :] ] :];
       k :]
and class_sig_item csf dg k =
  match csf with
  [ MLast.CgCtr _ t1 t2 ->
      HVbox
        [: `S LR "constraint"; `ctyp t1 "" [: `S LR "=" :]; `ctyp t2 "" k :]
  | MLast.CgDcl _ s ->
      HVbox [: `HVbox [: :]; list class_sig_item s "" [: :] :]
  | MLast.CgMth _ lab pf t ->
      HVbox
        [: `S LR "method"; private_flag pf; `label lab; `S LR ":";
           `ctyp t "" k :]
  | MLast.CgVal _ lab mf t ->
      HVbox
        [: `S LR "val"; mutable_flag mf; `label lab; `S LR ":";
           `ctyp t "" k :]
  | _ -> HVbox [: `not_impl "class_sig_item" csf; k :] ]
and class_description b ci _ k =
  HVbox
    [: `HVbox
          [: b; virtual_flag ci.MLast.ciVir;
             class_type_parameters ci.MLast.ciPrm; `S LR ci.MLast.ciNam;
             `S LR ":" :];
       `class_type ci.MLast.ciExp k :]
and class_type_declaration b ci _ k =
  HVbox
    [: `HVbox
          [: b; virtual_flag ci.MLast.ciVir;
             class_type_parameters ci.MLast.ciPrm; `S LR ci.MLast.ciNam;
             `S LR "=" :];
       `class_signature ci.MLast.ciExp k :]
;

pr_sig_item.pr_levels :=
  [{pr_label = "top"; pr_box _ x = HVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:sig_item< type $list:stl$ >> ->
          fun curr next dg k -> [: `type_list [: `S LR "type" :] stl "" k :]
      | <:sig_item< declare $list:s$ end >> ->
          fun curr next dg k -> [: `HVbox [: :]; list sig_item s "" [: :] :]
      | MLast.SgDir _ _ _ as si ->
          fun curr next dg k -> [: `not_impl "sig_item" si :]
      | <:sig_item< exception $c$ of $list:tl$ >> ->
          fun curr next dg k ->
            [: `variant [: `S LR "exception" :] (c, tl) "" k :]
      | <:sig_item< value $s$ : $t$ >> ->
          fun curr next dg k -> [: `value_description (s, t) "" k :]
      | <:sig_item< external $s$ : $t$ = $list:pl$ >> ->
          fun curr next dg k -> [: `external_def (s, t, pl) "" k :]
      | <:sig_item< include $mt$ >> ->
          fun curr next dg k -> [: `S LR "include"; `module_type mt k :]
      | <:sig_item< module $s$ : $mt$ >> ->
          fun curr next dg k ->
            [: `HVbox [: :];
               `HVbox
                  [: `HVbox [: `S LR "module"; `S LR s; `S LR ":" :];
                     `module_type mt [: :] :];
               k :]
      | <:sig_item< module type $s$ = $mt$ >> ->
          fun curr next dg k -> [: `modtype_declaration (s, mt) "" k :]
      | <:sig_item< open $sl$ >> ->
          fun curr next dg k -> [: `S LR "open"; mod_ident sl "" k :]
      | MLast.SgCls _ cd ->
          fun curr next dg k ->
            [: `HVbox [: :];
               listwbws class_description [: `S LR "class" :] (S LR "and") cd
                 "" k :]
      | MLast.SgClt _ cd ->
          fun curr next dg k ->
            [: `HVbox [: :];
               listwbws class_type_declaration
                 [: `S LR "class"; `S LR "type" :] (S LR "and") cd ""
                 k :] ]}];

pr_str_item.pr_levels :=
  [{pr_label = "top"; pr_box _ x = HVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:str_item< open $i$ >> ->
          fun curr next dg k -> [: `S LR "open"; mod_ident i "" k :]
      | <:str_item< $exp:e$ >> ->
          fun curr next dg k ->
            if no_ss.val then
              [: `HVbox [: `S LR "let"; `S LR "_"; `S LR "=" :];
                 `expr e "" k :]
            else [: `HVbox [: :]; `expr e "" k :]
      | <:str_item< declare $list:s$ end >> ->
          fun curr next dg k -> [: `HVbox [: :]; list str_item s "" [: :] :]
      | <:str_item< # $s$ $opt:x$ >> ->
          fun curr next dg k ->
            let s =
              "(* #" ^ s ^ " " ^
                (match x with
                 [ Some <:expr< $str:s$ >> -> "\"" ^ s ^ "\""
                 | _ -> "?" ]) ^
                " *)"
            in
            [: `S LR s :]
      | <:str_item< exception $c$ of $list:tl$ >> ->
          fun curr next dg k ->
            [: `variant [: `S LR "exception" :] (c, tl) "" k :]
      | <:str_item< include $me$ >> ->
          fun curr next dg k -> [: `S LR "include"; `module_expr me "" k :]
      | <:str_item< type $list:tdl$ >> ->
          fun curr next dg k -> [: `type_list [: `S LR "type" :] tdl "" k :]
      | <:str_item< value $rec:rf$ $list:pel$ >> ->
          fun curr next dg k ->
            [: `bind_list
                  [: `S LR "let"; if rf then [: `S LR "rec" :] else [: :] :]
                  pel "" k :]
      | <:str_item< external $s$ : $t$ = $list:pl$ >> ->
          fun curr next dg k -> [: `external_def (s, t, pl) "" k :]
      | <:str_item< module $s$ = $me$ >> ->
          fun curr next dg k ->
            [: `module_binding [: `S LR "module"; `S LR s :] me k :]
      | <:str_item< module type $s$ = $mt$ >> ->
          fun curr next dg k ->
            [: `HVbox [: :];
               `HVbox
                  [: `HVbox
                        [: `S LR "module"; `S LR "type"; `S LR s;
                           `S LR "=" :];
                     `module_type mt [: :] :];
               k :]
      | MLast.StCls _ cd ->
          fun curr next dg k ->
            [: `HVbox [: :];
               listwbws class_declaration [: `S LR "class" :] (S LR "and") cd
                 "" k :]
      | MLast.StClt _ cd ->
          fun curr next dg k ->
            [: `HVbox [: :];
               listwbws class_type_declaration
                 [: `S LR "class"; `S LR "type" :] (S LR "and") cd ""
                 k :] ]}];

pr_expr.pr_levels :=
  [{pr_label = "top"; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< do { $list:el$ } >> ->
          fun curr next dg k ->
            [: `HVbox [: `HVbox [: :]; listws next (S RO ";") el dg k :] :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = "expr1"; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< let $rec:r$ $p1$ = $e1$ in $e$ >> ->
          fun curr next dg k ->
            let r = if r then [: `S LR "rec" :] else [: :] in
            if dg <> ";" then
              [: `HVbox
                    [: `HVbox [: :];
                       `let_binding [: `S LR "let"; r :] (p1, e1) ""
                          [: `S LR "in" :];
                       `expr e dg k :] :]
            else
              let pel = [(p1, e1)] in
              [: `BEbox
                    [: `S LR "begin";
                       `HVbox
                          [: `HVbox [: :];
                             listwbws
                               (fun b (p, e) _ k -> let_binding b (p, e) "" k)
                               [: `S LR "let"; r :] (S LR "and") pel ""
                               [: `S LR "in" :];
                             `expr e "" [: :] :];
                       `HVbox [: `S LR "end"; k :] :] :]
      | <:expr< let $rec:r$ $list:pel$ in $e$ >> ->
          fun curr next dg k ->
            let r = if r then [: `S LR "rec" :] else [: :] in
            if dg <> ";" then
              [: `Vbox
                    [: `HVbox [: :];
                       listwbws
                         (fun b (p, e) _ k -> let_binding b (p, e) "" k)
                         [: `S LR "let"; r :] (S LR "and") pel ""
                         [: `S LR "in" :];
                       `expr e dg k :] :]
            else
              [: `BEbox
                    [: `S LR "begin";
                       `HVbox
                          [: `HVbox [: :];
                             listwbws
                               (fun b (p, e) _ k -> let_binding b (p, e) "" k)
                               [: `S LR "let"; r :] (S LR "and") pel ""
                               [: `S LR "in" :];
                             `expr e "" [: :] :];
                       `HVbox [: `S LR "end"; k :] :] :]
      | <:expr< let module $m$ = $mb$ in $e$ >> ->
          fun curr next dg k ->
            if dg <> ";" then
              [: `HVbox
                    [: `HVbox [: :];
                       `module_binding
                          [: `S LR "let"; `S LR "module"; `S LR m :] mb [: :];
                       `S LR "in"; `expr e dg k :] :]
            else
              [: `BEbox
                    [: `module_binding
                          [: `S LR "begin let"; `S LR "module"; `S LR m :] mb
                          [: :];
                       `HVbox
                          [: `HVbox [: :]; `S LR "in"; `expr e dg [: :] :];
                       `HVbox [: `S LR "end"; k :] :] :]
      | <:expr< fun [ $list:pel$ ] >> as e ->
          fun curr next dg k ->
            let loc = MLast.loc_of_expr e in
            if not (List.mem dg ["|"; ";"]) then
              match pel with
              [ [] ->
                  [: `S LR "fun"; `S LR "_"; `S LR "->";
                     `raise_match_failure loc k :]
              | [(p, None, e)] ->
                  let (pl, e) = expr_fun_args e in
                  [: `BEbox
                        [: `HOVbox
                              [: `S LR "fun";
                                 list simple_patt [p :: pl] ""
                                   [: `S LR "->" :] :];
                           `expr e "" k :] :]
              | _ ->
                  [: `Vbox
                        [: `HVbox [: :]; `S LR "function";
                           `match_assoc_list loc pel "" k :] :] ]
            else
              match pel with
              [ [] ->
                  [: `S LR "(fun"; `S LR "_"; `S LR "->";
                     `raise_match_failure loc [: `S RO ")"; k :] :]
              | [(p, None, e)] ->
                  if is_irrefut_patt p then
                    let (pl, e) = expr_fun_args e in
                    [: `S LO "(";
                       `BEbox
                          [: `HOVbox
                                [: `S LR "fun";
                                   list simple_patt [p :: pl] ""
                                     [: `S LR "->" :] :];
                             `expr e "" [: `S RO ")"; k :] :] :]
                  else
                    [: `HVbox
                          [: `S LR "fun ["; `patt p "" [: `S LR "->" :] :];
                       `expr e "" [: `S LR "]"; k :] :]
              | _ ->
                  [: `Vbox
                        [: `HVbox [: :]; `S LR "begin function";
                           `match_assoc_list loc pel "" k;
                           `HVbox [: `S LR "end"; k :] :] :] ]
      | <:expr< match $e$ with [ $list:pel$ ] >> as ge ->
          fun curr next dg k ->
            let loc = MLast.loc_of_expr ge in
            if not (List.mem dg ["|"; ";"]) then
              [: `HVbox
                    [: `HVbox [: :];
                       `BEbox
                          [: `S LR "match"; `expr e "" [: :]; `S LR "with" :];
                       `match_assoc_list loc pel "" k :] :]
            else
              [: `HVbox
                    [: `HVbox [: :];
                       `BEbox
                          [: `S LR "begin match"; `expr e "" [: :];
                             `S LR "with" :];
                       `match_assoc_list loc pel "" [: :];
                       `HVbox [: `S LR "end"; k :] :] :]
      | <:expr< try $e$ with [ $list:pel$ ] >> as ge ->
          fun curr next dg k ->
            let loc = MLast.loc_of_expr ge in
            if not (List.mem dg ["|"; ";"]) then
              [: `HVbox
                    [: `HVbox [: :];
                       `BEbox
                          [: `S LR "try"; `expr e "" [: :]; `S LR "with" :];
                       `match_assoc_list loc pel "" k :] :]
            else
              [: `HVbox
                    [: `HVbox [: :];
                       `BEbox
                          [: `S LR "begin try"; `expr e "" [: :];
                             `S LR "with" :];
                       `match_assoc_list loc pel "" [: :];
                       `HVbox [: `S LR "end"; k :] :] :]
      | <:expr< if $_$ then () else raise (Assert_failure $_$) >> as e ->
          fun curr next dg k -> [: `next e dg k :]
      | <:expr< if $e1$ then $e2$ else $e3$ >> as e ->
          fun curr next dg k ->
            let eel_e =
              elseif e3 where rec elseif e =
                match e with
                [ <:expr< if $e1$ then $e2$ else $e3$ >> ->
                    let (eel, e) = elseif e3 in
                    ([(e1, e2) :: eel], e)
                | _ -> ([], e) ]
            in
            if not (List.mem dg ["else"]) then
              match eel_e with
              [ ([], <:expr< () >>) ->
                  [: `HOVbox [: `S LR "if"; `expr e1 "" [: `S LR "then" :] :];
                     `expr1 e2 dg k :]
              | (eel, <:expr< () >>) ->
                  let (eel, (e1f, e2f)) =
                    let r = List.rev eel in
                    (List.rev (List.tl r), List.hd r)
                  in
                  [: `HVbox
                        [: `HVbox [: :];
                           `HVbox
                              [: `HOVbox
                                    [: `S LR "if";
                                       `expr e1 "" [: `S LR "then" :] :];
                                 `expr1 e2 "else" [: :] :];
                           list
                             (fun (e1, e2) _ k ->
                                HVbox
                                  [: `HOVbox
                                        [: `S LR "else"; `S LR "if";
                                           `expr e1 "" [: `S LR "then" :] :];
                                     `expr1 e2 "else" k :])
                             eel "" [: :];
                           `HVbox
                              [: `HOVbox
                                    [: `S LR "else"; `S LR "if";
                                       `expr e1f "" [: `S LR "then" :] :];
                                 `expr1 e2f dg k :] :] :]
              | (eel, e) ->
                  [: `HVbox
                        [: `HVbox [: :];
                           `HVbox
                              [: `HOVbox
                                    [: `S LR "if";
                                       `expr e1 "" [: `S LR "then" :] :];
                                 `expr1 e2 "else" [: :] :];
                           list
                             (fun (e1, e2) _ k ->
                                HVbox
                                  [: `HOVbox
                                        [: `S LR "else"; `S LR "if";
                                           `expr e1 "" [: `S LR "then" :] :];
                                     `expr1 e2 "else" k :])
                             eel "" [: :];
                           `HVbox [: `S LR "else"; `expr1 e dg k :] :] :] ]
            else
              match eel_e with
              [ (_, <:expr< () >>) -> [: `next e "" k :]
              | (eel, e) ->
                  [: `HVbox
                        [: `HVbox [: :];
                           `HVbox
                              [: `HOVbox
                                    [: `S LR "if";
                                       `expr e1 "" [: `S LR "then" :] :];
                                 `expr1 e2 "" [: :] :];
                           list
                             (fun (e1, e2) _ k ->
                                HVbox
                                  [: `HOVbox
                                        [: `S LR "else"; `S LR "if";
                                           `expr e1 "" [: `S LR "then" :] :];
                                     `expr1 e2 "" [: :] :])
                             eel "" [: :];
                           `HVbox [: `S LR "else"; `expr1 e "" k :] :] :] ]
      | <:expr< for $i$ = $e1$ $to:d$ $e2$ do { $list:el$ } >> ->
          fun curr next dg k ->
            let d = if d then "to" else "downto" in
            [: `BEbox
                  [: `HOVbox
                        [: `S LR "for"; `S LR i; `S LR "=";
                           `expr e1 "" [: `S LR d :];
                           `expr e2 "" [: `S LR "do" :] :];
                     `HVbox
                        [: `HVbox [: :];
                           listws expr (S RO ";") el "" [: :] :];
                     `HVbox [: `S LR "done"; k :] :] :]
      | <:expr< while $e1$ do { $list:el$ } >> ->
          fun curr next dg k ->
            [: `BEbox
                  [: `BEbox
                        [: `S LR "while"; `expr e1 "" [: :]; `S LR "do" :];
                     `HVbox
                        [: `HVbox [: :];
                           listws expr (S RO ";") el "" [: :] :];
                     `HVbox [: `S LR "done"; k :] :] :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< ($list:el$) >> ->
          fun curr next dg k ->
            [: `HVbox [: :]; listws next (S RO ",") el "" k :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $x$.val := $y$ >> ->
          fun curr next dg k ->
            [: `next x "" [: `S LR ":=" :]; `expr y dg k :]
      | <:expr< $x$ := $y$ >> ->
          fun curr next dg k ->
            [: `next x "" [: `S LR "<-" :]; `expr y dg k :]
      | e -> fun curr next dg k -> [: `next e "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox [: `HVbox [: :]; x :];
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:("||" as f)$ $x$ $y$ >> ->
          fun curr next dg k -> [: `next x "" [: `S LR f :]; curr y "" k :]
      | <:expr< $lid:("or" as f)$ $x$ $y$ >> ->
          fun curr next dg k -> [: `next x "" [: `S LR f :]; curr y "" k :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox [: `HVbox [: :]; x :];
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:(("&&") as f)$ $x$ $y$ >> ->
          fun curr next dg k -> [: `next x "" [: `S LR f :]; curr y "" k :]
      | <:expr< $lid:(("&") as f)$ $x$ $y$ >> ->
          fun curr next dg k -> [: `next x "" [: `S LR f :]; curr y "" k :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:op$ $x$ $y$ >> as e ->
          fun curr next dg k ->
            match op with
            [ "=" | "<>" | "<" | "<." | "<=" | ">" | ">=" | "==" | "!=" ->
                [: curr x "" [: `S LR op :]; `next y "" k :]
            | _ -> [: `next e "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:op$ $x$ $y$ >> as e ->
          fun curr next dg k ->
            match op with
            [ "^" | "@" -> [: `next x "" [: `S LR op :]; curr y "" k :]
            | _ -> [: `next e "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< [$_$ :: $_$] >> as e ->
          fun curr next dg k ->
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
            [ None -> [: `next e "" k :]
            | Some x ->
                [: listws next (S LR "::") el "" [: `S LR "::" :];
                   `next x "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:op$ $x$ $y$ >> as e ->
          fun curr next dg k ->
            match op with
            [ "+" | "+." | "-" | "-." ->
                [: curr x "" [: `S LR op :]; `next y "" k :]
            | _ -> [: `next e "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:op$ $x$ $y$ >> as e ->
          fun curr next dg k ->
            match op with
            [ "*" | "/" | "*." | "/." | "land" | "lor" | "lxor" | "mod" ->
                [: curr x "" [: `S LR op :]; `next y "" k :]
            | _ -> [: `next e "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:op$ $x$ $y$ >> as e ->
          fun curr next dg k ->
            match op with
            [ "**" | "asr" | "lsl" | "lsr" ->
                [: `next x "" [: `S LR op :]; curr y "" k :]
            | _ -> [: `next e "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $lid:"~-"$ $x$ >> ->
          fun curr next dg k -> [: `S LR "-"; curr x "" k :]
      | <:expr< $lid:"~-."$ $x$ >> ->
          fun curr next dg k -> [: `S LR "-."; curr x "" k :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $int:x$ >> -> fun curr next dg k -> [: `S LR x; k :]
      | <:expr< $flo:x$ >> -> fun curr next dg k -> [: `S LR x; k :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = "apply"; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< [$_$ :: $_$] >> as e ->
          fun curr next dg k -> [: `next e "" k :]
      | <:expr< Pervasives.ref (Lazy.Delayed (fun () -> $x$)) >> ->
          fun curr next dg k -> [: `S LR "lazy"; `next x "" k :]
      | <:expr< if $e$ then () else raise (Assert_failure $_$) >> ->
          fun curr next dg k -> [: `S LR "assert"; `next e "" k :]
      | <:expr<  raise (Assert_failure $_$) >> ->
          fun curr next dg k -> [: `S LR "assert"; `S LR "false"; k :]
      | <:expr< $lid:n$ $x$ $y$ >> as e ->
          fun curr next dg k ->
            let loc = MLast.loc_of_expr e in
            if is_infix n then [: `next e "" k :]
            else [: curr <:expr< $lid:n$ $x$ >> "" [: :]; `next y "" k :]
      | <:expr< $x$ $y$ >> ->
          fun curr next dg k ->
            match get_expr_args x [y] with
            [ (_, [_]) -> [: curr x "" [: :]; `next y "" k :]
            | ((<:expr< $uid:_$ >> | <:expr< $_$ . $uid:_$ >> as a), al) ->
                [: curr a "" [: :];
                   `HOVbox
                      [: `S LO "(";
                         listws (fun x _ k -> HOVbox [: curr x "" k :])
                           (S RO ",") al "" [: `S RO ")"; k :] :] :]
            | _ -> [: curr x "" [: :]; `next y "" k :] ]
      | MLast.ExNew _ sl ->
          fun curr next dg k -> [: `S LR "new"; `class_longident sl "" k :]
      | MLast.ExXnd _ c e ->
          fun curr next dg k -> [: `S LR ("Xnd_" ^ c); `next e "" k :]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = "dot"; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $x$ . ( $y$ ) >> ->
          fun curr next dg k ->
            [: curr x "" [: :]; `S NO ".("; `expr y "" [: `S RO ")"; k :] :]
      | <:expr< $x$ . [ $y$ ] >> ->
          fun curr next dg k ->
            [: curr x "" [: :]; `S NO ".["; `expr y "" [: `S RO "]"; k :] :]
      | <:expr< $e$. val >> ->
          fun curr next dg k -> [: `S LO "!"; `next e "" k :]
      | <:expr< $e1$ . $e2$ >> ->
          fun curr next dg k ->
            [: curr e1 "" [: :]; `S NO "."; curr e2 "" k :]
      | e -> fun curr next dg k -> [: `next e "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< [$_$ :: $_$] >> as e ->
          fun curr next dg k ->
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
                [: `S LO "[";
                   listws expr (S RO ";") el "" [: `S RO "]"; k :] :]
            | Some x -> [: `next e "" k :] ]
      | e -> fun curr next dg k -> [: `next e dg k :] ]};
   {pr_label = "simple"; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:expr< $int:x$ >> ->
          fun curr next dg k ->
            if x.[0] = '-' then [: `S LO "("; `S LR x; `S RO ")"; k :]
            else [: `S LR x; k :]
      | <:expr< $flo:x$ >> ->
          fun curr next dg k ->
            if x.[0] = '-' then [: `S LO "("; `S LR x; `S RO ")"; k :]
            else [: `S LR x; k :]
      | <:expr< $str:s$ >> ->
          fun curr next dg k -> [: `S LR ("\"" ^ s ^ "\""); k :]
      | <:expr< $chr:c$ >> ->
          fun curr next dg k ->
            let c = if c = "'" then "\'" else c in
            [: `S LR ("'" ^ c ^ "'"); k :]
      | <:expr< $uid:s$ >> ->
          fun curr next dg k -> [: `S LR (conv_con s); k :]
      | <:expr< $lid:s$ >> ->
          fun curr next dg k -> [: `S LR (var_escaped s); k :]
      | <:expr< $e$ # $lab$ >> ->
          fun curr next dg k ->
            [: curr e "" [: :]; `S NO "#"; `label lab; k :]
      | <:expr< ` $i$ >> -> fun curr next dg k -> [: `S LR ("`" ^ i); k :]
      | <:expr< ~ $i$ : $lid:j$ >> when i = j ->
          fun curr next dg k -> [: `S LR ("~" ^ i); k :]
      | <:expr< ~ $i$ : $e$ >> ->
          fun curr next dg k -> [: `S LO ("~" ^ i ^ ":"); curr e "" k :]
      | <:expr< ? $i$ : $lid:j$ >> when i = j ->
          fun curr next dg k -> [: `S LR ("?" ^ i); k :]
      | <:expr< ? $i$ : $e$ >> ->
          fun curr next dg k -> [: `S LO ("?" ^ i ^ ":"); curr e "" k :]
      | <:expr< [| $list:el$ |] >> ->
          fun curr next dg k ->
            [: `S LR "[|"; listws expr (S RO ";") el "" [: `S LR "|]"; k :] :]
      | <:expr< { $list:fel$ } >> ->
          fun curr next dg k ->
            [: `S LO "{";
               listws
                 (fun (lab, e) dg k ->
                    HVbox [: `patt lab "" [: `S LR "=" :]; `expr1 e dg k :])
                 (S RO ";") fel "" [: `S RO "}"; k :] :]
      | <:expr< { ($e$) with $list:fel$ } >> ->
          fun curr next dg k ->
            [: `HVbox [: `S LO "{"; curr e "" [: `S LR "with" :] :];
               listws
                 (fun (lab, e) dg k ->
                    HVbox [: `patt lab "" [: `S LR "=" :]; `expr1 e dg k :])
                 (S RO ";") fel "" [: `S RO "}"; k :] :]
      | <:expr< ($e$ : $t$) >> ->
          fun curr next dg k ->
            [: `S LO "("; `expr e "" [: `S LR ":" :];
               `ctyp t "" [: `S RO ")"; k :] :]
      | <:expr< ($e$ : $t1$ :> $t2$) >> ->
          fun curr next dg k ->
            [: `S LO "("; `expr e "" [: `S LR ":" :];
               `ctyp t1 "" [: `S LR ":>" :]; `ctyp t2 "" [: `S RO ")"; k :] :]
      | <:expr< ($e$ :> $t2$) >> ->
          fun curr next dg k ->
            [: `S LO "("; `expr e "" [: `S LR ":>" :];
               `ctyp t2 "" [: `S RO ")"; k :] :]
      | MLast.ExOvr _ [] -> fun curr next dg k -> [: `S LR "{< >}"; k :]
      | MLast.ExOvr _ fel ->
          fun curr next dg k ->
            [: `S LR "{<";
               listws field_expr (S RO ";") fel dg [: `S LR ">}"; k :] :]
      | <:expr< do { $list:el$ } >> ->
          fun curr next dg k ->
            match el with
            [ [e] -> curr e dg k
            | _ ->
                [: `BEbox
                      [: `S LR "begin";
                         `HVbox
                            [: `HVbox [: :];
                               listws expr1 (S RO ";") el "" [: :] :];
                         `HVbox [: `S LR "end"; k :] :] :] ]
      | <:expr< $_$ $_$ >> | <:expr< $uid:_$ $_$ $_$ >> |
        <:expr< fun [ $list:_$ ] >> | <:expr< match $_$ with [ $list:_$ ] >> |
        <:expr< if $_$ then $_$ else $_$ >> |
        <:expr< try $_$ with [ $list:_$ ] >> |
        <:expr< let $rec:_$ $list:_$ in $_$ >> |
        <:expr< for $_$ = $_$ $to:_$ $_$ do { $list:_$ } >> |
        <:expr< while $_$ do { $list:_$ } >> | <:expr< ($list: _$) >> |
        <:expr< $_$ . $_$ >> | <:expr< $_$ . ( $_$ ) >> |
        <:expr< $_$ . [ $_$ ] >> | <:expr< $_$ := $_$ >> | MLast.ExNew _ _ |
        MLast.ExXnd _ _ _ as e ->
          fun curr next dg k ->
            [: `S LO "("; `expr e "" [: `HVbox [: `S RO ")"; k :] :] :]
      | e -> fun curr next dg k -> [: `next e "" k :] ]}];

pr_patt.pr_levels :=
  [{pr_label = "top"; pr_box _ x = HOVCbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< ($x$ as $lid:y$) >> ->
          fun curr next dg k ->
            [: curr x "" [: :]; `S LR "as"; `S LR (var_escaped y); k :]
      | <:patt< ($x$ as $y$) >> ->
          fun curr next dg k ->
            [: curr y "" [: :]; `S LR "as"; `next x "" k :]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox [: `HVbox [: :]; x :];
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< $x$ | $y$ >> ->
          fun curr next dg k -> [: curr x "" [: `S LR "|" :]; `next y "" k :]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVCbox [: `HVbox [: :]; x :];
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< ($list:pl$) >> ->
          fun curr next dg k ->
            [: `HVbox [: :]; listws next (S RO ",") pl "" k :]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = "patt1"; pr_box _ x = HOVbox [: `HVbox [: :]; x :];
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< $x$ .. $y$ >> ->
          fun curr next dg k -> [: curr x "" [: `S NO ".." :]; `next y "" k :]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVCbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< [$_$ :: $_$] >> as p ->
          fun curr next dg k ->
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
                [: `S LO "[";
                   listws patt (S RO ";") pl "" [: `S RO "]"; k :] :]
            | Some x ->
                [: `HVbox [: :]; listws next (S LR "::") (pl @ [x]) "" k :] ]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< [$_$ :: $_$] >> as p ->
          fun curr next dg k -> [: `next p "" k :]
      | <:patt< $x$ $y$ >> ->
          fun curr next dg k ->
            match get_patt_args x [y] with
            [ (_, [_]) -> [: curr x "" [: :]; `next y "" k :]
            | ((<:patt< $uid:_$ >> | <:patt< $_$ . $uid:_$ >> as a), al) ->
                [: curr a "" [: :];
                   `HOVbox
                      [: `S LO "(";
                         listws (fun x _ k -> HOVbox [: curr x "" k :])
                           (S RO ",") al "" [: `S RO ")"; k :] :] :]
            | _ -> [: curr x "" [: :]; `next y "" k :] ]
      | MLast.PaXnd _ c p ->
          fun curr next dg k -> [: `S LR ("Xnd_" ^ c); `next p "" k :]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = ""; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< $x$ . $y$ >> ->
          fun curr next dg k -> [: curr x "" [: :]; `S NO "."; `next y "" k :]
      | p -> fun curr next dg k -> [: `next p "" k :] ]};
   {pr_label = "simple"; pr_box _ x = HOVbox x;
    pr_rules =
      extfun Extfun.empty with
      [ <:patt< [| $list:pl$ |] >> ->
          fun curr next dg k ->
            [: `S LR "[|"; listws patt (S RO ";") pl "" [: `S LR "|]"; k :] :]
      | <:patt< { $list:fpl$ } >> ->
          fun curr next dg k ->
            [: `HVbox
                  [: `S LO "{";
                     listws
                       (fun (lab, p) _ k ->
                          HVbox
                            [: `patt lab "" [: `S LR "=" :]; `patt p "" k :])
                       (S RO ";") fpl "" [: `S RO "}"; k :] :] :]
      | <:patt< [$_$ :: $_$] >> as p ->
          fun curr next dg k ->
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
                [: `S LO "[";
                   listws patt (S RO ";") pl "" [: `S RO "]"; k :] :]
            | Some x ->
                [: `S LO "("; `patt p "" [: `HVbox [: `S RO ")"; k :] :] :] ]
      | <:patt< ($p$ : $ct$) >> ->
          fun curr next dg k ->
            [: `S LO "("; `patt p "" [: `S LR ":" :];
               `ctyp ct "" [: `S RO ")"; k :] :]
      | <:patt< $int:s$ >> -> fun curr next dg k -> [: `S LR s; k :]
      | <:patt< $flo:s$ >> -> fun curr next dg k -> [: `S LR s; k :]
      | <:patt< $str:s$ >> ->
          fun curr next dg k -> [: `S LR ("\"" ^ s ^ "\""); k :]
      | <:patt< $chr:c$ >> ->
          fun curr next dg k ->
            let c = if c = "'" then "\'" else c in
            [: `S LR ("'" ^ c ^ "'"); k :]
      | <:patt< $lid:i$ >> -> fun curr next dg k -> [: `id_var i; k :]
      | <:patt< $uid:i$ >> ->
          fun curr next dg k -> [: `S LR (conv_con i); k :]
      | <:patt< ` $i$ >> -> fun curr next dg k -> [: `S LR ("`" ^ i); k :]
      | <:patt< # $list:sl$ >> ->
          fun curr next dg k -> [: `S LO "#"; mod_ident sl dg k :]
      | <:patt< ~ $i$ : $lid:j$ >> when i = j ->
          fun curr next dg k -> [: `S LR ("~" ^ i); k :]
      | <:patt< ~ $i$ : $p$ >> ->
          fun curr next dg k ->
            [: `S LO ("~" ^ i ^ ":"); `simple_patt p "" k :]
      | <:patt< ? $i$ : $lid:j$ >> when i = j ->
          fun curr next dg k -> [: `S LR ("?" ^ i); k :]
      | <:patt< ? $i$ : $p$ >> ->
          fun curr next dg k ->
            [: `S LO ("?" ^ i ^ ":"); `simple_patt p "" k :]
      | <:patt< ? $i$ : ($lid:j$ = $e$) >> when i = j ->
          fun curr next dg k ->
            [: `S LO "?"; `S LO "("; `S LR j; `S LR "=";
               `expr e "" [: `S RO ")"; k :] :]
      | <:patt< ? $i$ : ($p$ = $e$) >> ->
          fun curr next dg k ->
            [: `S LO ("?" ^ i ^ ":"); `S LO "("; `patt p "" [: `S LR "=" :];
               `expr e "" [: `S RO ")"; k :] :]
      | <:patt< _ >> -> fun curr next dg k -> [: `S LR "_"; k :]
      | <:patt< $_$ $_$ >> | <:patt< ($_$ as $_$) >> | <:patt< $_$ | $_$ >> |
        <:patt< ($list:_$) >> | <:patt< $_$ .. $_$ >> |
        MLast.PaXnd _ _ _ as p ->
          fun curr next dg k ->
            [: `S LO "("; `patt p "" [: `HVbox [: `S RO ")"; k :] :] :]
      | p -> fun curr next dg k -> [: `next p "" k :] ]}];

value output_string_eval oc s =
  loop 0 where rec loop i =
    if i == String.length s then ()
    else if i == String.length s - 1 then output_char oc s.[i]
    else
      match (s.[i], s.[i + 1]) with
      [ ('\\', 'n') -> do { output_char oc '\n'; loop (i + 2) }
      | (c, _) -> do { output_char oc c; loop (i + 1) } ]
;

value maxl = ref 78;
value sep = ref None;

value copy_source ic oc first bp ep =
  match sep.val with
  [ Some str ->
      if first then ()
      else if ep == in_channel_length ic then output_string oc "\n"
      else output_string_eval oc str
  | None ->
      do {
        seek_in ic bp;
        for i = bp to pred ep do { output_char oc (input_char ic) }
      } ]
;

value copy_to_end ic oc first bp =
  copy_source ic oc first bp (in_channel_length ic)
;

value apply_printer printer ast =
  let oc =
    match Pcaml.output_file.val with
    [ Some f -> open_out_bin f
    | None -> stdout ]
  in
  let cleanup () =
    match Pcaml.output_file.val with
    [ Some _ -> close_out oc
    | None -> () ]
  in
  let pr_ch = output_char oc in
  let pr_str = output_string oc in
  let pr_nl () = output_char oc '\n' in
  if Pcaml.input_file.val <> "-" && Pcaml.input_file.val <> "" then do {
    let ic = open_in_bin Pcaml.input_file.val in
    try
      let (first, last_pos) =
        List.fold_left
          (fun (first, last_pos) (si, (bp, ep)) ->
             do {
               copy_source ic oc first last_pos bp;
               flush oc;
               print_pretty pr_ch pr_str pr_nl "" "" maxl.val (fun _ -> ())
                 (printer si "" [: :]);
               flush oc;
               (False, ep)
             })
          (True, 0) ast
      in
      do { copy_to_end ic oc first last_pos; flush oc }
    with x ->
      do { close_in ic; cleanup (); raise x };
    close_in ic;
    cleanup ()
  }
  else do {
    List.iter
      (fun (si, _) ->
         do {
           print_pretty pr_ch pr_str pr_nl "" "" maxl.val (fun _ -> ())
             (printer si "" [: :]);
           match sep.val with
           [ Some str -> output_string_eval oc str
           | None -> output_char oc '\n' ];
           flush oc
         })
      ast;
    cleanup ()
  }
;

Pcaml.print_interf.val := apply_printer sig_item;
Pcaml.print_implem.val := apply_printer str_item;

Pcaml.add_option "-l" (Arg.Int (fun x -> maxl.val := x))
  "<length>   Maximum line length for pretty printing.";

Pcaml.add_option "-no_ss" (Arg.Set no_ss)
  "       Do not print double semicolons.";

Pcaml.add_option "-sep" (Arg.String (fun x -> sep.val := Some x))
  "<string> Use this string between phrases instead of reading source.";
