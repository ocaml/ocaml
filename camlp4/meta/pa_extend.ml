(* camlp4r pa_extend.cmo q_MLast.cmo *)
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

open Stdpp;

value split_ext = ref False;

Pcaml.add_option "-split_ext" (Arg.Set split_ext)
  "   Split EXTEND by functions to turn around a PowerPC problem.";

Pcaml.add_option "-split_gext" (Arg.Set split_ext)
  "  Old name for the option -split_ext.";

type name = { expr : MLast.expr; tvar : string; loc : (int * int) };

type entry 'e 'p 't =
  { name : name; pos : option 'e; levels : list (level 'e 'p 't) }
and level 'e 'p 't =
  { label : option string; assoc : option 'e; rules : list (rule 'e 'p 't) }
and rule 'e 'p 't = { prod : list (psymbol 'e 'p 't); action : option 'e }
and psymbol 'e 'p 't = { pattern : option 'p; symbol : symbol 'e 'p 't }
and symbol 'e 'p 't =
  { used : list name; text : string -> string -> 'e; styp : string -> 't }
;

type used = [ Unused | UsedScanned | UsedNotScanned ];

value mark_used modif ht n =
  try
    let rll = Hashtbl.find_all ht n.tvar in
    List.iter
      (fun (r, _) ->
         if r.val == Unused then do {
           r.val := UsedNotScanned; modif.val := True;
         }
         else ())
      rll
  with
  [ Not_found -> () ]
;

value rec mark_symbol modif ht symb =
  List.iter (fun e -> mark_used modif ht e) symb.used
;

value check_use nl el =
  let ht = Hashtbl.create 301 in
  let modif = ref False in
  do {
    List.iter
      (fun e ->
         let u =
           match e.name.expr with
           [ <:expr< $lid:_$ >> -> Unused
           | _ -> UsedNotScanned ]
         in
         Hashtbl.add ht e.name.tvar (ref u, e))
      el;
    List.iter
      (fun n ->
         try
           let rll = Hashtbl.find_all ht n.tvar in
           List.iter (fun (r, _) -> r.val := UsedNotScanned) rll
         with _ ->
           ())
      nl;
    modif.val := True;
    while modif.val do {
      modif.val := False;
      Hashtbl.iter
        (fun s (r, e) ->
           if r.val = UsedNotScanned then do {
             r.val := UsedScanned;
             List.iter
               (fun level ->
                  let rules = level.rules in
                  List.iter
                    (fun rule ->
                       List.iter (fun ps -> mark_symbol modif ht ps.symbol)
                         rule.prod)
                    rules)
               e.levels
           }
           else ())
        ht
    };
    Hashtbl.iter
      (fun s (r, e) ->
         if r.val = Unused then
           Pcaml.warning.val e.name.loc ("Unused local entry \"" ^ s ^ "\"")
         else ())
      ht;
  }
;

value locate n = let loc = n.loc in <:expr< $n.expr$ >>;

value new_type_var =
  let i = ref 0 in fun () -> do { incr i; "e__" ^ string_of_int i.val }
;

value used_of_rule_list rl =
  List.fold_left
    (fun nl r -> List.fold_left (fun nl s -> s.symbol.used @ nl) nl r.prod) []
    rl
;

value retype_rule_list_without_patterns loc rl =
  try
    List.map
      (fun
       [ {prod = [{pattern = None; symbol = s}]; action = None} ->
           {prod = [{pattern = Some <:patt< x >>; symbol = s}];
            action = Some <:expr< x >>}
       | {prod = []; action = Some _} as r -> r
       | _ -> raise Exit ])
      rl
  with
  [ Exit -> rl ]
;

value text_of_psymbol_list loc gmod psl tvar =
  List.fold_right
    (fun ps txt ->
       let x = ps.symbol.text gmod tvar in <:expr< [$x$ :: $txt$] >>)
    psl <:expr< [] >>
;

value meta_action = ref False;

module MetaAction =
  struct
    value not_impl f x =
      let desc =
        if Obj.is_block (Obj.repr x) then
          "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
        else "int_val = " ^ string_of_int (Obj.magic x)
      in
      failwith (f ^ ", not impl: " ^ desc)
    ;
    value loc = (0, 0);
    value rec mlist mf =
      fun
      [ [] -> <:expr< [] >>
      | [x :: l] -> <:expr< [ $mf x$ :: $mlist mf l$ ] >> ]
    ;
    value moption mf =
      fun
      [ None -> <:expr< None >>
      | Some x -> <:expr< Some $mf x$ >> ]
    ;
    value mbool =
      fun
      [ False -> <:expr< False >>
      | True -> <:expr< True >> ]
    ;
    value mloc = <:expr< (0, 0) >>;
    value rec mexpr =
      fun
      [ MLast.ExAcc loc e1 e2 ->
          <:expr< MLast.ExAcc $mloc$ $mexpr e1$ $mexpr e2$ >>
      | MLast.ExApp loc e1 e2 ->
          <:expr< MLast.ExApp $mloc$ $mexpr e1$ $mexpr e2$ >>
      | MLast.ExChr loc s -> <:expr< MLast.ExChr $mloc$ $str:s$ >>
      | MLast.ExFun loc pwel -> <:expr< MLast.ExFun $mloc$ $mlist mpwe pwel$ >>
      | MLast.ExIfe loc e1 e2 e3 ->
          <:expr< MLast.ExIfe $mloc$ $mexpr e1$ $mexpr e2$ $mexpr e3$ >>
      | MLast.ExInt loc s -> <:expr< MLast.ExInt $mloc$ $str:s$ >>
      | MLast.ExFlo loc s -> <:expr< MLast.ExFlo $mloc$ $str:s$ >>
      | MLast.ExLet loc rf pel e ->
          <:expr< MLast.ExLet $mloc$ $mbool rf$ $mlist mpe pel$ $mexpr e$ >>
      | MLast.ExLid loc s -> <:expr< MLast.ExLid $mloc$ $str:s$ >>
      | MLast.ExMat loc e pwel ->
          <:expr< MLast.ExMat $mloc$ $mexpr e$ $mlist mpwe pwel$ >>
      | MLast.ExRec loc pel eo ->
          <:expr< MLast.ExRec $mloc$ $mlist mpe pel$ $moption mexpr eo$ >>
      | MLast.ExSeq loc el -> <:expr< MLast.ExSeq $mloc$ $mlist mexpr el$ >>
      | MLast.ExSte loc e1 e2 ->
          <:expr< MLast.ExSte $mloc$ $mexpr e1$ $mexpr e2$ >>
      | MLast.ExStr loc s ->
          <:expr< MLast.ExStr $mloc$ $str:String.escaped s$ >>
      | MLast.ExTry loc e pwel ->
          <:expr< MLast.ExTry $mloc$ $mexpr e$ $mlist mpwe pwel$ >>
      | MLast.ExTup loc el -> <:expr< MLast.ExTup $mloc$ $mlist mexpr el$ >>
      | MLast.ExTyc loc e t ->
          <:expr< MLast.ExTyc $mloc$ $mexpr e$ $mctyp t$ >>
      | MLast.ExUid loc s -> <:expr< MLast.ExUid $mloc$ $str:s$ >>
      | x -> not_impl "mexpr" x ]
    and mpatt =
      fun
      [ MLast.PaAcc loc p1 p2 ->
          <:expr< MLast.PaAcc $mloc$ $mpatt p1$ $mpatt p2$ >>
      | MLast.PaAny loc -> <:expr< MLast.PaAny $mloc$ >>
      | MLast.PaApp loc p1 p2 ->
          <:expr< MLast.PaApp $mloc$ $mpatt p1$ $mpatt p2$ >>
      | MLast.PaInt loc s -> <:expr< MLast.PaInt $mloc$ $str:s$ >>
      | MLast.PaLid loc s -> <:expr< MLast.PaLid $mloc$ $str:s$ >>
      | MLast.PaOrp loc p1 p2 ->
          <:expr< MLast.PaOrp $mloc$ $mpatt p1$ $mpatt p2$ >>
      | MLast.PaStr loc s ->
          <:expr< MLast.PaStr $mloc$ $str:String.escaped s$ >>
      | MLast.PaTup loc pl -> <:expr< MLast.PaTup $mloc$ $mlist mpatt pl$ >>
      | MLast.PaTyc loc p t ->
          <:expr< MLast.PaTyc $mloc$ $mpatt p$ $mctyp t$ >>
      | MLast.PaUid loc s -> <:expr< MLast.PaUid $mloc$ $str:s$ >>
      | x -> not_impl "mpatt" x ]
    and mctyp =
      fun 
      [ MLast.TyAcc loc t1 t2 ->
          <:expr< MLast.TyAcc $mloc$ $mctyp t1$ $mctyp t2$ >>
      | MLast.TyApp loc t1 t2 ->
          <:expr< MLast.TyApp $mloc$ $mctyp t1$ $mctyp t2$ >>
      | MLast.TyLid loc s -> <:expr< MLast.TyLid $mloc$ $str:s$ >>
      | MLast.TyQuo loc s -> <:expr< MLast.TyQuo $mloc$ $str:s$ >>
      | MLast.TyTup loc tl -> <:expr< MLast.TyTup $mloc$ $mlist mctyp tl$ >>
      | MLast.TyUid loc s -> <:expr< MLast.TyUid $mloc$ $str:s$ >>
      | x -> not_impl "mctyp" x ]
    and mpe (p, e) = <:expr< ($mpatt p$, $mexpr e$) >>
    and mpwe (p, w, e) = <:expr< ($mpatt p$, $moption mexpr w$, $mexpr e$) >>
    ;
  end
;

value text_of_action loc psl rtvar act tvar =
  let locid = <:patt< $lid:Stdpp.loc_name.val$ >> in
  let act =
    match act with
    [ Some act -> act
    | None -> <:expr< () >> ]
  in
  let e = <:expr< fun [ ($locid$ : (int * int)) -> ($act$ : '$rtvar$) ] >> in
  let txt =
    List.fold_left
      (fun txt ps ->
         match ps.pattern with
         [ None -> <:expr< fun _ -> $txt$ >>
         | Some p ->
             let t = ps.symbol.styp tvar in
             <:expr< fun [ ($p$ : $t$) -> $txt$ ] >> ])
      e psl
  in
  let txt =
    if meta_action.val then
      <:expr< Obj.magic $MetaAction.mexpr txt$ >>
    else txt
  in
  <:expr< Gramext.action $txt$ >>
;

value text_of_rule_list loc gmod rtvar rl tvar =
  List.fold_left
    (fun txt r ->
       let sl = text_of_psymbol_list loc gmod r.prod tvar in
       let ac = text_of_action loc r.prod rtvar r.action tvar in
       <:expr< [($sl$, $ac$) :: $txt$] >>)
    <:expr< [] >> rl
;

value text_of_entry loc gmod e =
  let ent =
    let x = e.name in
    let loc = e.name.loc in
    <:expr< ($x.expr$ : $uid:gmod$.Entry.e '$x.tvar$) >>
  in
  let pos =
    match e.pos with
    [ Some pos -> <:expr< Some $pos$ >>
    | None -> <:expr< None >> ]
  in
  let txt =
    List.fold_right
      (fun level txt ->
         let lab =
           match level.label with
           [ Some lab -> <:expr< Some $str:lab$ >>
           | None -> <:expr< None >> ]
         in
         let ass =
           match level.assoc with
           [ Some ass -> <:expr< Some $ass$ >>
           | None -> <:expr< None >> ]
         in
         let txt =
           let rl =
             text_of_rule_list loc gmod e.name.tvar level.rules e.name.tvar
           in
           <:expr< [($lab$, $ass$, $rl$) :: $txt$] >>
         in
         txt)
      e.levels <:expr< [] >>
  in
  (ent, pos, txt)
;

value let_in_of_extend loc gmod functor_version gl el args =
  match gl with
  [ Some ([n1 :: _] as nl) ->
      do {
        check_use nl el;
        let ll =
          List.fold_right
            (fun e ll ->
               match e.name.expr with
               [ <:expr< $lid:_$ >> ->
                   if List.exists (fun n -> e.name.tvar = n.tvar) nl then ll
                   else [e.name :: ll]
               | _ -> ll ])
            el []
        in
        let globals =
          List.map
            (fun {expr = e; tvar = x; loc = loc} ->
               (<:patt< _ >>, <:expr< ($e$ : $uid:gmod$.Entry.e '$x$) >>))
            nl
        in
        let locals =
          List.map
            (fun {expr = e; tvar = x; loc = loc} ->
               let i =
                 match e with
                 [ <:expr< $lid:i$ >> -> i
                 | _ -> failwith "internal error in pa_extend" ]
               in
               (<:patt< $lid:i$ >>, <:expr<
                (grammar_entry_create $str:i$ : $uid:gmod$.Entry.e '$x$) >>))
            ll
        in
        let e =
          if ll = [] then args
          else if functor_version then
            <:expr<
            let grammar_entry_create = $uid:gmod$.Entry.create in
            let $list:locals$ in $args$ >>
          else
            <:expr<
            let grammar_entry_create s =
              $uid:gmod$.Entry.create ($uid:gmod$.of_entry $locate n1$) s
            in
            let $list:locals$ in $args$ >>
        in
        <:expr< let $list:globals$ in $e$ >>
      }
  | _ -> args ]
;

value text_of_extend loc gmod gl el f =
  if split_ext.val then
    let args =
      List.map
        (fun e ->
           let (ent, pos, txt) = text_of_entry e.name.loc gmod e in
           let ent = <:expr< $uid:gmod$.Entry.obj $ent$ >> in
           let e = <:expr< ($ent$, $pos$, $txt$) >> in
           <:expr< let aux () = $f$ [$e$] in aux () >>)
        el
    in
    let args = <:expr< do { $list:args$ } >> in
    let_in_of_extend loc gmod False gl el args
  else
    let args =
      List.fold_right
        (fun e el ->
           let (ent, pos, txt) = text_of_entry e.name.loc gmod e in
           let ent = <:expr< $uid:gmod$.Entry.obj $ent$ >> in
           let e = <:expr< ($ent$, $pos$, $txt$) >> in
           <:expr< [$e$ :: $el$] >>)
        el <:expr< [] >>
    in
    let args = let_in_of_extend loc gmod False gl el args in
    <:expr< $f$ $args$ >>
;

value text_of_functorial_extend loc gmod gl el =
  let args =
    let el =
      List.map
        (fun e ->
           let (ent, pos, txt) = text_of_entry e.name.loc gmod e in
           let e = <:expr< $uid:gmod$.extend $ent$ $pos$ $txt$ >> in
           if split_ext.val then <:expr< let aux () = $e$ in aux () >> else e)
        el
    in
    <:expr< do { $list:el$ } >>
  in
  let_in_of_extend loc gmod True gl el args
;

value expr_of_delete_rule loc gmod n sl =
  let sl =
    List.fold_right (fun s e -> <:expr< [$s.text gmod ""$ :: $e$] >>) sl
      <:expr< [] >>
  in
  (<:expr< $n.expr$ >>, sl)
;

value sself loc gmod n = <:expr< Gramext.Sself >>;
value snext loc gmod n = <:expr< Gramext.Snext >>;
value snterm loc n lev gmod tvar =
  match lev with
  [ Some lab ->
      <:expr< Gramext.Snterml
                ($uid:gmod$.Entry.obj
                   ($n.expr$ : $uid:gmod$.Entry.e '$n.tvar$))
                $str:lab$ >>
  | None ->
      if n.tvar = tvar then sself loc gmod tvar
      else
        <:expr<
           Gramext.Snterm
             ($uid:gmod$.Entry.obj
                ($n.expr$ : $uid:gmod$.Entry.e '$n.tvar$)) >> ]
;
value slist loc min sep symb gmod n =
  let txt = symb.text gmod "" in
  match (min, sep) with
  [ (False, None) -> <:expr< Gramext.Slist0 $txt$ >>
  | (True, None) -> <:expr< Gramext.Slist1 $txt$ >>
  | (False, Some s) ->
      let x = s.text gmod n in <:expr< Gramext.Slist0sep $txt$ $x$ >>
  | (True, Some s) ->
      let x = s.text gmod n in <:expr< Gramext.Slist1sep $txt$ $x$ >> ]
;
value sopt loc symb gmod n =
  let txt = symb.text gmod "" in <:expr< Gramext.Sopt $txt$ >>
;
value srules loc t rl gmod tvar =
  let e = text_of_rule_list loc gmod t rl "" in
  <:expr< Gramext.srules $e$ >>
;

value rec ident_of_expr =
  fun
  [ <:expr< $lid:s$ >> -> s
  | <:expr< $uid:s$ >> -> s
  | <:expr< $e1$ . $e2$ >> -> ident_of_expr e1 ^ "__" ^ ident_of_expr e2
  | _ -> failwith "internal error in pa_extend" ]
;

value mk_name loc e = {expr = e; tvar = ident_of_expr e; loc = loc};

open Pcaml;
value symbol = Grammar.Entry.create gram "symbol";

EXTEND
  GLOBAL: expr symbol;
  expr: AFTER "top"
    [ [ "EXTEND"; e = extend_body; "END" -> e
      | "GEXTEND"; e = gextend_body; "END" -> e
      | "DELETE_RULE"; e = delete_rule_body; "END" -> e
      | "GDELETE_RULE"; e = gdelete_rule_body; "END" -> e ] ]
  ;
  extend_body:
    [ [ f = efunction; sl = OPT global; el = LIST1 [ e = entry; ";" -> e ] ->
          text_of_extend loc "Grammar" sl el f ] ]
  ;
  gextend_body:
    [ [ g = UIDENT; sl = OPT global; el = LIST1 [ e = entry; ";" -> e ] ->
          text_of_functorial_extend loc g sl el ] ]
  ;
  delete_rule_body:
    [ [ n = name; ":"; sl = LIST1 symbol SEP ";" ->
          let (e, b) = expr_of_delete_rule loc "Grammar" n sl in
          <:expr< Grammar.delete_rule $e$ $b$ >> ] ]
  ;
  gdelete_rule_body:
    [ [ g = UIDENT; n = name; ":"; sl = LIST1 symbol SEP ";" ->
          let (e, b) = expr_of_delete_rule loc g n sl in
          <:expr< $uid:g$.delete_rule $e$ $b$ >> ] ]
  ;
  efunction:
    [ [ UIDENT "FUNCTION"; ":"; f = qualid; ";" -> f
      | -> <:expr< Grammar.extend >> ] ]
  ;
  global:
    [ [ UIDENT "GLOBAL"; ":"; sl = LIST1 name; ";" -> sl ] ]
  ;
  entry:
    [ [ n = name; ":"; pos = OPT position; ll = level_list ->
          {name = n; pos = pos; levels = ll} ] ]
  ;
  position:
    [ [ UIDENT "FIRST" -> <:expr< Gramext.First >>
      | UIDENT "LAST" -> <:expr< Gramext.Last >>
      | UIDENT "BEFORE"; n = string -> <:expr< Gramext.Before $n$ >>
      | UIDENT "AFTER"; n = string -> <:expr< Gramext.After $n$ >>
      | UIDENT "LEVEL"; n = string -> <:expr< Gramext.Level $n$ >> ] ]
  ;
  level_list:
    [ [ "["; ll = LIST0 level SEP "|"; "]" -> ll ] ]
  ;
  level:
    [ [ lab = OPT STRING; ass = OPT assoc; rules = rule_list ->
          {label = lab; assoc = ass; rules = rules} ] ]
  ;
  assoc:
    [ [ UIDENT "LEFTA" -> <:expr< Gramext.LeftA >>
      | UIDENT "RIGHTA" -> <:expr< Gramext.RightA >>
      | UIDENT "NONA" -> <:expr< Gramext.NonA >> ] ]
  ;
  rule_list:
    [ [ "["; "]" -> []
      | "["; rules = LIST1 rule SEP "|"; "]" -> 
          retype_rule_list_without_patterns loc rules ] ]
  ;
  rule:
    [ [ psl = LIST0 psymbol SEP ";"; "->"; act = expr ->
          {prod = psl; action = Some act}
      | psl = LIST0 psymbol SEP ";" ->
          {prod = psl; action = None} ] ]
  ;
  psymbol:
    [ [ p = LIDENT; "="; s = symbol ->
          {pattern = Some <:patt< $lid:p$ >>; symbol = s}
      | i = LIDENT; lev = OPT [ UIDENT "LEVEL"; s = STRING -> s ] ->
          let name = mk_name loc <:expr< $lid:i$ >> in
          let text = snterm loc name lev in
          let styp _ = <:ctyp< '$i$ >> in
          let symb = {used = [name]; text = text; styp = styp} in
          {pattern = None; symbol = symb}
      | p = pattern; "="; s = symbol -> {pattern = Some p; symbol = s}
      | s = symbol -> {pattern = None; symbol = s} ] ]
  ;
  symbol:
    [ "top" NONA
      [ UIDENT "LIST0"; s = SELF;
        sep = OPT [ UIDENT "SEP"; t = symbol -> t ] ->
          let used =
            match sep with
            [ Some symb -> symb.used @ s.used
            | None -> s.used ]
          in
          let styp n = let t = s.styp n in <:ctyp< list $t$ >> in
          {used = used; text = slist loc False sep s; styp = styp}
      | UIDENT "LIST1"; s = SELF;
        sep = OPT [ UIDENT "SEP"; t = symbol -> t ] ->
          let used =
            match sep with
            [ Some symb -> symb.used @ s.used
            | None -> s.used ]
          in
          let styp n = let t = s.styp n in <:ctyp< list $t$ >> in
          {used = used; text = slist loc True sep s; styp = styp}
      | UIDENT "OPT"; s = SELF ->
          let styp n = let t = s.styp n in <:ctyp< option $t$ >> in
          {used = s.used; text = sopt loc s; styp = styp} ]
    | [ UIDENT "SELF" ->
          let styp n =
            if n = "" then
              Stdpp.raise_with_loc loc
                (Stream.Error "'SELF' illegal in anonymous entry level")
            else <:ctyp< '$n$ >>
          in
          {used = []; text = sself loc; styp = styp}
      | UIDENT "NEXT" ->
          let styp n =
            if n = "" then
              Stdpp.raise_with_loc loc
                (Stream.Error "'NEXT' illegal in anonymous entry level")
            else <:ctyp< '$n$ >>
          in
          {used = []; text = snext loc; styp = styp}
      | "["; rl = LIST0 rule SEP "|"; "]" ->
          let rl = retype_rule_list_without_patterns loc rl in
          let t = new_type_var () in
          {used = used_of_rule_list rl; text = srules loc t rl;
           styp = fun _ -> <:ctyp< '$t$ >>}
      | x = UIDENT ->
          {used = [];
           text = fun _ _ -> <:expr< Gramext.Stoken ($str:x$, "") >>;
           styp = fun _ -> <:ctyp< string >>}
      | x = UIDENT; e = string ->
          {used = [];
           text = fun _ _ -> <:expr< Gramext.Stoken ($str:x$, $e$) >>;
           styp = fun _ -> <:ctyp< string >>}
      | e = string ->
          {used = []; text = fun _ _ -> <:expr< Gramext.Stoken ("", $e$) >>;
           styp = fun _ -> <:ctyp< string >>}
      | i = UIDENT; "."; e = qualid;
        lev = OPT [ UIDENT "LEVEL"; s = STRING -> s ] ->
          let n = mk_name loc <:expr< $uid:i$ . $e$ >> in
          {used = [n]; text = snterm loc n lev;
           styp = fun _ -> <:ctyp< '$n.tvar$ >>}
      | n = name; lev = OPT [ UIDENT "LEVEL"; s = STRING -> s ] ->
          {used = [n]; text = snterm loc n lev;
           styp = fun _ -> <:ctyp< '$n.tvar$ >>}
      | "("; s_t = SELF; ")" -> s_t ] ]
  ;
  pattern:
    [ [ i = LIDENT -> <:patt< $lid:i$ >>
      | "_" -> <:patt< _ >>
      | "("; p = SELF; ")" -> <:patt< $p$ >>
      | "("; p = SELF; ","; pl = patterns_comma; ")" ->
          <:patt< ( $list:[p :: pl]$ ) >> ] ]
  ;
  patterns_comma:
    [ [ pl = SELF; ","; p = pattern -> pl @ [p] ]
    | [ p = pattern -> [p] ] ]
  ;
  name:
    [ [ e = qualid -> mk_name loc e ] ]
  ;
  qualid:
    [ [ e1 = SELF; "."; e2 = SELF -> <:expr< $e1$ . $e2$ >> ]
    | [ i = UIDENT -> <:expr< $uid:i$ >>
      | i = LIDENT -> <:expr< $lid:i$ >> ] ]
  ;
  string:
    [ [ s = STRING -> <:expr< $str:s$ >>
      | i = ANTIQUOT ->
          let shift = fst loc + String.length "$" in
          let e =
            try Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string i) with
            [ Exc_located (bp, ep) exc ->
                raise_with_loc (shift + bp, shift + ep) exc ]
          in
          Pcaml.expr_reloc (fun (bp, ep) -> (shift + bp, shift + ep)) 0 e ] ]
  ;
END;

Pcaml.add_option "-meta_action" (Arg.Set meta_action)
  " Undocumented";
