(* camlp4r pa_extend.cmo q_MLast.cmo *)
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

open Stdpp;

value split_ext = ref False;

Pcaml.add_option "-split_ext" (Arg.Set split_ext)
  "Split EXTEND by functions to turn around a PowerPC problem.";

Pcaml.add_option "-split_gext" (Arg.Set split_ext)
  "Old name for the option -split_ext.";

type loc = (Lexing.position * Lexing.position);

type name 'e = { expr : 'e; tvar : string; loc : loc };

type styp =
  [ STlid of loc and string
  | STapp of loc and styp and styp
  | STquo of loc and string
  | STself of loc and string
  | STtyp of MLast.ctyp ]
;

type text 'e =
  [ TXmeta of loc and string and list (text 'e) and 'e and styp
  | TXlist of loc and bool and text 'e and option (text 'e)
  | TXnext of loc
  | TXnterm of loc and name 'e and option string
  | TXopt of loc and text 'e
  | TXrules of loc and list (list (text 'e) * 'e)
  | TXself of loc
  | TXtok of loc and string and 'e ]
;

type entry 'e 'p =
  { name : name 'e; pos : option 'e; levels : list (level 'e 'p) }
and level 'e 'p =
  { label : option string; assoc : option 'e; rules : list (rule 'e 'p) }
and rule 'e 'p = { prod : list (psymbol 'e 'p); action : option 'e }
and psymbol 'e 'p = { pattern : option 'p; symbol : symbol 'e 'p }
and symbol 'e 'p = { used : list string; text : text 'e; styp : styp }
;

type used = [ Unused | UsedScanned | UsedNotScanned ];

value mark_used modif ht n =
  try
    let rll = Hashtbl.find_all ht n in
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

value quotify = ref False;
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
    value loc =
        let nowhere =
          { (Lexing.dummy_pos) with Lexing.pos_lnum = 1; Lexing.pos_cnum = 0 } in
        (nowhere, nowhere);
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
    value mloc =
         <:expr< let nowhere =
          { (Lexing.dummy_pos) with Lexing.pos_lnum = 1; Lexing.pos_cnum = 0 } in
           (nowhere, nowhere) >>;
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

value mklistexp loc =
  loop True where rec loop top =
    fun
    [ [] -> <:expr< [] >>
    | [e1 :: el] ->
        let loc =
          if top then loc else (fst (MLast.loc_of_expr e1), snd loc)
        in
        <:expr< [$e1$ :: $loop False el$] >> ]
;

value mklistpat loc =
  loop True where rec loop top =
    fun
    [ [] -> <:patt< [] >>
    | [p1 :: pl] ->
        let loc =
          if top then loc else (fst (MLast.loc_of_patt p1), snd loc)
        in
        <:patt< [$p1$ :: $loop False pl$] >> ]
;

value rec expr_fa al =
  fun
  [ <:expr< $f$ $a$ >> -> expr_fa [a :: al] f
  | f -> (f, al) ]
;

value rec quot_expr e =
  let loc = MLast.loc_of_expr e in
  match e with
  [ <:expr< None >> -> <:expr< Qast.Option None >>
  | <:expr< Some $e$ >> -> <:expr< Qast.Option (Some $quot_expr e$) >>
  | <:expr< False >> -> <:expr< Qast.Bool False >>
  | <:expr< True >> -> <:expr< Qast.Bool True >>
  | <:expr< () >> -> e
  | <:expr< Qast.List $_$ >> -> e
  | <:expr< Qast.Option $_$ >> -> e
  | <:expr< Qast.Str $_$ >> -> e
  | <:expr< [] >> -> <:expr< Qast.List [] >>
  | <:expr< [$e$] >> -> <:expr< Qast.List [$quot_expr e$] >>
  | <:expr< [$e1$ :: $e2$] >> ->
      <:expr< Qast.Cons $quot_expr e1$  $quot_expr e2$ >>
  | <:expr< $_$ $_$ >> ->
      let (f, al) = expr_fa [] e in
      match f with
      [ <:expr< $uid:c$ >> ->
          let al = List.map quot_expr al in
          <:expr< Qast.Node $str:c$ $mklistexp loc al$ >>
      | <:expr< MLast.$uid:c$ >> ->
          let al = List.map quot_expr al in
          <:expr< Qast.Node $str:c$ $mklistexp loc al$ >>
      | <:expr< $uid:m$.$uid:c$ >> ->
          let al = List.map quot_expr al in
          <:expr< Qast.Node $str:m ^ "." ^ c$ $mklistexp loc al$ >>
      | <:expr< $lid:f$ >> ->
          let al = List.map quot_expr al in
          List.fold_left (fun f e -> <:expr< $f$ $e$ >>)
            <:expr< $lid:f$ >> al
      | _ -> e ]
  | <:expr< {$list:pel$} >> ->
      try
        let lel =
          List.map
            (fun (p, e) ->
               let lab =
                 match p with
                 [ <:patt< $lid:c$ >> -> <:expr< $str:c$ >>
                 | <:patt< $_$.$lid:c$ >> -> <:expr< $str:c$ >>
                 | _ -> raise Not_found ]
               in
               <:expr< ($lab$, $quot_expr e$) >>)
            pel
        in
        <:expr< Qast.Record $mklistexp loc lel$>>
      with
      [ Not_found -> e ]
  | <:expr< $lid:s$ >> ->
      if s = Stdpp.loc_name.val then <:expr< Qast.Loc >> else e
  | <:expr< MLast.$uid:s$ >> -> <:expr< Qast.Node $str:s$ [] >>
  | <:expr< $uid:m$.$uid:s$ >> -> <:expr< Qast.Node $str:m ^ "." ^ s$ [] >>
  | <:expr< $uid:s$ >> -> <:expr< Qast.Node $str:s$ [] >>
  | <:expr< $str:s$ >> -> <:expr< Qast.Str $str:s$ >>
  | <:expr< ($list:el$) >> ->
      let el = List.map quot_expr el in
      <:expr< Qast.Tuple $mklistexp loc el$ >>
  | <:expr< let $opt:r$ $list:pel$ in $e$ >> ->
      let pel = List.map (fun (p, e) -> (p, quot_expr e)) pel in
      <:expr< let $opt:r$ $list:pel$ in $quot_expr e$ >>
  | _ -> e ]
;

value symgen = "xx";

value pname_of_ptuple pl =
  List.fold_left
    (fun pname p ->
       match p with
       [ <:patt< $lid:s$ >> -> pname ^ s
       | _ -> pname ])
    "" pl
;

value quotify_action psl act =
  let e = quot_expr act in
  List.fold_left
    (fun e ps ->
       match ps.pattern with
       [ Some <:patt< ($list:pl$) >> ->
           let loc =
             let nowhere =
               { (Lexing.dummy_pos) with Lexing.pos_lnum = 1; Lexing.pos_cnum = 0 } in
               (nowhere, nowhere) in
           let pname = pname_of_ptuple pl in
           let (pl1, el1) =
             let (l, _) =
               List.fold_left
                 (fun (l, cnt) _ ->
                    ([symgen ^ string_of_int cnt :: l], cnt + 1))
                 ([], 1) pl
             in
             let l = List.rev l in
             (List.map (fun s -> <:patt< $lid:s$ >>) l,
              List.map (fun s -> <:expr< $lid:s$ >>) l)
           in
           <:expr<
              let ($list:pl$) =
                match $lid:pname$ with
                [ Qast.Tuple $mklistpat loc pl1$ -> ($list:el1$)
                | _ -> match () with [] ]
              in $e$ >>
       | _ -> e ])
    e psl
;

value rec make_ctyp styp tvar =
  match styp with
  [ STlid loc s -> <:ctyp< $lid:s$ >>
  | STapp loc t1 t2 -> <:ctyp< $make_ctyp t1 tvar$ $make_ctyp t2 tvar$ >>
  | STquo loc s -> <:ctyp< '$s$ >>
  | STself loc x ->
      if tvar = "" then
        Stdpp.raise_with_loc loc
          (Stream.Error ("'" ^ x ^  "' illegal in anonymous entry level"))
      else <:ctyp< '$tvar$ >>
  | STtyp t -> t ]
;

value rec make_expr gmod tvar =
  fun
  [ TXmeta loc n tl e t ->
      let el =
        List.fold_right
          (fun t el -> <:expr< [$make_expr gmod "" t$ :: $el$] >>)
          tl <:expr< [] >>
      in
      <:expr<
        Gramext.Smeta $str:n$ $el$ (Obj.repr ($e$ : $make_ctyp t tvar$)) >>
  | TXlist loc min t ts ->
      let txt = make_expr gmod "" t in
      match (min, ts) with
      [ (False, None) -> <:expr< Gramext.Slist0 $txt$ >>
      | (True, None) -> <:expr< Gramext.Slist1 $txt$ >>
      | (False, Some s) ->
          let x = make_expr gmod tvar s in
          <:expr< Gramext.Slist0sep $txt$ $x$ >>
      | (True, Some s) ->
          let x = make_expr gmod tvar s in
          <:expr< Gramext.Slist1sep $txt$ $x$ >> ]
  | TXnext loc -> <:expr< Gramext.Snext >>
  | TXnterm loc n lev ->
      match lev with
      [ Some lab ->
          <:expr<
             Gramext.Snterml
               ($uid:gmod$.Entry.obj ($n.expr$ : $uid:gmod$.Entry.e '$n.tvar$))
               $str:lab$ >>
      | None ->
          if n.tvar = tvar then <:expr< Gramext.Sself >>
          else
            <:expr<
               Gramext.Snterm
                 ($uid:gmod$.Entry.obj
                    ($n.expr$ : $uid:gmod$.Entry.e '$n.tvar$)) >> ]
  | TXopt loc t -> <:expr< Gramext.Sopt $make_expr gmod "" t$ >>
  | TXrules loc rl ->
      <:expr< Gramext.srules $make_expr_rules loc gmod rl ""$ >>
  | TXself loc -> <:expr< Gramext.Sself >>
  | TXtok loc s e -> <:expr< Gramext.Stoken ($str:s$, $e$) >> ]
and make_expr_rules loc gmod rl tvar =
  List.fold_left
    (fun txt (sl, ac) ->
       let sl =
         List.fold_right
           (fun t txt ->
              let x = make_expr gmod tvar t in
              <:expr< [$x$ :: $txt$] >>)
           sl <:expr< [] >>
       in
       <:expr< [($sl$, $ac$) :: $txt$] >>)
    <:expr< [] >> rl
;

value text_of_action loc psl rtvar act tvar =
  let locid = <:patt< $lid:Stdpp.loc_name.val$ >> in
  let act =
    match act with
    [ Some act -> if quotify.val then quotify_action psl act else act
    | None -> <:expr< () >> ]
  in
  let e = <:expr< fun [ ($locid$ : (Lexing.position * Lexing.position)) -> ($act$ : '$rtvar$) ] >> in
  let txt =
    List.fold_left
      (fun txt ps ->
         match ps.pattern with
         [ None -> <:expr< fun _ -> $txt$ >>
         | Some p ->
             let t = make_ctyp ps.symbol.styp tvar in
             let p =
               match p with
               [ <:patt< ($list:pl$) >> when quotify.val ->
                   <:patt< $lid:pname_of_ptuple pl$ >>
               | _ -> p ]
             in
             <:expr< fun ($p$ : $t$) -> $txt$ >> ])
      e psl
  in
  let txt =
    if meta_action.val then
      <:expr< Obj.magic $MetaAction.mexpr txt$ >>
    else txt
  in
  <:expr< Gramext.action $txt$ >>
;

value srules loc t rl tvar =
  List.map
    (fun r ->
       let sl = List.map (fun ps -> ps.symbol.text) r.prod in
       let ac = text_of_action loc r.prod t r.action tvar in
       (sl, ac))
    rl
;

value expr_of_delete_rule loc gmod n sl =
  let sl =
    List.fold_right
      (fun s e -> <:expr< [$make_expr gmod "" s.text$ :: $e$] >>) sl
      <:expr< [] >>
  in
  (<:expr< $n.expr$ >>, sl)
;

value rec ident_of_expr =
  fun
  [ <:expr< $lid:s$ >> -> s
  | <:expr< $uid:s$ >> -> s
  | <:expr< $e1$ . $e2$ >> -> ident_of_expr e1 ^ "__" ^ ident_of_expr e2
  | _ -> failwith "internal error in pa_extend" ]
;

value mk_name loc e = {expr = e; tvar = ident_of_expr e; loc = loc};

value slist loc min sep symb =
  let t =
    match sep with
    [ Some s -> Some s.text
    | None -> None ]
  in
  TXlist loc min symb.text t
;

value sstoken loc s =
  let n = mk_name loc <:expr< $lid:"a_" ^ s$ >> in
  TXnterm loc n None
;

value mk_psymbol p s t =
  let symb = {used = []; text = s; styp = t} in
  {pattern = Some p; symbol = symb}
;

value sslist loc min sep s =
  let rl =
    let r1 =
      let prod =
        let n = mk_name loc <:expr< a_list >> in
        [mk_psymbol <:patt< a >> (TXnterm loc n None) (STquo loc "a_list")]
      in
      let act = <:expr< a >> in
      {prod = prod; action = Some act}
    in
    let r2 =
      let prod =
        [mk_psymbol <:patt< a >> (slist loc min sep s)
           (STapp loc (STlid loc "list") s.styp)]
      in
      let act = <:expr< Qast.List a >> in
      {prod = prod; action = Some act}
    in
    [r1; r2]
  in
  let used =
    match sep with
    [ Some symb -> symb.used @ s.used
    | None -> s.used ]
  in
  let used = ["a_list" :: used] in
  let text = TXrules loc (srules loc "a_list" rl "") in
  let styp = STquo loc "a_list" in
  {used = used; text = text; styp = styp}
;

value ssopt loc s =
  let rl =
    let r1 =
      let prod =
        let n = mk_name loc <:expr< a_opt >> in
        [mk_psymbol <:patt< a >> (TXnterm loc n None) (STquo loc "a_opt")]
      in
      let act = <:expr< a >> in
      {prod = prod; action = Some act}
    in
    let r2 =
      let s =
        match s.text with
        [ TXtok loc "" <:expr< $str:_$ >> ->
            let rl =
              [{prod = [{pattern = Some <:patt< x >>; symbol = s}];
                action = Some <:expr< Qast.Str x >>}]
            in
            let t = new_type_var () in
            {used = []; text = TXrules loc (srules loc t rl "");
             styp = STquo loc t}
        | _ -> s ]
      in
      let prod =
        [mk_psymbol <:patt< a >> (TXopt loc s.text)
           (STapp loc (STlid loc "option") s.styp)]
      in
      let act = <:expr< Qast.Option a >> in
      {prod = prod; action = Some act}
    in
    [r1; r2]
  in
  let used = ["a_opt" :: s.used] in
  let text = TXrules loc (srules loc "a_opt" rl "") in
  let styp = STquo loc "a_opt" in
  {used = used; text = text; styp = styp}
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
           let rl = srules loc e.name.tvar level.rules e.name.tvar in
           let e = make_expr_rules loc gmod rl e.name.tvar in
           <:expr< [($lab$, $ass$, $e$) :: $txt$] >>
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
          let same_tvar e n = e.name.tvar = n.tvar in
          List.fold_right
            (fun e ll ->
               match e.name.expr with
               [ <:expr< $lid:_$ >> ->
                   if List.exists (same_tvar e) nl then ll
                   else if List.exists (same_tvar e) ll then ll
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

value zero_loc = {(Lexing.dummy_pos) with Lexing.pos_cnum = 0};

open Pcaml;
value symbol = Grammar.Entry.create gram "symbol";
value semi_sep =
  if syntax_name.val = "Scheme" then
    Grammar.Entry.of_parser gram "'/'" (parser [: `("", "/") :] -> ())
  else
    Grammar.Entry.of_parser gram "';'" (parser [: `("", ";") :] -> ())
;

EXTEND
  GLOBAL: expr symbol;
  expr: AFTER "top"
    [ [ "EXTEND"; e = extend_body; "END" -> e
      | "GEXTEND"; e = gextend_body; "END" -> e
      | "DELETE_RULE"; e = delete_rule_body; "END" -> e
      | "GDELETE_RULE"; e = gdelete_rule_body; "END" -> e ] ]
  ;
  extend_body:
    [ [ f = efunction; sl = OPT global;
        el = LIST1 [ e = entry; semi_sep -> e ] ->
          text_of_extend loc "Grammar" sl el f ] ]
  ;
  gextend_body:
    [ [ g = UIDENT; sl = OPT global; el = LIST1 [ e = entry; semi_sep -> e ] ->
          text_of_functorial_extend loc g sl el ] ]
  ;
  delete_rule_body:
    [ [ n = name; ":"; sl = LIST1 symbol SEP semi_sep ->
          let (e, b) = expr_of_delete_rule loc "Grammar" n sl in
          <:expr< Grammar.delete_rule $e$ $b$ >> ] ]
  ;
  gdelete_rule_body:
    [ [ g = UIDENT; n = name; ":"; sl = LIST1 symbol SEP semi_sep ->
          let (e, b) = expr_of_delete_rule loc g n sl in
          <:expr< $uid:g$.delete_rule $e$ $b$ >> ] ]
  ;
  efunction:
    [ [ UIDENT "FUNCTION"; ":"; f = qualid; semi_sep -> f
      | -> <:expr< Grammar.extend >> ] ]
  ;
  global:
    [ [ UIDENT "GLOBAL"; ":"; sl = LIST1 name; semi_sep -> sl ] ]
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
    [ [ psl = LIST0 psymbol SEP semi_sep; "->"; act = expr ->
          {prod = psl; action = Some act}
      | psl = LIST0 psymbol SEP semi_sep ->
          {prod = psl; action = None} ] ]
  ;
  psymbol:
    [ [ p = LIDENT; "="; s = symbol ->
          {pattern = Some <:patt< $lid:p$ >>; symbol = s}
      | i = LIDENT; lev = OPT [ UIDENT "LEVEL"; s = STRING -> s ] ->
          let name = mk_name loc <:expr< $lid:i$ >> in
          let text = TXnterm loc name lev in
          let styp = STquo loc i in
          let symb = {used = [i]; text = text; styp = styp} in
          {pattern = None; symbol = symb}
      | p = pattern; "="; s = symbol -> {pattern = Some p; symbol = s}
      | s = symbol -> {pattern = None; symbol = s} ] ]
  ;
  symbol:
    [ "top" NONA
      [ UIDENT "LIST0"; s = SELF;
        sep = OPT [ UIDENT "SEP"; t = symbol -> t ] ->
          if quotify.val then sslist loc False sep s
          else
            let used =
              match sep with
              [ Some symb -> symb.used @ s.used
              | None -> s.used ]
            in
            let styp = STapp loc (STlid loc "list") s.styp in
            let text = slist loc False sep s in
            {used = used; text = text; styp = styp}
      | UIDENT "LIST1"; s = SELF;
        sep = OPT [ UIDENT "SEP"; t = symbol -> t ] ->
          if quotify.val then sslist loc True sep s
          else
            let used =
              match sep with
              [ Some symb -> symb.used @ s.used
              | None -> s.used ]
            in
            let styp = STapp loc (STlid loc "list") s.styp in
            let text = slist loc True sep s in
            {used = used; text = text; styp = styp}
      | UIDENT "OPT"; s = SELF ->
          if quotify.val then ssopt loc s
          else
            let styp = STapp loc (STlid loc "option") s.styp in
            let text = TXopt loc s.text in
            {used = s.used; text = text; styp = styp} ]
    | [ UIDENT "SELF" ->
          {used = []; text = TXself loc; styp = STself loc "SELF"}
      | UIDENT "NEXT" ->
          {used = []; text = TXnext loc; styp = STself loc "NEXT"}
      | "["; rl = LIST0 rule SEP "|"; "]" ->
          let rl = retype_rule_list_without_patterns loc rl in
          let t = new_type_var () in
          {used = used_of_rule_list rl;
           text = TXrules loc (srules loc t rl "");
           styp = STquo loc t}
      | x = UIDENT ->
          let text =
            if quotify.val then sstoken loc x
            else TXtok loc x <:expr< "" >>
          in
          {used = []; text = text; styp = STlid loc "string"}
      | x = UIDENT; e = string ->
          let text = TXtok loc x e in
          {used = []; text = text; styp = STlid loc "string"}
      | e = string ->
          let text = TXtok loc "" e in
          {used = []; text = text; styp = STlid loc "string"}
      | i = UIDENT; "."; e = qualid;
        lev = OPT [ UIDENT "LEVEL"; s = STRING -> s ] ->
          let n = mk_name loc <:expr< $uid:i$ . $e$ >> in
          {used = [n.tvar]; text = TXnterm loc n lev;
           styp = STquo loc n.tvar}
      | n = name; lev = OPT [ UIDENT "LEVEL"; s = STRING -> s ] ->
          {used = [n.tvar]; text = TXnterm loc n lev;
           styp = STquo loc n.tvar}
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
          let shift = Reloc.shift_pos (String.length "$") (fst loc) in
          let e =
            try Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string i) with
            [ Exc_located (bp, ep) exc ->
                raise_with_loc (Reloc.adjust_loc shift (bp,ep)) exc ]
          in
          Pcaml.expr_reloc (fun (bp, ep) -> (Reloc.adjust_loc shift (bp,ep))) zero_loc e ] ]
  ;
END;

Pcaml.add_option "-quotify" (Arg.Set quotify)
  "Generate code for quotations";

Pcaml.add_option "-meta_action" (Arg.Set meta_action)
  "Undocumented";
