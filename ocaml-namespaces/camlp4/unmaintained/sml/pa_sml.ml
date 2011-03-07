(* pa_r.cmo pa_rp.cmo pa_extend.cmo q_MLast.cmo pr_dump.cmo *)
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



open Stdpp;
open Pcaml;

value ocaml_records = ref False;

Pcaml.no_constructors_arity.val := True;

value lexer = Plexer.gmake ();

do {
  Grammar.Unsafe.gram_reinit gram lexer;
  Grammar.Unsafe.clear_entry interf;
  Grammar.Unsafe.clear_entry implem;
  Grammar.Unsafe.clear_entry top_phrase;
  Grammar.Unsafe.clear_entry use_file;
  Grammar.Unsafe.clear_entry module_type;
  Grammar.Unsafe.clear_entry module_expr;
  Grammar.Unsafe.clear_entry sig_item;
  Grammar.Unsafe.clear_entry str_item;
  Grammar.Unsafe.clear_entry expr;
  Grammar.Unsafe.clear_entry patt;
  Grammar.Unsafe.clear_entry ctyp;
  Grammar.Unsafe.clear_entry let_binding;
};

Pcaml.parse_interf.val := Grammar.Entry.parse interf;
Pcaml.parse_implem.val := Grammar.Entry.parse implem;

value not_impl loc s =
  raise_with_loc loc (Stream.Error ("not implemented feature [" ^ s ^ "]"))
;

type altern 'a 'b = [ Left of 'a | Right of 'b ];

value get_seq =
  fun
  [ <:expr< do { $list:el$ } >> -> el
  | e -> [e] ]
;

value choose_tvar tpl =
  let rec find_alpha v =
    let s = String.make 1 v in
    if List.mem_assoc s tpl then
      if v = 'z' then None else find_alpha (Char.chr (Char.code v + 1))
    else Some (String.make 1 v)
  in
  let rec make_n n =
    let v = "a" ^ string_of_int n in
    if List.mem_assoc v tpl then make_n (succ n) else v
  in
  match find_alpha 'a' with
  [ Some x -> x
  | None -> make_n 1 ]
;

value mklistexp loc last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some e -> e
        | None -> <:expr< [] >> ]
    | [e1 :: el] ->
        let loc = if top then loc else (fst (MLast.loc_of_expr e1), snd loc) in
        <:expr< [$e1$ :: $loop False el$] >> ]
;

value mklistpat loc last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some p -> p
        | None -> <:patt< [] >> ]
    | [p1 :: pl] ->
        let loc = if top then loc else (fst (MLast.loc_of_patt p1), snd loc) in
        <:patt< [$p1$ :: $loop False pl$] >> ]
;

value expr_of_patt p =
  let loc = MLast.loc_of_patt p in
  match p with
  [ <:patt< $lid:x$ >> -> <:expr< $lid:x$ >>
  | _ -> Stdpp.raise_with_loc loc (Stream.Error "identifier expected") ]
;

value apply_bind loc e bl =
  let rec loop e =
    fun
    [ [] -> e
    | [<:str_item< value $p1$ = $e1$ >> :: list] ->
        loop_let e [(p1, e1)] list
    | [<:str_item< value rec $p1$ = $e1$ >> :: list] ->
        loop_letrec e [(p1, e1)] list
    | [<:str_item< module $s$ = $me$ >> :: list] ->
        let e = <:expr< let module $s$ = $me$ in $e$ >> in
        loop e list
    | [si :: list] ->
        raise Exit ]
  and loop_let e pel =
    fun
    [ [<:str_item< value $p1$ = $e1$ >> :: list] ->
        loop_let e [(p1, e1) :: pel] list
    | list ->
        let e = <:expr< let $list:pel$ in $e$ >> in
        loop e list ]
  and loop_letrec e pel =
    fun
    [ [<:str_item< value rec $p1$ = $e1$ >> :: list] ->
        loop_letrec e [(p1, e1) :: pel] list
    | list ->
        let e = <:expr< let rec $list:pel$ in $e$ >> in
        loop e list ]
  in
  loop e (List.rev bl)
;

value make_local loc sl1 sl2 =
  try
    let pl =
      List.map
        (fun
         [ <:str_item< value $opt:_$ $p$ = $_$ >> -> p
         | _ -> raise Exit ])
        sl2
      in
    let e1 =
      match List.map expr_of_patt pl with
      [ [e] -> e
      | el -> <:expr< ($list:el$) >> ]
    in
    let p1 =
      match pl with
      [ [p] -> p
      | pl -> <:patt< ($list:pl$) >> ]
    in
    let e = apply_bind loc e1 sl2 in
    let e = apply_bind loc e sl1 in
    <:str_item< value $p1$ = $e$ >>
  with
  [ Exit ->
      do {
        Printf.eprintf "\
*** Warning: a 'local' statement will be defined global because of bindings
which cannot be defined as first class values (modules, exceptions, ...)\n";
        flush stderr;
        <:str_item< declare $list:sl1 @ sl2$ end >>
      } ]
;

value str_declare loc =
  fun
  [ [d] -> d
  | dl -> <:str_item< declare $list:dl$ end >> ]
;

value sig_declare loc =
  fun
  [ [d] -> d
  | dl -> <:sig_item< declare $list:dl$ end >> ]
;

value extract_label_types loc tn tal cdol =
  let (cdl, aux) =
    List.fold_right
      (fun (loc, c, tl, aux_opt) (cdl, aux) ->
         match aux_opt with
         [ Some anon_record_type ->
             let new_tn = tn ^ "_" ^ c in
             let loc = MLast.loc_of_ctyp anon_record_type in
             let aux_def = ((loc, new_tn), [], anon_record_type, []) in
             let tl = [<:ctyp< $lid:new_tn$ >>] in
             ([(loc, c, tl) :: cdl], [aux_def :: aux])
         | None -> ([(loc, c, tl) :: cdl], aux) ])
      cdol ([], [])
  in
  [((loc, tn), tal, <:ctyp< [ $list:cdl$ ] >>, []) :: aux]
;

value function_of_clause_list loc xl =
  let (fname, fname_loc, nbpat, l) =
    List.fold_left
      (fun (fname, fname_loc, nbpat, l) ((x1, loc), x2, x3, x4) ->
         let (fname, fname_loc, nbpat) =
           if fname = "" then (x1, loc, List.length x2)
           else if x1 <> fname then
             raise_with_loc loc
               (Stream.Error ("'" ^ fname ^ "' expected"))
           else if List.length x2 <> nbpat then
             raise_with_loc loc
               (Stream.Error "bad number of patterns in that clause")
           else (fname, fname_loc, nbpat)
         in
         let x4 =
           match x3 with
           [ Some t -> <:expr< ($x4$ : $t$) >>
           | _ -> x4 ]
         in
         let l = [(x2, x4) :: l] in
         (fname, fname_loc, nbpat, l))
      ("", loc, 0, []) xl
  in
  let l = List.rev l in
  let e =
    match l with
    [ [(pl, e)] ->
        List.fold_right (fun p e -> <:expr< fun $p$ -> $e$ >>) pl e
    | _ ->
        if nbpat = 1 then
          let pwel =
            List.map
              (fun (pl, e) -> (<:patt< $List.hd pl$ >>, None, e)) l
          in
          <:expr< fun [ $list:pwel$ ] >>
        else
          let sl =
            loop 0 where rec loop n =
              if n = nbpat then []
              else ["a" ^ string_of_int (n + 1) :: loop (n + 1)]
          in
          let e =
            let el = List.map (fun s -> <:expr< $lid:s$ >>) sl in
            let pwel =
              List.map
                (fun (pl, e) -> (<:patt< ($list:pl$) >>, None, e)) l
            in
            <:expr< match ($list:el$) with [ $list:pwel$ ] >>
          in
          List.fold_right (fun s e -> <:expr< fun $lid:s$ -> $e$ >>) sl e ]
  in
  (let loc = fname_loc in <:patt< $lid:fname$ >>, e)
;

value record_expr loc x1 =
  if ocaml_records.val then <:expr< { $list:x1$ } >>
  else
    let list1 =
      List.map
        (fun (l, v) ->
           let id =
             match l with
             [ <:patt< $lid:l$ >> -> l
             | _ -> "" ]
           in
           let loc = MLast.loc_of_expr v in
           <:class_str_item< value $id$ = $v$ >>)
        x1
    in
    let list2 =
      List.map
        (fun (l, v) ->
           let id =
             match l with
             [ <:patt< $lid:l$ >> -> l
             | _ -> "" ]
           in
           let loc = MLast.loc_of_patt l in
           <:class_str_item< method $id$ = $lid:id$ >>)
        x1
    in
    <:expr<
      let module M =
        struct
          class a = object $list:list1 @ list2$ end; 
        end
      in
      new M.a
    >>
;

value record_match_assoc loc lpl e =
  if ocaml_records.val then (<:patt< { $list:lpl$ } >>, e)
  else
    let pl = List.map (fun (_, p) -> p) lpl in
    let e =
      let el =
        List.map
          (fun (l, _) ->
             let s =
               match l with
               [ <:patt< $lid:l$ >> -> l
               | _ -> "" ]
             in
             let loc = MLast.loc_of_patt l in
             <:expr< v # $lid:s$ >>)
          lpl
      in
      let loc = MLast.loc_of_expr e in
      <:expr< let v = $e$ in ($list:el$) >>
    in
    let p = <:patt< ($list:pl$) >> in
    (p, e)
;

value op =
  Grammar.Entry.of_parser gram "op"
    (parser [: `("", "op"); `(_, x) :] -> x)
;
lexer.Token.tok_using ("", "op");

value special x =
  if String.length x >= 2 then
    match x.[0] with
    [ '+' | '<' | '^' -> True
    | _ -> False ]
  else False
;

value idd =
  let p =
    parser
    [ [: `("LIDENT", x) :] -> x
    | [: `("UIDENT", x) :] -> x
    | [: `("", "op"); `(_, x) :] -> x
    | [: `("", x) when special x :] -> x ]
  in
  Grammar.Entry.of_parser Pcaml.gram "ID" p
;

value uncap s = String.uncapitalize s;

EXTEND
  GLOBAL: implem interf top_phrase use_file sig_item str_item ctyp patt expr
    module_type module_expr;

  implem:
    [ [ x = interdec; EOI -> x ] ]
  ;
  interf:
    [ [ x = LIST1 [ s = sig_item; OPT ";" -> (s, loc) ] -> (x, False) ] ]
  ;
  top_phrase:
    [ [ ph = phrase; ";" -> Some ph
      | EOI -> None ] ]
  ;
  use_file:
    [ [ l = LIST0 phrase; EOI -> (l, False) ] ]
  ;
  phrase:
    [ [ x = str_item -> x
      | x = expr -> <:str_item< $exp:x$ >>
      | "#"; n = LIDENT; dp = dir_param -> MLast.StDir loc n dp ] ]
  ;
  dir_param:
    [ [ -> None
      | e = expr -> Some e ] ]
  ;
  sdecs:
    [ [ x = sdec; l = sdecs -> [x :: l]
      | ";"; l = sdecs -> l
      | -> [] ] ]
  ;

  fsigb: [ [ -> not_impl loc "fsigb" ] ];
  fsigconstraint_op: [ [ -> not_impl loc "fsigconstraint_op" ] ];
  fct_exp: [ [ -> not_impl loc "fct_exp" ] ];
  exp_pa: [ [ -> not_impl loc "exp_pa" ] ];
  rvb: [ [ -> not_impl loc "rvb" ] ];
  tyvarseq: [ [ -> not_impl loc "tyvarseq" ] ];

  tyvar_pc:
    [ [ "'"; x1 = LIDENT -> [(x1, (False, False))]
      | "'"; x1 = LIDENT; ","; l = tyvar_pc -> [(x1, (False, False)) :: l] ] ]
  ;
  id:
    [ [ x1 = idd -> x1
      | "*" -> "*" ] ]
  ;
  ident:
    [ [ x1 = idd -> x1
      | "*" -> "*"
      | "=" -> "="
      | "<" -> "<"
      | ">" -> ">"
      | "<=" -> "<="
      | ">=" -> ">="
      | "^" -> "^" ] ]
  ;
  op_op:
    [ [ x1 = op -> not_impl loc "op_op 1"
      | -> () ] ]
  ;
  qid:
    [ [ x1 = idd; "."; x2 = qid -> <:module_expr< $uid:x1$ . $x2$ >>
      | x1 = idd -> <:module_expr< $uid:x1$ >>
      | x1 = "*" -> <:module_expr< $uid:x1$ >>
      | x1 = "=" -> <:module_expr< $uid:x1$ >> ] ]
  ;
  eqid:
    [ [ x1 = UIDENT; "."; x2 = eqid -> <:expr< $uid:x1$ . $x2$ >>
      | x1 = UIDENT -> <:expr< $uid:x1$ >>
      | x1 = idd -> <:expr< $lid:x1$ >>
      | x1 = "*" -> <:expr< $lid:x1$ >>
      | x1 = "=" -> <:expr< $lid:x1$ >> ] ]
  ;
  sqid:
    [ [ x1 = idd; "."; x2 = sqid -> [x1 :: x2]
      | x1 = idd -> [x1]
      | x1 = "*" -> [x1]
      | x1 = "=" -> [x1] ] ]
  ;
  tycon:
    [ [ LIDENT "real" -> <:ctyp< float >>
      | x1 = idd; "."; x2 = tycon ->
          let r = <:ctyp< $uid:x1$ . $x2$ >> in
          loop r where rec loop =
            fun
            [ <:ctyp< $a$ . ($b$ . $c$) >> -> <:ctyp< $a$ . $b$ . $loop c$ >>
            | x -> x ]
      | x1 = idd -> <:ctyp< $lid:uncap x1$ >> ] ]
  ;
  selector:
    [ [ x1 = id -> x1
      | x1 = INT -> not_impl loc "selector 1" ] ]
  ;
  tlabel:
    [ [ x1 = selector; ":"; x2 = ctyp -> (loc, x1, False, x2) ] ]
  ;
  tuple_ty:
    [ [ x1 = ctyp LEVEL "ty'"; "*"; x2 = tuple_ty -> [x1 :: x2]
      | x1 = ctyp LEVEL "ty'" -> [x1] ] ]
  ;
  ctyp:
    [ RIGHTA
      [ x1 = ctyp; "->"; x2 = ctyp -> <:ctyp< $x1$ -> $x2$ >> ]
    | [ x1 = ctyp; "*"; x2 = tuple_ty -> <:ctyp< ($list:[x1 :: x2]$) >> ]
    | "ty'"
      [ "'"; x1 = LIDENT -> <:ctyp< '$x1$ >>
      | "'"; "'"; x1 = LIDENT -> <:ctyp< '$x1$ >>
      | "{"; x1 = LIST1 tlabel SEP ","; "}" ->
          if ocaml_records.val then <:ctyp< { $list:x1$ } >>
          else
            let list = List.map (fun (_, l, _, t) -> (l, t)) x1 in
            <:ctyp< < $list:list$ > >>
      | "{"; "}" -> not_impl loc "ty' 3"
      | "("; x1 = ctyp; ","; x2 = LIST1 ctyp SEP ","; ")"; x3 = tycon ->
          List.fold_left (fun t1 t2 -> <:ctyp< $t1$ $t2$ >>) x3 [x1 :: x2]
      | "("; x1 = ctyp; ")" -> x1
      | x1 = ctyp; x2 = tycon -> <:ctyp< $x2$ $x1$ >>
      | x1 = tycon -> x1 ] ]
  ;
  rule:
    [ [ x1 = patt; "=>"; x2 = expr -> (x1, None, x2) ] ]
  ;
  elabel:
    [ [ x1 = selector; "="; x2 = expr -> (<:patt< $lid:x1$ >>, x2) ] ]
  ;
  exp_ps:
    [ [ x1 = expr -> x1
      | x1 = expr; ";"; x2 = exp_ps ->
          <:expr< do { $list:[x1 :: get_seq x2]$ } >> ] ]
  ;
  expr:
    [ [ "if"; x1 = expr; "then"; x2 = expr; "else"; x3 = expr ->
          <:expr< if $x1$ then $x2$ else $x3$ >>
      | "fn"; x1 = LIST1 rule SEP "|" -> <:expr< fun [$list:x1$] >>
      | "case"; x1 = expr; "of"; x2 = LIST1 rule SEP "|" ->
          <:expr< match $x1$ with [$list:x2$] >>
      | "while"; x1 = expr; "do"; x2 = expr ->
          <:expr< while $x1$ do { $x2$ } >>
      | x1 = expr; "handle"; x2 = LIST1 rule SEP "|" ->
          <:expr< try $x1$ with [$list:x2$] >> ]
    | RIGHTA
      [ "raise"; x1 = expr -> <:expr< raise $x1$ >> ]
    | [ e1 = expr; ":="; e2 = expr -> <:expr< $e1$.val := $e2$ >> ]
    | LEFTA
      [ x1 = expr; "orelse"; x2 = expr -> <:expr< $x1$ || $x2$ >> ]
    | LEFTA
      [ x1 = expr; "andalso"; x2 = expr -> <:expr< $x1$ && $x2$ >> ]
    | LEFTA
      [ x1 = expr; ":"; x2 = ctyp -> <:expr< ($x1$ : $x2$) >> ]
    | "4" NONA
      [ x1 = expr; "<"; x2 = expr -> <:expr< $x1$ < $x2$ >>
      | x1 = expr; ">"; x2 = expr -> <:expr< $x1$ > $x2$ >>
      | x1 = expr; "<>"; x2 = expr -> <:expr< $x1$ <> $x2$ >>
      | x1 = expr; "="; x2 = expr -> <:expr< $x1$ = $x2$ >>
      | x1 = expr; ">="; x2 = expr -> <:expr< $x1$ >= $x2$ >>
      | x1 = expr; "<="; x2 = expr -> <:expr< $x1$ <= $x2$ >> ]
    | RIGHTA
      [ x1 = expr; "^"; x2 = expr -> <:expr< $x1$ ^ $x2$ >>
      | x1 = expr; "@"; x2 = expr -> <:expr< $x1$ @ $x2$ >>
      | x1 = expr; "o"; x2 = expr -> <:expr< ooo $x1$ $x2$ >> ]
    | "5" RIGHTA
      [ x1 = expr; "::"; x2 = expr -> <:expr< [$x1$ :: $x2$] >> ]
    | "6" LEFTA
      [ x1 = expr; "+"; x2 = expr -> <:expr< $x1$ + $x2$ >>
      | x1 = expr; "-"; x2 = expr -> <:expr< $x1$ - $x2$ >> ]
    | "7" LEFTA
      [ x1 = expr; "*"; x2 = expr -> <:expr< $x1$ * $x2$ >>
      | x1 = expr; "/"; x2 = expr -> <:expr< $x1$ / $x2$ >>
      | x1 = expr; "div"; x2 = expr -> <:expr< $x1$ / $x2$ >>
      | x1 = expr; "mod"; x2 = expr -> <:expr< $x1$ mod $x2$ >> ]
    | LEFTA
      [ x1 = expr; x2 = expr -> <:expr< $x1$ $x2$ >> ]
    | [ "#"; x1 = STRING -> <:expr< $chr:x1$ >>
      | "#"; x1 = selector; x2 = expr ->
          if ocaml_records.val then <:expr< $x2$ . $lid:x1$ >>
          else <:expr< $x2$ # $lid:x1$ >>
      | x1 = expr; "ocaml_record_access"; x2 = expr -> <:expr< $x1$ . $x2$ >> ]
    | [ "!"; x1 = expr -> <:expr< $x1$ . val >>
      | "~"; x1 = expr -> <:expr< - $x1$ >> ]
    | [ x1 = LIDENT ->
          match x1 with
          [ "true" | "false" -> <:expr< $uid:String.capitalize x1$ >>
          | "nil" -> <:expr< [] >>
          | _ -> <:expr< $lid:x1$ >> ]
      | x1 = UIDENT -> <:expr< $uid:x1$ >>
      | x1 = UIDENT; "."; x2 = eqid -> <:expr< $uid:x1$ . $x2$ >>
      | x1 = INT -> <:expr< $int:x1$ >>
      | x1 = FLOAT -> <:expr< $flo:x1$ >>
      | x1 = STRING -> <:expr< $str:x1$ >>
      | "~"; x1 = INT -> <:expr< $int:"-"^x1$ >>
      | i = op ->
          if i = "::" then <:expr< fun (x, y) -> [x :: y] >>
          else <:expr< fun (x, y) -> $lid:i$ x y >>
      | "let"; x1 = ldecs; "in"; x2 = exp_ps; "end" ->
          List.fold_right
            (fun pel x2 ->
               let loc =
                 match pel with
                 [ [(p, _) :: _] ->
                     (fst (MLast.loc_of_patt p), snd (MLast.loc_of_expr x2))
                 | _ -> loc ]
               in
               match pel with
               [ [(_, <:expr< fun [$list:_$] >>) :: _] ->
                   <:expr< let rec $list:pel$ in $x2$ >>
               | _ ->
                   let pel =
                     List.map
                       (fun (p, e) ->
                          match p with
                          [ <:patt< { $list:lpl$ } >> ->
                              record_match_assoc (MLast.loc_of_patt p) lpl e
                          | _ -> (p, e) ])
                       pel
                   in
                   <:expr< let $list:pel$ in $x2$ >> ])
            x1 x2
      | "{"; x1 = LIST1 elabel SEP ","; "}" -> record_expr loc x1
      | "["; "]" -> <:expr< [] >>
      | "["; x1 = expr; "]" -> <:expr< [$x1$] >>
      | "["; x1 = expr; ","; x2 = LIST1 SELF SEP ","; "]" ->
          mklistexp loc None [x1 :: x2]
      | "("; ")" -> <:expr< () >>
      | "("; x1 = expr; ","; x2 = LIST1 SELF SEP ","; ")" ->
          <:expr< ($list:[x1::x2]$) >>
      | "("; x1 = expr; ";"; x2 = LIST1 SELF SEP ";"; ")" ->
          <:expr< do { $list:[x1::x2]$ } >>
      | "("; x1 = expr; ")" -> x1 ] ]
  ;
  fixity:
    [ [ "infix" -> ("infix", None)
      | "infix"; x1 = INT -> not_impl loc "fixity 2"
      | "infixr" -> not_impl loc "fixity 3"
      | "infixr"; x1 = INT -> ("infixr", Some x1)
      | "nonfix" -> not_impl loc "fixity 5" ] ]
  ;
  patt:
    [ [ x1 = patt; "as"; x2 = patt -> <:patt< ($x1$ as $x2$) >> ]
    | LEFTA
      [ x1 = patt; ":"; x2 = ctyp -> <:patt< ($x1$ : $x2$) >> ]
    | RIGHTA
      [ x1 = patt; "::"; x2 = patt -> <:patt< [$x1$ :: $x2$] >> ]
    | [ x1 = patt; x2 = patt ->
          match x1 with
          [ <:patt< ref >> -> <:patt< {contents = $x2$} >>
          | _ -> <:patt< $x1$ $x2$ >> ] ]
    | "apat"
      [ x1 = patt; "."; x2 = patt -> <:patt< $x1$ . $x2$ >>
      | x1 = INT -> <:patt< $int:x1$ >>
      | x1 = UIDENT -> <:patt< $uid:x1$ >>
      | x1 = STRING -> <:patt< $str:x1$ >>
      | "#"; x1 = STRING -> <:patt< $chr:x1$ >>
      | "~"; x1 = INT -> <:patt< $int:"-"^x1$ >>
      | LIDENT "nil" -> <:patt< [] >>
      | LIDENT "false" -> <:patt< False >>
      | LIDENT "true" -> <:patt< True >>
      | x1 = id -> <:patt< $lid:x1$ >>
      | x1 = op -> <:patt< $lid:x1$ >>
      | "_" -> <:patt< _ >>
      | "["; "]" -> <:patt< [] >>
      | "["; x1 = patt; "]" -> <:patt< [$x1$] >>
      | "["; x1 = patt; ","; x2 = LIST1 SELF SEP ","; "]" ->
          mklistpat loc None [x1 :: x2]
      | "{"; x1 = LIST1 plabel SEP ","; "}" -> <:patt< {$list:x1$} >>
      | "("; ")" -> <:patt< () >>
      | "("; x1 = patt; ","; x2 = LIST1 SELF SEP ","; ")" ->
          <:patt< ($list:[x1::x2]$) >>
      | "("; x1 = patt; ")" -> x1 ] ]
  ;
  plabel:
    [ [ x1 = selector; "="; x2 = patt -> (<:patt< $lid:x1$ >>, x2)
      | x1 = selector -> (<:patt< $lid:x1$ >>, <:patt< $lid:x1$ >>) ] ]
  ;
  vb:
    [ [ "lazy"; x1 = patt; "="; x2 = expr -> not_impl loc "vb 1"
      | x1 = patt; "="; x2 = expr -> (x1, x2) ] ]
  ;
  constrain:
    [ [ -> None
      | ":"; x1 = ctyp -> Some x1 ] ]
  ;
  fb:
    [ [ xl = LIST1 clause SEP "|" -> function_of_clause_list loc xl
      | "lazy"; x1 = LIST1 clause SEP "|" -> not_impl loc "fb 2" ] ]
  ;
  clause:
    [ [ x1 = patt LEVEL "apat"; x2 = LIST1 (patt LEVEL "apat");
        x3 = constrain; "="; x4 = expr ->
          let x1 =
            match x1 with
            [ <:patt< $lid:id$ >> -> (id, MLast.loc_of_patt x1)
            | _ -> not_impl loc "clause 1" ]
          in
          (x1, x2, x3, x4) ] ]
  ;
  tb:
    [ [ x1 = tyvars; x2 = idd; "="; x3 = ctyp ->
          ((loc, uncap x2), x1, x3, [])
      | x1 = tyvars; x2 = idd; "="; x3 = ctyp; "=="; x4 = dbrhs ->
          let x4 = List.map (fun (loc, c, tl, _) -> (loc, c, tl)) x4 in
          ((loc, uncap x2), x1, <:ctyp< $x3$ == [ $list:x4$ ] >>, []) ] ]
  ;
  tyvars:
    [ [ "'"; x1 = LIDENT -> [(x1, (False, False))]
      | "("; x1 = tyvar_pc; ")" -> x1
      | -> [] ] ]
  ;
  db1:
    [ [ x1 = tyvars; x2 = ident; "="; x3 = dbrhs ->
          let x2 = uncap x2 in
          extract_label_types loc x2 x1 x3
      | "lazy"; x1 = tyvars; x2 = ident; "="; x3 = dbrhs ->
          not_impl loc "db 2" ] ]
  ;
  db:
    [ [ x1 = LIST1 db1 SEP "and" ->
          List.fold_right (fun td tdl -> td @ tdl) x1 [] ] ]
  ;
  dbrhs:
    [ [ x1 = LIST1 constr SEP "|"  -> x1
      | "datatype"; x1 = tycon -> not_impl loc "dbrhs 2" ] ]
  ;
  constr:
    [ [ x1 = op_op; x2 = ident -> (loc, x2, [], None)
      | x1 = op_op; x2 = ident; "of"; x3 = ctyp ->
          match x3 with
          [ <:ctyp< {$list:_$} >> -> (loc, x2, [], Some x3)
          | _ -> (loc, x2, [x3], None) ] ] ]
  ;
  eb:
    [ [ x1 = op_op; x2 = ident -> (x2, [], [])
      | x1 = op_op; x2 = ident; "of"; x3 = ctyp -> (x2, [x3], [])
      | x1 = op_op; x2 = ident; "="; x3 = sqid -> (x2, [], x3) ] ]
  ;
  ldec1:
    [ [ "val"; x1 = LIST1 vb SEP "and" -> x1
      | "fun"; x1 = LIST1 fb SEP "and" -> x1 ] ]
  ;
  ldecs:
    [ [ -> []
      | x1 = ldec1; x2 = ldecs -> [x1 :: x2]
      | ";"; x1 = ldecs -> x1
      | "local"; x1 = ldecs; "in"; x2 = ldecs; "end"; x3 = ldecs ->
          not_impl loc "ldecs 4" ] ]
  ;
  spec_s:
    [ [ -> []
      | x1 = spec; x2 = spec_s -> [x1 :: x2]
      | ";"; x1 = spec_s -> x1 ] ]
  ;
  spec:
   [ [ "structure"; x1 = LIST1 strspec SEP "and" -> sig_declare loc x1
     | "functor"; x1 = LIST1 fctspec SEP "and" -> sig_declare loc x1
     | "datatype"; x1 = db -> <:sig_item< type $list:x1$ >>
     | "type"; x1 = LIST1 tyspec SEP "and" -> <:sig_item< type $list:x1$ >>
     | "eqtype"; x1 = LIST1 tyspec SEP "and" -> <:sig_item< type $list:x1$ >>
     | "val"; x1 = LIST1 valspec SEP "and" -> sig_declare loc x1
     | "exception"; x1 = LIST1 exnspec SEP "and" -> sig_declare loc x1
     | "sharing"; x1 = LIST1 sharespec SEP "and" -> <:sig_item< declare end >>
     | "include"; x1 = module_type -> <:sig_item< include $x1$ >> ] ]
  ;
  sig_item:
    [ [ x = spec -> x ] ]
  ;
  strspec:
    [ [ x1 = ident; ":"; x2 = module_type; x3 = LIST0 sharing_def ->
          let x2 =
            List.fold_left
              (fun mt sdl ->
                 List.fold_right
                   (fun spl mt ->
                      match spl with
                      [ Right ([m1], m2) ->
                          let (m1, m2) =
                            match m2 with
                            [ <:module_expr< $uid:x$ . $_$ >> ->
                                if x = x1 then (m2, m1) else (m1, m2)
                            | _ -> (m1, m2) ]
                          in
                          let m1 =
                            loop m1 where rec loop =
                              fun
                              [ <:module_expr< $uid:x$ >> -> x
                              | <:module_expr< $uid:x$ . $y$ >> -> loop y
                              | _ -> not_impl loc "strspec 2" ]
                          in
                          <:module_type< $mt$ with module $[m1]$ = $m2$ >>
                      | _ -> not_impl loc "strspec 1" ])
                   sdl mt)
              x2 x3
          in
          <:sig_item< module $x1$ : $x2$ >> ] ]
  ;
  sharing_def:
    [ [ "sharing"; x3 = LIST1 sharespec SEP "and" -> x3 ] ]
  ;
  fctspec:
    [ [ x1 = ident; x2 = fsig -> <:sig_item< module $x1$ : $x2$ >> ] ]
  ;
  tyspec:
    [ [ x1 = tyvars; x2 = idd ->
          ((loc, uncap x2), x1, <:ctyp< '$choose_tvar x1$ >>, [])
      | x1 = tyvars; x2 = idd; "="; x3 = ctyp ->
          ((loc, uncap x2), x1, x3, []) ] ]
  ;
  valspec:
    [ [ x1 = op_op; x2 = ident; ":"; x3 = ctyp ->
          <:sig_item< value $x2$ : $x3$ >> ] ]
  ;
  exnspec:
    [ [ x1 = ident -> <:sig_item< exception $x1$ >>
      | x1 = ident; "of"; x2 = ctyp ->
          <:sig_item< exception $x1$ of $x2$ >> ] ]
  ;
  sharespec:
    [ [ "type"; x1 = patheqn -> Left x1
      | x1 = patheqn -> Right x1 ] ]
  ;
  patheqn:
    [ [ l = patheqn1 -> l ] ]
  ;
  patheqn1:
    [ [ (l, y) = patheqn1; "="; x = qid -> ([y :: l], x)
      | x = qid -> ([], x) ] ]
  ;
  whspec:
    [ [ "type"; x1 = tyvars; x2 = sqid; "="; x3 = ctyp ->
          MLast.WcTyp loc x2 x1 x3
      | x1 = sqid; "="; x2 = qid -> MLast.WcMod loc x1 x2 ] ]
  ;
  module_type:
    [ [ x1 = ident -> <:module_type< $uid:x1$ >>
      | "sig"; x1 = spec_s; "end" -> <:module_type< sig $list:x1$ end >>
      | x1 = module_type; "where"; x2 = LIST1 whspec SEP "and" ->
          <:module_type< $x1$ with $list:x2$ >> ] ]
  ;
  sigconstraint_op:
    [ [ -> None
      | ":"; x1 = module_type -> Some x1
      | ":>"; x1 = module_type -> not_impl loc "sigconstraint_op 3" ] ]
  ;
  sigb:
    [ [ x1 = ident; "="; x2 = module_type ->
          <:str_item< module type $x1$ = $x2$ >> ] ]
  ;
  fsig:
    [ [ ":"; x1 = ident -> not_impl loc "fsig 1"
      | x1 = fparamList; ":"; x2 = module_type -> not_impl loc "fsig 2" ] ]
  ;
  module_expr:
    [ [ x1 = qid -> x1
      | "struct"; x1 = strdecs; "end" -> <:module_expr< struct $list:x1$ end >>
      | x1 = qid; x2 = arg_fct ->
          match x2 with
          [ Left [] -> x1
          | Left x2 -> <:module_expr< $x1$ (struct $list:x2$ end) >>
          | Right x2 -> <:module_expr< $x1$ $x2$ >> ]
      | "let"; x1 = strdecs; "in"; x2 = module_expr; "end" ->
          not_impl loc "str 4"
      | x1 = module_expr; ":"; x2 = module_type -> not_impl loc "str 5"
      | x1 = module_expr; x2 = ":>"; x3 = module_type ->
          not_impl loc "str 6" ] ]
  ;
  arg_fct:
    [ [ "("; x1 = strdecs; ")"; x2 = arg_fct -> not_impl loc "arg_fct 1"
      | "("; x1 = module_expr; ")"; x2 = arg_fct -> not_impl loc "arg_fct 2"
      | "("; x1 = module_expr; ")" -> Right x1
      | "("; x2 = strdecs; ")" -> Left x2 ] ]
  ;
  strdecs:
    [ [ x1 = str_item LEVEL "strdec"; x2 = strdecs -> [x1 :: x2]
      | ";"; x1 = strdecs -> x1
      | -> [] ] ]
  ;
  str_item:
    [ [ "signature"; x1 = LIST1 sigb SEP "and" -> str_declare loc x1
      | "funsig"; x1 = fsigb -> not_impl loc "sdec 3" ]
    | "strdec"
      [ "structure"; x1 = LIST1 strb SEP "and" -> str_declare loc x1
      | "functor"; x1 = LIST1 fctb SEP "and" -> str_declare loc x1
      | "local"; x1 = sdecs; "in"; x2 = sdecs; "end" ->
          make_local loc x1 x2 ]
    | [ "val"; x1 = LIST1 vb SEP "and" -> <:str_item< value $list:x1$ >>
      | "val"; x1 = tyvarseq; x3 = LIST1 vb SEP "and" ->
          not_impl loc "ldec 2"
      | "val"; "rec"; x1 = rvb -> not_impl loc "ldec 3"
      | "val"; "rec"; x1 = tyvarseq; x2 = rvb -> not_impl loc "ldec 4"
      | "fun"; x1 = LIST1 fb SEP "and" -> <:str_item< value rec $list:x1$ >>
      | "fun"; x1 = tyvarseq; x2 = fb -> not_impl loc "ldec 6"
      | "type"; x1 = LIST1 tb SEP "and" -> <:str_item< type $list:x1$ >>
      | "datatype"; x1 = db -> <:str_item< type $list:x1$ >>
      | "datatype"; x1 = db; "withtype"; x2 = tb ->
          <:str_item< type $list:x1 @ [x2]$ >>
      | "abstype"; x1 = db; "with"; x2 = ldecs; "end" -> not_impl loc "ldec 10"
      | "abstype"; x1 = db; "withtype"; x2 = tb; "with"; x3 = ldecs; "end" ->
          not_impl loc "ldec 11"
      | "exception"; x1 = LIST1 eb SEP "and" ->
          let dl =
            List.map
              (fun (s, tl, eqn) ->
                 <:str_item< exception $s$ of $list:tl$ = $eqn$ >>)
              x1
          in
          str_declare loc dl
      | "open"; x1 = LIST1 sqid ->
          let dl = List.map (fun sl -> <:str_item< open $sl$ >>) x1 in
          str_declare loc dl
      | LIDENT "use"; s = STRING ->
          <:str_item< #use $str:s$ >>
      | x1 = fixity; list = LIST1 idd ->
          match x1 with
          [ ("infixr", Some n) ->
              do {
                List.iter
                  (fun s ->
                     EXTEND
                       expr: LEVEL $n$
                         [ [ x1 = expr; $s$; x2 = expr ->
                               <:expr< $lid:s$ ($x1$, $x2$) >> ] ]
                       ;
                     END)
                  list;
                  str_declare loc []
              }
          | ("infix", None) ->
              do {
                List.iter
                  (fun s ->
                     EXTEND
                       expr: LEVEL "4"
                         [ [ x1 = expr; $s$; x2 = expr ->
                               <:expr< $lid:s$ ($x1$, $x2$) >> ] ]
                       ;
                       clause:
                         [ [ x1 = patt LEVEL "apat"; $s$;
                             x2 = patt LEVEL "apat"; "="; x4 = expr ->
                               ((s, loc), [<:patt< ($x1$, $x2$) >>],
                                None, x4) ] ]
                       ;
                     END)
                  list;
                  str_declare loc []
              }
          | _ -> not_impl loc "ldec 14" ]
      | "overload"; x1 = ident; ":"; x2 = ctyp; "as"; x3 = exp_pa ->
          not_impl loc "ldec 15"
      | x = expr -> <:str_item< $exp:x$ >> ] ]
  ;
  sdec:
    [ [ x = str_item -> x ] ]
  ;
  strb:
    [ [ x1 = ident; x2 = sigconstraint_op; "="; x3 = module_expr ->
          let x3 =
            match x2 with
            [ Some x2 -> <:module_expr< ($x3$ : $x2$) >>
            | None -> x3 ]
          in
          <:str_item< module $x1$ = $x3$ >> ] ]
  ;
  fparam:
    [ [ x1 = idd; ":"; x2 = module_type -> [<:sig_item< module $x1$ : $x2$ >>]
      | x1 = spec_s -> x1 ] ]
  ;
  fparamList:
    [ [ "("; x1 = fparam; ")" -> [x1]
      | "("; x1 = fparam; ")"; x2 = fparamList -> [x1 :: x2] ] ]
  ;
  fctb:
    [ [ x1 = ident; x2 = fparamList; x3 = sigconstraint_op; "=";
        x4 = module_expr ->
          let list = List.flatten x2 in
          let x4 =
            if list = [] then x4
            else
              match x4 with
              [ <:module_expr< struct $list:list$ end >> ->
                  let si =
                    let loc = (Token.nowhere, Token.nowhere) in
                    <:str_item< open AAA >> in
                  <:module_expr< struct $list:[si :: list]$ end >>
              | _ -> not_impl loc "fctb 1" ]
          in
          let x4 =
            match x3 with
            [ Some x3 -> <:module_expr< ($x4$ : $x3$) >>
            | None -> x4 ]
          in
          let x4 =
            if list = [] then x4
            else
              let mt =
                let loc =
                  (fst (MLast.loc_of_sig_item (List.hd list)),
                   snd (MLast.loc_of_sig_item (List.hd (List.rev list))))
                in
                <:module_type< sig $list:list$ end >>
              in
              <:module_expr< functor (AAA : $mt$) -> $x4$ >>
          in
          <:str_item< module $x1$ = $x4$ >>
      | x1 = ident; x2 = fsigconstraint_op; "="; x3 = fct_exp ->
          not_impl loc "fctb 2" ] ]
  ;
  interdec:
    [ [ x = LIST1 [ s = str_item; OPT ";" -> (s, loc) ] -> (x, False)
      | x = expr; OPT ";" -> not_impl loc "interdec 2" ] ]
  ;
END;

Pcaml.add_option "-records" (Arg.Set ocaml_records)
  "Convert record into OCaml records, instead of objects";
