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
open Pcaml;

Pcaml.syntax_name.val := "OCaml";
Pcaml.no_constructors_arity.val := True;

do {
  let odfa = Plexer.dollar_for_antiquotation.val in
  Plexer.dollar_for_antiquotation.val := False;
  let (lexer, pos) = Plexer.make_lexer () in
  Pcaml.position.val := pos;
  Grammar.Unsafe.gram_reinit gram lexer;
  Plexer.dollar_for_antiquotation.val := odfa;
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
  Grammar.Unsafe.clear_entry type_declaration;
  Grammar.Unsafe.clear_entry class_type;
  Grammar.Unsafe.clear_entry class_expr;
  Grammar.Unsafe.clear_entry class_sig_item;
  Grammar.Unsafe.clear_entry class_str_item
};

Pcaml.parse_interf.val := Grammar.Entry.parse interf;
Pcaml.parse_implem.val := Grammar.Entry.parse implem;

value o2b =
  fun
  [ Some _ -> True
  | None -> False ]
;

value mkexprident _loc ids = match ids with
  [ [] -> Stdpp.raise_with_loc _loc (Stream.Error "illegal long identifier")
  | [ id :: ids ] ->
      let rec loop m = fun
        [ [ id :: ids ] -> loop <:expr< $m$ . $id$ >> ids
        | [] -> m ]
  in
  loop id ids ]
;

value mkumin _loc f arg =
  match (f, arg) with
  [ ("-", <:expr< $int:n$ >>) when int_of_string n > 0 ->
      let n = "-" ^ n in
      <:expr< $int:n$ >>
  | ("-", MLast.ExInt32 loc n) when (Int32.of_string n) > 0l ->
      MLast.ExInt32 loc ("-" ^ n)
  | ("-", MLast.ExInt64 loc n) when (Int64.of_string n) > 0L ->
      MLast.ExInt64 loc ("-" ^ n)
  | ("-", MLast.ExNativeInt loc n) when (Nativeint.of_string n) > 0n ->
      MLast.ExNativeInt loc ("-" ^ n)
  | (_, <:expr< $flo:n$ >>) when float_of_string n > 0.0 ->
      let n = "-" ^ n in
      <:expr< $flo:n$ >>
  | _ ->
      let f = "~" ^ f in
      <:expr< $lid:f$ $arg$ >> ]
;


value mklistexp _loc last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some e -> e
        | None -> <:expr< [] >> ]
    | [e1 :: el] ->
        let _loc = if top then _loc else (fst (MLast.loc_of_expr e1), snd _loc) in
        <:expr< [$e1$ :: $loop False el$] >> ]
;

value mklistpat _loc last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some p -> p
        | None -> <:patt< [] >> ]
    | [p1 :: pl] ->
        let _loc = if top then _loc else (fst (MLast.loc_of_patt p1), snd _loc) in
        <:patt< [$p1$ :: $loop False pl$] >> ]
;

value is_operator =
  let ht = Hashtbl.create 73 in
  let ct = Hashtbl.create 73 in
  do {
    List.iter (fun x -> Hashtbl.add ht x True)
      ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"];
    List.iter (fun x -> Hashtbl.add ct x True)
      ['!'; '&'; '*'; '+'; '-'; '/'; ':'; '<'; '='; '>'; '@'; '^'; '|'; '~';
       '?'; '%'; '.'; '$'];
    fun x ->
      try Hashtbl.find ht x with
      [ Not_found -> try Hashtbl.find ct x.[0] with _ -> False ]
  }
;

value operator_rparen =
  Grammar.Entry.of_parser gram "operator_rparen"
    (fun strm ->
       match Stream.npeek 2 strm with
       [ [("", s); ("", ")")] when is_operator s ->
           do { Stream.junk strm; Stream.junk strm; s }
       | _ -> raise Stream.Failure ])
;

value lident_colon =
  Grammar.Entry.of_parser gram "lident_colon"
    (fun strm ->
       match Stream.npeek 2 strm with
       [ [("LIDENT", i); ("", ":")] ->
           do { Stream.junk strm; Stream.junk strm; i }
       | _ -> raise Stream.Failure ])
;

value symbolchar =
  let list =
    ['!'; '$'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?';
     '@'; '^'; '|'; '~']
  in
  let rec loop s i =
    if i == String.length s then True
    else if List.mem s.[i] list then loop s (i + 1)
    else False
  in
  loop
;

value prefixop =
  let list = ['!'; '?'; '~'] in
  let excl = ["!="; "??"] in
  Grammar.Entry.of_parser gram "prefixop"
    (parser
       [: `("", x)
           when
             not (List.mem x excl) && String.length x >= 2 &&
             List.mem x.[0] list && symbolchar x 1 :] ->
         x)
;

value infixop0 =
  let list = ['='; '<'; '>'; '|'; '&'; '$'] in
  let excl = ["<-"; "||"; "&&"] in
  Grammar.Entry.of_parser gram "infixop0"
    (parser
       [: `("", x)
           when
             not (List.mem x excl) && String.length x >= 2 &&
             List.mem x.[0] list && symbolchar x 1 :] ->
         x)
;

value infixop1 =
  let list = ['@'; '^'] in
  Grammar.Entry.of_parser gram "infixop1"
    (parser
       [: `("", x)
           when
             String.length x >= 2 && List.mem x.[0] list &&
             symbolchar x 1 :] ->
         x)
;

value infixop2 =
  let list = ['+'; '-'] in
  Grammar.Entry.of_parser gram "infixop2"
    (parser
       [: `("", x)
           when
             x <> "->" && String.length x >= 2 && List.mem x.[0] list &&
             symbolchar x 1 :] ->
         x)
;

value infixop3 =
  let list = ['*'; '/'; '%'] in
  Grammar.Entry.of_parser gram "infixop3"
    (parser
       [: `("", x)
           when
             String.length x >= 2 && List.mem x.[0] list &&
             symbolchar x 1 :] ->
         x)
;

value infixop4 =
  Grammar.Entry.of_parser gram "infixop4"
    (parser
       [: `("", x)
           when
             String.length x >= 3 && x.[0] == '*' && x.[1] == '*' &&
             symbolchar x 2 :] ->
         x)
;

value test_constr_decl =
  Grammar.Entry.of_parser gram "test_constr_decl"
    (fun strm ->
       match Stream.npeek 1 strm with
       [ [("UIDENT", _)] ->
           match Stream.npeek 2 strm with
           [ [_; ("", ".")] -> raise Stream.Failure
           | [_; ("", "(")] -> raise Stream.Failure
           | [_ :: _] -> ()
           | _ -> raise Stream.Failure ]
       | [("", "|")] -> ()
       | _ -> raise Stream.Failure ])
;

value stream_peek_nth n strm =
  loop n (Stream.npeek n strm) where rec loop n =
    fun
    [ [] -> None
    | [x] -> if n == 1 then Some x else None
    | [_ :: l] -> loop (n - 1) l ]
;

(* horrible hack to be able to parse class_types *)

value test_ctyp_minusgreater =
  Grammar.Entry.of_parser gram "test_ctyp_minusgreater"
    (fun strm ->
       let rec skip_simple_ctyp n =
         match stream_peek_nth n strm with
         [ Some ("", "->") -> n
         | Some ("", "[" | "[<") ->
             skip_simple_ctyp (ignore_upto "]" (n + 1) + 1)
         | Some ("", "(") -> skip_simple_ctyp (ignore_upto ")" (n + 1) + 1)
         | Some
             ("",
              "as" | "'" | ":" | "*" | "." | "#" | "<" | ">" | ".." | ";" |
              "_") ->
             skip_simple_ctyp (n + 1)
         | Some ("QUESTIONIDENT" | "LIDENT" | "UIDENT", _) ->
             skip_simple_ctyp (n + 1)
         | Some _ | None -> raise Stream.Failure ]
       and ignore_upto end_kwd n =
         match stream_peek_nth n strm with
         [ Some ("", prm) when prm = end_kwd -> n
         | Some ("", "[" | "[<") ->
             ignore_upto end_kwd (ignore_upto "]" (n + 1) + 1)
         | Some ("", "(") -> ignore_upto end_kwd (ignore_upto ")" (n + 1) + 1)
         | Some _ -> ignore_upto end_kwd (n + 1)
         | None -> raise Stream.Failure ]
       in
       match Stream.peek strm with
       [ Some (("", "[") | ("LIDENT" | "UIDENT", _)) -> skip_simple_ctyp 1
       | Some ("", "object") -> raise Stream.Failure
       | _ -> 1 ])
;

value test_label_eq =
  Grammar.Entry.of_parser gram "test_label_eq"
    (test 1 where rec test lev strm =
       match stream_peek_nth lev strm with
       [ Some (("UIDENT", _) | ("LIDENT", _) | ("", ".")) ->
           test (lev + 1) strm
       | Some ("", "=") -> ()
       | _ -> raise Stream.Failure ])
;

value test_typevar_list_dot =
  Grammar.Entry.of_parser gram "test_typevar_list_dot"
    (let rec test lev strm =
       match stream_peek_nth lev strm with
       [ Some ("", "'") -> test2 (lev + 1) strm
       | Some ("", ".") -> ()
       | _ -> raise Stream.Failure ]
    and test2 lev strm =
       match stream_peek_nth lev strm with
       [ Some ("UIDENT" | "LIDENT", _) -> test (lev + 1) strm
       | _ -> raise Stream.Failure ]
    in
    test 1)
;

value constr_arity = ref [("Some", 1); ("Match_Failure", 1)];

value rec is_expr_constr_call =
  fun
  [ <:expr< $uid:_$ >> -> True
  | <:expr< $uid:_$.$e$ >> -> is_expr_constr_call e
  | <:expr< $e$ $_$ >> -> is_expr_constr_call e
  | _ -> False ]
;

value rec constr_expr_arity _loc =
  fun
  [ <:expr< $uid:c$ >> ->
      try List.assoc c constr_arity.val with [ Not_found -> 0 ]
  | <:expr< $uid:_$.$e$ >> -> constr_expr_arity _loc e
  | <:expr< $e$ $_$ >> ->
      if is_expr_constr_call e then
        Stdpp.raise_with_loc _loc (Stream.Error "currified constructor")
      else 1
  | _ -> 1 ]
;

value rec is_patt_constr_call =
  fun
  [ <:patt< $uid:_$ >> -> True
  | <:patt< $uid:_$.$p$ >> -> is_patt_constr_call p
  | <:patt< $p$ $_$ >> -> is_patt_constr_call p
  | _ -> False ]
;

value rec constr_patt_arity _loc =
  fun
  [ <:patt< $uid:c$ >> ->
      try List.assoc c constr_arity.val with [ Not_found -> 0 ]
  | <:patt< $uid:_$.$p$ >> -> constr_patt_arity _loc p
  | <:patt< $p$ $_$ >> ->
      if is_patt_constr_call p then
        Stdpp.raise_with_loc _loc (Stream.Error "currified constructor")
      else 1
  | _ -> 1 ]
;

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

value rec patt_lid =
  fun
  [ <:patt< $p1$ $p2$ >> ->
      match p1 with
      [ <:patt< $lid:i$ >> -> Some (MLast.loc_of_patt p1, i, [p2])
      | _ ->
          match patt_lid p1 with
          [ Some (loc, i, pl) -> Some (loc, i, [p2 :: pl])
          | None -> None ] ]
  | _ -> None ]
;

value bigarray_get _loc arr arg =
  let coords =
    match arg with
    [ <:expr< ($list:el$) >> -> el
    | _ -> [arg] ]
  in
  match coords with
  [ [c1] -> <:expr< Bigarray.Array1.get $arr$ $c1$ >>
  | [c1; c2] -> <:expr< Bigarray.Array2.get $arr$ $c1$ $c2$ >>
  | [c1; c2; c3] -> <:expr< Bigarray.Array3.get $arr$ $c1$ $c2$ $c3$ >>
  | coords -> <:expr< Bigarray.Genarray.get $arr$ [| $list:coords$ |] >> ]
;

value bigarray_set _loc var newval =
  match var with
  [ <:expr< Bigarray.Array1.get $arr$ $c1$ >> ->
      Some <:expr< Bigarray.Array1.set $arr$ $c1$ $newval$ >>
  | <:expr< Bigarray.Array2.get $arr$ $c1$ $c2$ >> ->
      Some <:expr< Bigarray.Array2.set $arr$ $c1$ $c2$ $newval$ >>
  | <:expr< Bigarray.Array3.get $arr$ $c1$ $c2$ $c3$ >> ->
      Some <:expr< Bigarray.Array3.set $arr$ $c1$ $c2$ $c3$ $newval$ >>
  | <:expr< Bigarray.Genarray.get $arr$ [| $list:coords$ |] >> ->
      Some <:expr< Bigarray.Genarray.set $arr$ [| $list:coords$ |] $newval$ >>
  | _ -> None ]
;

(* ...works bad...
value rec sync cs =
  match cs with parser
  [ [: `';' :] -> sync_semi cs
  | [: `_ :] -> sync cs ]
and sync_semi cs =
  match cs with parser
  [ [: `';' :] -> sync_semisemi cs
  | [: :] -> sync cs ]
and sync_semisemi cs =
  match Stream.peek cs with 
  [ Some ('\010' | '\013') -> ()
  | _ -> sync_semi cs ]
;
Pcaml.sync.val := sync;
*)


EXTEND
  GLOBAL: sig_item str_item ctyp patt expr module_type module_expr class_type
    class_expr class_sig_item class_str_item let_binding type_declaration;
  module_expr:
    [ [ "functor"; "("; i = UIDENT; ":"; t = module_type; ")"; "->";
        me = SELF ->
          <:module_expr< functor ( $i$ : $t$ ) -> $me$ >>
      | "struct"; st = LIST0 [ s = str_item; OPT ";;" -> s ]; "end" ->
          <:module_expr< struct $list:st$ end >> ]
    | [ me1 = SELF; me2 = SELF -> <:module_expr< $me1$ $me2$ >> ]
    | [ i = mod_expr_ident -> i
      | "("; me = SELF; ":"; mt = module_type; ")" ->
          <:module_expr< ( $me$ : $mt$ ) >>
      | "("; me = SELF; ")" -> <:module_expr< $me$ >> ] ]
  ;

  mod_expr_ident:
    [ LEFTA
      [ i = SELF; "."; j = SELF -> <:module_expr< $i$ . $j$ >> ]
    | [ i = UIDENT -> <:module_expr< $uid:i$ >> ] ]
  ;

  str_item:
    [ "top"
      [ "exception"; (_, c, tl) = constructor_declaration; b = rebind_exn ->
          <:str_item< exception $c$ of $list:tl$ = $b$ >>
      | "external"; i = LIDENT; ":"; t = ctyp; "="; pd = LIST1 STRING ->
          <:str_item< external $i$ : $t$ = $list:pd$ >>
      | "external"; "("; i = operator_rparen; ":"; t = ctyp; "=";
        pd = LIST1 STRING ->
          <:str_item< external $i$ : $t$ = $list:pd$ >>
      | "include"; me = module_expr -> <:str_item< include $me$ >>
      | "module"; i = UIDENT; mb = module_binding ->
          <:str_item< module $i$ = $mb$ >>
      | "module"; "rec"; nmtmes = LIST1 module_rec_binding SEP "and" ->
          MLast.StRecMod _loc nmtmes
      | "module"; "type"; i = UIDENT; "="; mt = module_type ->
          <:str_item< module type $i$ = $mt$ >>
      | "open"; i = mod_ident -> <:str_item< open $i$ >>
      | "type"; tdl = LIST1 type_declaration SEP "and" ->
          <:str_item< type $list:tdl$ >>
      | "let"; r = OPT "rec"; l = LIST1 let_binding SEP "and"; "in";
        x = expr ->
          let e = <:expr< let $opt:o2b r$ $list:l$ in $x$ >> in
          <:str_item< $exp:e$ >>
      | "let"; r = OPT "rec"; l = LIST1 let_binding SEP "and" ->
          match l with
          [ [(<:patt< _ >>, e)] -> <:str_item< $exp:e$ >>
          | _ -> <:str_item< value $opt:o2b r$ $list:l$ >> ]
      | "let"; "module"; m = UIDENT; mb = module_binding; "in"; e = expr ->
          <:str_item< let module $m$ = $mb$ in $e$ >>
      | e = expr -> <:str_item< $exp:e$ >> ] ]
  ;

  rebind_exn:
    [ [ "="; sl = mod_ident -> sl
      | -> [] ] ]
  ;
  module_binding:
    [ RIGHTA
      [ "("; m = UIDENT; ":"; mt = module_type; ")"; mb = SELF ->
          <:module_expr< functor ( $m$ : $mt$ ) -> $mb$ >>
      | ":"; mt = module_type; "="; me = module_expr ->
          <:module_expr< ( $me$ : $mt$ ) >>
      | "="; me = module_expr -> <:module_expr< $me$ >> ] ]
  ;
  module_rec_binding:
    [ [ m = UIDENT; ":"; mt = module_type; "="; me = module_expr ->
          (m, mt, me) ] ]
  ;
  (* Module types *)
  module_type:
    [ [ "functor"; "("; i = UIDENT; ":"; t = SELF; ")"; "->"; mt = SELF ->
          <:module_type< functor ( $i$ : $t$ ) -> $mt$ >> ]
    | [ mt = SELF; "with"; wcl = LIST1 with_constr SEP "and" ->
          <:module_type< $mt$ with $list:wcl$ >> ]
    | [ "sig"; sg = LIST0 [ s = sig_item; OPT ";;" -> s ]; "end" ->
          <:module_type< sig $list:sg$ end >>
      | i = mod_type_ident -> i
      | "("; mt = SELF; ")" -> <:module_type< $mt$ >> ] ]
  ;
  mod_type_ident:
    [ LEFTA
      [ m1 = SELF; "."; m2 = SELF -> <:module_type< $m1$ . $m2$ >>
      | m1 = SELF; "("; m2 = SELF; ")" -> <:module_type< $m1$ $m2$ >> ]
    | [ m = UIDENT -> <:module_type< $uid:m$ >>
      | m = LIDENT -> <:module_type< $lid:m$ >> ] ]
  ;
  sig_item:
    [ "top"
      [ "exception"; (_, c, tl) = constructor_declaration ->
          <:sig_item< exception $c$ of $list:tl$ >>
      | "external"; i = LIDENT; ":"; t = ctyp; "="; pd = LIST1 STRING ->
          <:sig_item< external $i$ : $t$ = $list:pd$ >>
      | "external"; "("; i = operator_rparen; ":"; t = ctyp; "=";
        pd = LIST1 STRING ->
          <:sig_item< external $i$ : $t$ = $list:pd$ >>
      | "include"; mt = module_type -> <:sig_item< include $mt$ >>
      | "module"; i = UIDENT; mt = module_declaration ->
          <:sig_item< module $i$ : $mt$ >>
      | "module"; "rec"; mds = LIST1 module_rec_declaration SEP "and" ->
          MLast.SgRecMod _loc mds
      | "module"; "type"; i = UIDENT; "="; mt = module_type ->
          <:sig_item< module type $i$ = $mt$ >>
      | "module"; "type"; i = UIDENT ->
          <:sig_item< module type $i$ = 'abstract >>
      | "open"; i = mod_ident -> <:sig_item< open $i$ >>
      | "type"; tdl = LIST1 type_declaration SEP "and" ->
          <:sig_item< type $list:tdl$ >>
      | "val"; i = LIDENT; ":"; t = ctyp -> <:sig_item< value $i$ : $t$ >>
      | "val"; "("; i = operator_rparen; ":"; t = ctyp ->
          <:sig_item< value $i$ : $t$ >> ] ]
  ;
  module_declaration:
    [ RIGHTA
      [ ":"; mt = module_type -> <:module_type< $mt$ >>
      | "("; i = UIDENT; ":"; t = module_type; ")"; mt = SELF ->
          <:module_type< functor ( $i$ : $t$ ) -> $mt$ >> ] ]
  ;
  module_rec_declaration:
    [ [ m = UIDENT; ":"; mt = module_type -> (m, mt)] ]
  ;
  (* "with" constraints (additional type equations over signature
     components) *)
  with_constr:
    [ [ "type"; tpl = type_parameters; i = mod_ident; "="; t = ctyp ->
          MLast.WcTyp _loc i tpl t
      | "module"; i = mod_ident; "="; me = module_expr ->
          MLast.WcMod _loc i me ] ]
  ;
  (* Core expressions *)
  expr:
    [ "top" RIGHTA
      [ e1 = SELF; ";"; e2 = SELF ->
          <:expr< do { $list:[e1 :: get_seq e2]$ } >>
      | e1 = SELF; ";" -> e1 ]
    | "expr1"
      [ "let"; o = OPT "rec"; l = LIST1 let_binding SEP "and"; "in";
        x = expr LEVEL "top" ->
          <:expr< let $opt:o2b o$ $list:l$ in $x$ >>
      | "let"; "module"; m = UIDENT; mb = module_binding; "in";
        e = expr LEVEL "top" ->
          <:expr< let module $m$ = $mb$ in $e$ >>
      | "function"; OPT "|"; l = LIST1 match_case SEP "|" ->
          <:expr< fun [ $list:l$ ] >>
      | "fun"; p = patt LEVEL "simple"; e = fun_def ->
          <:expr< fun [$p$ -> $e$] >>
      | "match"; e = SELF; "with"; OPT "|"; l = LIST1 match_case SEP "|" ->
          <:expr< match $e$ with [ $list:l$ ] >>
      | "try"; e = SELF; "with"; OPT "|"; l = LIST1 match_case SEP "|" ->
          <:expr< try $e$ with [ $list:l$ ] >>
      | "if"; e1 = SELF; "then"; e2 = expr LEVEL "expr1";
        "else"; e3 = expr LEVEL "expr1" ->
          <:expr< if $e1$ then $e2$ else $e3$ >>
      | "if"; e1 = SELF; "then"; e2 = expr LEVEL "expr1" ->
          <:expr< if $e1$ then $e2$ else () >>
      | "for"; i = LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; e = SELF; "done" ->
          <:expr< for $i$ = $e1$ $to:df$ $e2$ do { $list:get_seq e$ } >>
      | "while"; e1 = SELF; "do"; e2 = SELF; "done" ->
          <:expr< while $e1$ do { $list:get_seq e2$ } >>
      | "object"; cspo = OPT class_self_patt; cf = class_structure; "end" ->
          (* <:expr< object $opt:cspo$ $list:cf$ end >> *)
          MLast.ExObj _loc cspo cf ]
    | [ e = SELF; ","; el = LIST1 NEXT SEP "," ->
          <:expr< ( $list:[e :: el]$ ) >> ]
    | ":=" NONA
      [ e1 = SELF; ":="; e2 = expr LEVEL "expr1" ->
          <:expr< $e1$.val := $e2$ >>
      | e1 = SELF; "<-"; e2 = expr LEVEL "expr1" ->
          match bigarray_set _loc e1 e2 with
          [ Some e -> e
          | None -> <:expr< $e1$ := $e2$ >> ] ]
    | "||" RIGHTA
      [ e1 = SELF; "or"; e2 = SELF -> <:expr< $lid:"or"$ $e1$ $e2$ >>
      | e1 = SELF; "||"; e2 = SELF -> <:expr< $e1$ || $e2$ >> ]
    | "&&" RIGHTA
      [ e1 = SELF; "&"; e2 = SELF -> <:expr< $lid:"&"$ $e1$ $e2$ >>
      | e1 = SELF; "&&"; e2 = SELF -> <:expr< $e1$ && $e2$ >> ]
    | "<" LEFTA
      [ e1 = SELF; "<"; e2 = SELF -> <:expr< $e1$ < $e2$ >>
      | e1 = SELF; ">"; e2 = SELF -> <:expr< $e1$ > $e2$ >>
      | e1 = SELF; "<="; e2 = SELF -> <:expr< $e1$ <= $e2$ >>
      | e1 = SELF; ">="; e2 = SELF -> <:expr< $e1$ >= $e2$ >>
      | e1 = SELF; "="; e2 = SELF -> <:expr< $e1$ = $e2$ >>
      | e1 = SELF; "<>"; e2 = SELF -> <:expr< $e1$ <> $e2$ >>
      | e1 = SELF; "=="; e2 = SELF -> <:expr< $e1$ == $e2$ >>
      | e1 = SELF; "!="; e2 = SELF -> <:expr< $e1$ != $e2$ >>
      | e1 = SELF; "$"; e2 = SELF -> <:expr< $lid:"\$"$ $e1$ $e2$ >>
      | e1 = SELF; op = infixop0; e2 = SELF -> <:expr< $lid:op$ $e1$ $e2$ >> ]
    | "^" RIGHTA
      [ e1 = SELF; "^"; e2 = SELF -> <:expr< $e1$ ^ $e2$ >>
      | e1 = SELF; "@"; e2 = SELF -> <:expr< $e1$ @ $e2$ >>
      | e1 = SELF; op = infixop1; e2 = SELF -> <:expr< $lid:op$ $e1$ $e2$ >> ]
    | RIGHTA
      [ e1 = SELF; "::"; e2 = SELF -> <:expr< [$e1$ :: $e2$] >> ]
    | "+" LEFTA
      [ e1 = SELF; "+"; e2 = SELF -> <:expr< $e1$ + $e2$ >>
      | e1 = SELF; "-"; e2 = SELF -> <:expr< $e1$ - $e2$ >>
      | e1 = SELF; op = infixop2; e2 = SELF -> <:expr< $lid:op$ $e1$ $e2$ >> ]
    | "*" LEFTA
      [ e1 = SELF; "*"; e2 = SELF -> <:expr< $e1$ * $e2$ >>
      | e1 = SELF; "/"; e2 = SELF -> <:expr< $e1$ / $e2$ >>
      | e1 = SELF; "%"; e2 = SELF -> <:expr< $lid:"%"$ $e1$ $e2$ >>
      | e1 = SELF; "land"; e2 = SELF -> <:expr< $e1$ land $e2$ >>
      | e1 = SELF; "lor"; e2 = SELF -> <:expr< $e1$ lor $e2$ >>
      | e1 = SELF; "lxor"; e2 = SELF -> <:expr< $e1$ lxor $e2$ >>
      | e1 = SELF; "mod"; e2 = SELF -> <:expr< $e1$ mod $e2$ >>
      | e1 = SELF; op = infixop3; e2 = SELF -> <:expr< $lid:op$ $e1$ $e2$ >> ]
    | "**" RIGHTA
      [ e1 = SELF; "**"; e2 = SELF -> <:expr< $e1$ ** $e2$ >>
      | e1 = SELF; "asr"; e2 = SELF -> <:expr< $e1$ asr $e2$ >>
      | e1 = SELF; "lsl"; e2 = SELF -> <:expr< $e1$ lsl $e2$ >>
      | e1 = SELF; "lsr"; e2 = SELF -> <:expr< $e1$ lsr $e2$ >>
      | e1 = SELF; op = infixop4; e2 = SELF -> <:expr< $lid:op$ $e1$ $e2$ >> ]
    | "unary minus" NONA
      [ "-"; e = SELF -> <:expr< $mkumin _loc "-" e$ >>
      | "-."; e = SELF -> <:expr< $mkumin _loc "-." e$ >> ]
    | "apply" LEFTA
      [ e1 = SELF; e2 = SELF ->
          match constr_expr_arity _loc e1 with
          [ 1 -> <:expr< $e1$ $e2$ >>
          | _ ->
              match e2 with
              [ <:expr< ( $list:el$ ) >> ->
                  List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>) e1 el
              | _ -> <:expr< $e1$ $e2$ >> ] ]
      | "assert"; e = SELF ->
          match e with
          [ <:expr< False >> -> <:expr< assert False >>
          | _ -> <:expr< assert ($e$) >> ]
      | "lazy"; e = SELF ->
          <:expr< lazy ($e$) >> ]
    | "." LEFTA
      [ e1 = SELF; "."; "("; e2 = SELF; ")" -> <:expr< $e1$ .( $e2$ ) >>
      | e1 = SELF; "."; "["; e2 = SELF; "]" -> <:expr< $e1$ .[ $e2$ ] >>
      | e1 = SELF; "."; "{"; e2 = SELF; "}" -> bigarray_get _loc e1 e2
      | e1 = SELF; "."; e2 = SELF -> <:expr< $e1$ . $e2$ >> ]
    | "~-" NONA
      [ "!"; e = SELF -> <:expr< $e$ . val>>
      | "~-"; e = SELF -> <:expr< ~- $e$ >>
      | "~-."; e = SELF -> <:expr< ~-. $e$ >>
      | f = prefixop; e = SELF -> <:expr< $lid:f$ $e$ >> ]
    | "simple" LEFTA
      [ s = INT -> <:expr< $int:s$ >>
      | s = INT32 -> MLast.ExInt32 _loc s
      | s = INT64 -> MLast.ExInt64 _loc s
      | s = NATIVEINT -> MLast.ExNativeInt _loc s
      | s = FLOAT -> <:expr< $flo:s$ >>
      | s = STRING -> <:expr< $str:s$ >>
      | c = CHAR -> <:expr< $chr:c$ >>
      | UIDENT "True" -> <:expr< $uid:" True"$ >>
      | UIDENT "False" -> <:expr< $uid:" False"$ >>
      | ids = expr_ident -> mkexprident _loc ids
      | s = "false" -> <:expr< False >>
      | s = "true" -> <:expr< True >>
      | "["; "]" -> <:expr< [] >>
      | "["; el = expr1_semi_list; "]" -> <:expr< $mklistexp _loc None el$ >>
      | "[|"; "|]" -> <:expr< [| |] >>
      | "[|"; el = expr1_semi_list; "|]" -> <:expr< [| $list:el$ |] >>
      | "{"; test_label_eq; lel = lbl_expr_list; "}" ->
          <:expr< { $list:lel$ } >>
      | "{"; e = expr LEVEL "."; "with"; lel = lbl_expr_list; "}" ->
          <:expr< { ($e$) with $list:lel$ } >>
      | "("; ")" -> <:expr< () >>
      | "("; op = operator_rparen -> <:expr< $lid:op$ >>
      | "("; e = SELF; ":"; t = ctyp; ")" -> <:expr< ($e$ : $t$) >>
      | "("; e = SELF; ")" -> <:expr< $e$ >>
      | "begin"; e = SELF; "end" -> <:expr< $e$ >>
      | "begin"; "end" -> <:expr< () >>
      | x = LOCATE ->
          let x =
            try
              let i = String.index x ':' in
              ({Lexing.pos_fname = "";
                Lexing.pos_lnum = 0;
                Lexing.pos_bol = 0;
                Lexing.pos_cnum = int_of_string (String.sub x 0 i)},
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found | Failure _ -> (Token.nowhere, x) ]
          in
          Pcaml.handle_expr_locate _loc x
      | x = QUOTATION ->
          let x =
            try
              let i = String.index x ':' in
              (String.sub x 0 i,
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found -> ("", x) ]
          in
          Pcaml.handle_expr_quotation _loc x ] ]
  ;
  let_binding:
    [ [ p = patt; e = fun_binding ->
          match patt_lid p with
          [ Some (_loc, i, pl) ->
              let e =
                List.fold_left (fun e p -> <:expr< fun $p$ -> $e$ >>) e pl
              in
              (<:patt< $lid:i$ >>, e)
          | None -> (p, e) ] ] ]
  ;
  fun_binding:
    [ RIGHTA
      [ p = patt LEVEL "simple"; e = SELF -> <:expr< fun $p$ -> $e$ >>
      | "="; e = expr -> <:expr< $e$ >>
      | ":"; t = ctyp; "="; e = expr -> <:expr< ($e$ : $t$) >> 
      | ":>"; t = ctyp; "="; e = expr -> <:expr< ($e$ :> $t$) >> ] ]
  ;
  match_case:
    [ [ x1 = patt; w = OPT [ "when"; e = expr -> e ]; "->"; x2 = expr ->
          (x1, w, x2) ] ]
  ;
  lbl_expr_list:
    [ [ le = lbl_expr; ";"; lel = SELF -> [le :: lel]
      | le = lbl_expr; ";" -> [le]
      | le = lbl_expr -> [le] ] ]
  ;
  lbl_expr:
    [ [ i = patt_label_ident; "="; e = expr LEVEL "expr1" -> (i, e) ] ]
  ;
  expr1_semi_list:
    [ [ e = expr LEVEL "expr1"; ";"; el = SELF -> [e :: el]
      | e = expr LEVEL "expr1"; ";" -> [e]
      | e = expr LEVEL "expr1" -> [e] ] ]
  ;
  fun_def:
    [ RIGHTA
      [ p = patt LEVEL "simple"; e = SELF -> <:expr< fun $p$ -> $e$ >>
      | "->"; e = expr -> <:expr< $e$ >> ] ]
  ;
  expr_ident:
    [ RIGHTA
      [ i = LIDENT -> [ <:expr< $lid:i$ >> ]
      | i = UIDENT -> [ <:expr< $uid:i$ >> ]
      | i = UIDENT; "."; "("; j = operator_rparen ->
         [ <:expr< $uid:i$ >> ; <:expr< $lid:j$ >> ]
      | i = UIDENT; "."; j = SELF -> [ <:expr< $uid:i$ >> :: j ]
      ]
    ]
  ;
  (* Patterns *)
  patt:
    [ LEFTA
      [ p1 = SELF; "as"; i = LIDENT -> <:patt< ($p1$ as $lid:i$) >> ]
    | LEFTA
      [ p1 = SELF; "|"; p2 = SELF -> <:patt< $p1$ | $p2$ >> ]
    | [ p = SELF; ","; pl = LIST1 NEXT SEP "," ->
          <:patt< ( $list:[p :: pl]$) >> ]
    | NONA
      [ p1 = SELF; ".."; p2 = SELF -> <:patt< $p1$ .. $p2$ >> ]
    | RIGHTA
      [ p1 = SELF; "::"; p2 = SELF -> <:patt< [$p1$ :: $p2$] >> ]
    | LEFTA
      [ p1 = SELF; p2 = SELF ->
          match constr_patt_arity _loc p1 with
          [ 1 -> <:patt< $p1$ $p2$ >>
          | n ->
              let p2 =
                match p2 with
                [ <:patt< _ >> when n > 1 ->
                    let pl =
                      loop n where rec loop n =
                        if n = 0 then [] else [<:patt< _ >> :: loop (n - 1)]
                    in
                    <:patt< ( $list:pl$ ) >>
                | _ -> p2 ]
              in
              match p2 with
              [ <:patt< ( $list:pl$ ) >> ->
                  List.fold_left (fun p1 p2 -> <:patt< $p1$ $p2$ >>) p1 pl
              | _ -> <:patt< $p1$ $p2$ >> ] ] ]
    | LEFTA
      [ p1 = SELF; "."; p2 = SELF -> <:patt< $p1$ . $p2$ >> ]
    | "simple"
      [ s = LIDENT -> <:patt< $lid:s$ >>
      | s = UIDENT -> <:patt< $uid:s$ >>
      | s = INT -> <:patt< $int:s$ >>
      | s = INT32 -> MLast.PaInt32 _loc s
      | s = INT64 -> MLast.PaInt64 _loc s
      | s = NATIVEINT -> MLast.PaNativeInt _loc s
      | "-"; s = INT -> <:patt< $int:"-" ^ s$ >>
      | "-"; s = INT32 -> MLast.PaInt32 _loc ("-" ^ s)
      | "-"; s = INT64 -> MLast.PaInt64 _loc ("-" ^ s)
      | "-"; s = NATIVEINT -> MLast.PaNativeInt _loc ("-" ^ s)
      | "-"; s = FLOAT -> <:patt< $flo:"-" ^ s$ >>
      | s = FLOAT -> <:patt< $flo:s$ >>
      | s = STRING -> <:patt< $str:s$ >>
      | s = CHAR -> <:patt< $chr:s$ >>
      | UIDENT "True" -> <:patt< $uid:" True"$ >>
      | UIDENT "False" -> <:patt< $uid:" False"$ >>
      | s = "false" -> <:patt< False >>
      | s = "true" -> <:patt< True >>
      | "["; "]" -> <:patt< [] >>
      | "["; pl = patt_semi_list; "]" -> <:patt< $mklistpat _loc None pl$ >>
      | "[|"; "|]" -> <:patt< [| |] >>
      | "[|"; pl = patt_semi_list; "|]" -> <:patt< [| $list:pl$ |] >>
      | "{"; lpl = lbl_patt_list; "}" -> <:patt< { $list:lpl$ } >>
      | "("; ")" -> <:patt< () >>
      | "("; op = operator_rparen -> <:patt< $lid:op$ >>
      | "("; p = patt; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
      | "("; p = patt; ")" -> <:patt< $p$ >>
      | "_" -> <:patt< _ >>
      | "`"; s = ident -> <:patt< ` $s$ >>
      | "#"; t = mod_ident -> <:patt< # $list:t$ >>
      | x = LOCATE ->
          let x =
            try
              let i = String.index x ':' in
              ({Lexing.pos_fname = "";
                Lexing.pos_lnum = 0;
                Lexing.pos_bol = 0;
                Lexing.pos_cnum = int_of_string (String.sub x 0 i)},
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found | Failure _ -> (Token.nowhere, x) ]
          in
          Pcaml.handle_patt_locate _loc x
      | x = QUOTATION ->
          let x =
            try
              let i = String.index x ':' in
              (String.sub x 0 i,
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found -> ("", x) ]
          in
          Pcaml.handle_patt_quotation _loc x ] ]
  ;

  patt_semi_list:
    [ [ p = patt; ";"; pl = SELF -> [p :: pl]
      | p = patt; ";" -> [p]
      | p = patt -> [p] ] ]
  ;
  lbl_patt_list:
    [ [ le = lbl_patt; ";"; lel = SELF -> [le :: lel]
      | le = lbl_patt; ";" -> [le]
      | le = lbl_patt -> [le] ] ]
  ;
  lbl_patt:
    [ [ i = patt_label_ident; "="; p = patt -> (i, p) ] ]
  ;
  patt_label_ident:
    [ LEFTA
      [ p1 = SELF; "."; p2 = SELF -> <:patt< $p1$ . $p2$ >> ]
    | RIGHTA
      [ i = UIDENT -> <:patt< $uid:i$ >>
      | i = LIDENT -> <:patt< $lid:i$ >> ] ]
  ;
  (* Type declaration *)
  type_declaration:
    [ [ tpl = type_parameters; n = type_patt; "="; tk = type_kind;
        cl = LIST0 constrain ->
          (n, tpl, tk, cl)
      | tpl = type_parameters; n = type_patt; cl = LIST0 constrain ->
          (n, tpl, <:ctyp< '$choose_tvar tpl$ >>, cl) ] ]
  ;
  type_patt:
    [ [ n = LIDENT -> (_loc, n) ] ]
  ;
  constrain:
    [ [ "constraint"; t1 = ctyp; "="; t2 = ctyp -> (t1, t2) ] ]
  ;
  type_kind:
    [ [ "private"; tk = type_kind -> <:ctyp< private $tk$ >>
      | test_constr_decl; OPT "|";
        cdl = LIST1 constructor_declaration SEP "|" -> <:ctyp< [ $list:cdl$ ] >>
      | t = ctyp -> <:ctyp< $t$ >>
      | t = ctyp; "="; "private"; tk = type_kind ->
          <:ctyp< $t$ == private $tk$ >>
      | t = ctyp; "="; "{"; ldl = label_declarations; "}" ->
          <:ctyp< $t$ == { $list:ldl$ } >>
      | t = ctyp; "="; OPT "|"; cdl = LIST1 constructor_declaration SEP "|" ->
          <:ctyp< $t$ == [ $list:cdl$ ] >>
      | "{"; ldl = label_declarations; "}" ->
          <:ctyp< { $list:ldl$ } >> ] ]
  ;
  type_parameters:
    [ [ -> (* empty *) []
      | tp = type_parameter -> [tp]
      | "("; tpl = LIST1 type_parameter SEP ","; ")" -> tpl ] ]
  ;
  type_parameter:
    [ [ "'"; i = ident -> (i, (False, False))
      | "+"; "'"; i = ident -> (i, (True, False))
      | "-"; "'"; i = ident -> (i, (False, True)) ] ]
  ;
  constructor_declaration:
    [ [ ci = UIDENT; "of"; cal = LIST1 ctyp LEVEL "ctyp1" SEP "*" ->
          (_loc, ci, cal)
      | ci = UIDENT -> (_loc, ci, []) ] ]
  ;
  label_declarations:
    [ [ ld = label_declaration; ";"; ldl = SELF -> [ld :: ldl]
      | ld = label_declaration; ";" -> [ld]
      | ld = label_declaration -> [ld] ] ]
  ;
  label_declaration:
    [ [ i = LIDENT; ":"; t = poly_type -> (_loc, i, False, t)
      | "mutable"; i = LIDENT; ":"; t = poly_type -> (_loc, i, True, t) ] ]
  ;
  (* Core types *)
  ctyp:
    [ [ t1 = SELF; "as"; "'"; i = ident -> <:ctyp< $t1$ as '$i$ >> ]
    | "arrow" RIGHTA
      [ t1 = SELF; "->"; t2 = SELF -> <:ctyp< $t1$ -> $t2$ >> ]
    | "star"
      [ t = SELF; "*"; tl = LIST1 (ctyp LEVEL "ctyp1") SEP "*" ->
          <:ctyp< ( $list:[t :: tl]$ ) >> ]
    | "ctyp1"
      [ t1 = SELF; t2 = SELF -> <:ctyp< $t2$ $t1$ >> ]
    | "ctyp2"
      [ t1 = SELF; "."; t2 = SELF -> <:ctyp< $t1$ . $t2$ >>
      | t1 = SELF; "("; t2 = SELF; ")" -> <:ctyp< $t1$ $t2$ >> ]
    | "simple"
      [ "'"; i = ident -> <:ctyp< '$i$ >>
      | "_" -> <:ctyp< _ >>
      | i = LIDENT -> <:ctyp< $lid:i$ >>
      | i = UIDENT -> <:ctyp< $uid:i$ >>
      | "("; t = SELF; ","; tl = LIST1 ctyp SEP ","; ")";
        i = ctyp LEVEL "ctyp2" ->
          List.fold_left (fun c a -> <:ctyp< $c$ $a$ >>) i [t :: tl]
      | "("; t = SELF; ")" -> <:ctyp< $t$ >> ] ]
  ;
  (* Identifiers *)
  ident:
    [ [ i = LIDENT -> i
      | i = UIDENT -> i ] ]
  ;
  mod_ident:
    [ RIGHTA
      [ i = UIDENT -> [i]
      | i = LIDENT -> [i]
      | i = UIDENT; "."; j = SELF -> [i :: j] ] ]
  ;
  (* Miscellaneous *)
  direction_flag:
    [ [ "to" -> True
      | "downto" -> False ] ]
  ;
  (* Objects and Classes *)
  str_item:
    [ [ "class"; cd = LIST1 class_declaration SEP "and" ->
          <:str_item< class $list:cd$ >>
      | "class"; "type"; ctd = LIST1 class_type_declaration SEP "and" ->
          <:str_item< class type $list:ctd$ >> ] ]
  ;
  sig_item:
    [ [ "class"; cd = LIST1 class_description SEP "and" ->
          <:sig_item< class $list:cd$ >>
      | "class"; "type"; ctd = LIST1 class_type_declaration SEP "and" ->
          <:sig_item< class type $list:ctd$ >> ] ]
  ;
  (* Class expressions *)
  class_declaration:
    [ [ vf = OPT "virtual"; ctp = class_type_parameters; i = LIDENT;
        cfb = class_fun_binding ->
          {MLast.ciLoc = _loc; MLast.ciVir = o2b vf; MLast.ciPrm = ctp;
           MLast.ciNam = i; MLast.ciExp = cfb} ] ]
  ;
  class_fun_binding:
    [ [ "="; ce = class_expr -> ce
      | ":"; ct = class_type; "="; ce = class_expr ->
          <:class_expr< ($ce$ : $ct$) >>
      | p = patt LEVEL "simple"; cfb = SELF ->
          <:class_expr< fun $p$ -> $cfb$ >> ] ]
  ;
  class_type_parameters:
    [ [ -> (_loc, [])
      | "["; tpl = LIST1 type_parameter SEP ","; "]" -> (_loc, tpl) ] ]
  ;
  class_fun_def:
    [ [ p = patt LEVEL "simple"; "->"; ce = class_expr ->
          <:class_expr< fun $p$ -> $ce$ >>
      | p = labeled_patt; "->"; ce = class_expr ->
          <:class_expr< fun $p$ -> $ce$ >>
      | p = patt LEVEL "simple"; cfd = SELF ->
          <:class_expr< fun $p$ -> $cfd$ >>
      | p = labeled_patt; cfd = SELF ->
          <:class_expr< fun $p$ -> $cfd$ >> ] ]
  ;
  class_expr:
    [ "top"
      [ "fun"; cfd = class_fun_def -> cfd
      | "let"; rf = OPT "rec"; lb = LIST1 let_binding SEP "and"; "in";
        ce = SELF ->
          <:class_expr< let $opt:o2b rf$ $list:lb$ in $ce$ >> ]
    | "apply" LEFTA
      [ ce = SELF; e = expr LEVEL "label" ->
          <:class_expr< $ce$ $e$ >> ]
    | "simple"
      [ "["; ct = ctyp; ","; ctcl = LIST1 ctyp SEP ","; "]";
        ci = class_longident ->
          <:class_expr< $list:ci$ [ $list:[ct :: ctcl]$ ] >>
      | "["; ct = ctyp; "]"; ci = class_longident ->
          <:class_expr< $list:ci$ [ $ct$ ] >>
      | ci = class_longident -> <:class_expr< $list:ci$ >>
      | "object"; cspo = OPT class_self_patt; cf = class_structure; "end" ->
          <:class_expr< object $opt:cspo$ $list:cf$ end >>
      | "("; ce = SELF; ":"; ct = class_type; ")" ->
          <:class_expr< ($ce$ : $ct$) >>
      | "("; ce = SELF; ")" -> ce ] ]
  ;
  class_structure:
    [ [ cf = LIST0 class_str_item -> cf ] ]
  ;
  class_self_patt:
    [ [ "("; p = patt; ")" -> p
      | "("; p = patt; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >> ] ]
  ;
  class_str_item:
    [ [ "inherit"; ce = class_expr; pb = OPT [ "as"; i = LIDENT -> i ] ->
          <:class_str_item< inherit $ce$ $opt:pb$ >>
      | "val"; mf = OPT "mutable"; lab = label; e = cvalue_binding ->
          <:class_str_item< value $opt:o2b mf$ $lab$ = $e$ >>
      | "method"; "private"; "virtual"; l = label; ":"; t = poly_type ->
          <:class_str_item< method virtual private $l$ : $t$ >>
      | "method"; "virtual"; "private"; l = label; ":"; t = poly_type ->
          <:class_str_item< method virtual private $l$ : $t$ >>
      | "method"; "virtual"; l = label; ":"; t = poly_type ->
          <:class_str_item< method virtual $l$ : $t$ >>
      | "method"; "private"; l = label; ":"; t = poly_type; "="; e = expr ->
          MLast.CrMth _loc l True e (Some t)
      | "method"; "private"; l = label; sb = fun_binding ->
          MLast.CrMth _loc l True sb None
      | "method"; l = label; ":"; t = poly_type; "="; e = expr ->
          MLast.CrMth _loc l False e (Some t)
      | "method"; l = label; sb = fun_binding ->
          MLast.CrMth _loc l False sb None
      | "constraint"; t1 = ctyp; "="; t2 = ctyp ->
          <:class_str_item< type $t1$ = $t2$ >>
      | "initializer"; se = expr -> <:class_str_item< initializer $se$ >> ] ]
  ;
  cvalue_binding:
    [ [ "="; e = expr -> e
      | ":"; t = ctyp; "="; e = expr -> <:expr< ($e$ : $t$) >>
      | ":"; t = ctyp; ":>"; t2 = ctyp; "="; e = expr ->
          <:expr< ($e$ : $t$ :> $t2$) >>
      | ":>"; t = ctyp; "="; e = expr ->
          <:expr< ($e$ :> $t$) >> ] ]
  ;
  label:
    [ [ i = LIDENT -> i ] ]
  ;
  (* Class types *)
  class_type:
    [ [ test_ctyp_minusgreater; t = ctyp LEVEL "star"; "->"; ct = SELF ->
          <:class_type< [ $t$ ] -> $ct$ >>
      | cs = class_signature -> cs ] ]
  ;
  class_signature:
    [ [ "["; tl = LIST1 ctyp SEP ","; "]"; id = clty_longident ->
          <:class_type< $list:id$ [ $list:tl$ ] >>
      | id = clty_longident -> <:class_type< $list:id$ >>
      | "object"; cst = OPT class_self_type; csf = LIST0 class_sig_item;
        "end" ->
          <:class_type< object $opt:cst$ $list:csf$ end >> ] ]
  ;
  class_self_type:
    [ [ "("; t = ctyp; ")" -> t ] ]
  ;
  class_sig_item:
    [ [ "inherit"; cs = class_signature -> <:class_sig_item< inherit $cs$ >>
      | "val"; mf = OPT "mutable"; l = label; ":"; t = ctyp ->
          <:class_sig_item< value $opt:o2b mf$ $l$ : $t$ >>
      | "method"; "private"; "virtual"; l = label; ":"; t = poly_type ->
          <:class_sig_item< method virtual private $l$ : $t$ >>
      | "method"; "virtual"; "private"; l = label; ":"; t = poly_type ->
          <:class_sig_item< method virtual private $l$ : $t$ >>
      | "method"; "virtual"; l = label; ":"; t = poly_type ->
          <:class_sig_item< method virtual $l$ : $t$ >>
      | "method"; "private"; l = label; ":"; t = poly_type ->
          <:class_sig_item< method private $l$ : $t$ >>
      | "method"; l = label; ":"; t = poly_type ->
          <:class_sig_item< method $l$ : $t$ >>
      | "constraint"; t1 = ctyp; "="; t2 = ctyp ->
          <:class_sig_item< type $t1$ = $t2$ >> ] ]
  ;
  class_description:
    [ [ vf = OPT "virtual"; ctp = class_type_parameters; n = LIDENT; ":";
        ct = class_type ->
          {MLast.ciLoc = _loc; MLast.ciVir = o2b vf; MLast.ciPrm = ctp;
           MLast.ciNam = n; MLast.ciExp = ct} ] ]
  ;
  class_type_declaration:
    [ [ vf = OPT "virtual"; ctp = class_type_parameters; n = LIDENT; "=";
        cs = class_signature ->
          {MLast.ciLoc = _loc; MLast.ciVir = o2b vf; MLast.ciPrm = ctp;
           MLast.ciNam = n; MLast.ciExp = cs} ] ]
  ;
  (* Expressions *)
  expr: LEVEL "simple"
    [ LEFTA
      [ "new"; i = class_longident -> <:expr< new $list:i$ >> ] ]
  ;
  expr: LEVEL "."
    [ [ e = SELF; "#"; lab = label -> <:expr< $e$ # $lab$ >> ] ]
  ;
  expr: LEVEL "simple"
    [ [ "("; e = SELF; ":"; t = ctyp; ":>"; t2 = ctyp; ")" ->
          <:expr< ($e$ : $t$ :> $t2$) >>
      | "("; e = SELF; ":>"; t = ctyp; ")" -> <:expr< ($e$ :> $t$) >>
      | "{<"; ">}" -> <:expr< {< >} >>
      | "{<"; fel = field_expr_list; ">}" -> <:expr< {< $list:fel$ >} >> ] ]
  ;
  field_expr_list:
    [ [ l = label; "="; e = expr LEVEL "expr1"; ";"; fel = SELF ->
          [(l, e) :: fel]
      | l = label; "="; e = expr LEVEL "expr1"; ";" -> [(l, e)]
      | l = label; "="; e = expr LEVEL "expr1" -> [(l, e)] ] ]
  ;
  (* Core types *)
  ctyp: LEVEL "simple"
    [ [ "#"; id = class_longident -> <:ctyp< # $list:id$ >>
      | "<"; (ml, v) = meth_list; ">" -> <:ctyp< < $list:ml$ $opt:v$ > >>
      | "<"; ">" -> <:ctyp< < > >> ] ]
  ;
  meth_list:
    [ [ f = field; ";"; (ml, v) = SELF -> ([f :: ml], v)
      | f = field; ";" -> ([f], False)
      | f = field -> ([f], False)
      | ".." -> ([], True) ] ]
  ;
  field:
    [ [ lab = LIDENT; ":"; t = poly_type -> (lab, t) ] ]
  ;
  (* Polymorphic types *)
  typevar:
    [ [ "'"; i = ident -> i ] ]
  ;
  poly_type:
    [ [ test_typevar_list_dot; tpl = LIST1 typevar; "."; t2 = ctyp ->
          <:ctyp< ! $list:tpl$ . $t2$ >>
      | t = ctyp -> t ] ]
  ;
  (* Identifiers *)
  clty_longident:
    [ [ m = UIDENT; "."; l = SELF -> [m :: l]
      | i = LIDENT -> [i] ] ]
  ;
  class_longident:
    [ [ m = UIDENT; "."; l = SELF -> [m :: l]
      | i = LIDENT -> [i] ] ]
  ;
  (* Labels *)
  ctyp: LEVEL "arrow"
    [ RIGHTA
      [ i = lident_colon; t1 = ctyp LEVEL "star"; "->"; t2 = SELF ->
          <:ctyp< ( ~ $i$ : $t1$ ) -> $t2$ >>
      | i = OPTLABEL; t1 = ctyp LEVEL "star"; "->"; t2 = SELF ->
          <:ctyp< ( ? $i$ : $t1$ ) -> $t2$ >>
      | i = QUESTIONIDENT; ":"; t1 = ctyp LEVEL "star"; "->"; t2 = SELF ->
          <:ctyp< ( ? $i$ : $t1$ ) -> $t2$ >>
      | "?"; i=lident_colon;t1 = ctyp LEVEL "star"; "->"; t2 = SELF ->
          <:ctyp< ( ? $i$ : $t1$ ) -> $t2$ >> ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ "["; OPT "|"; rfl = LIST1 row_field SEP "|"; "]" ->
          <:ctyp< [ = $list:rfl$ ] >>
      | "["; ">"; "]" -> <:ctyp< [ > $list:[]$ ] >>
      | "["; ">"; OPT "|"; rfl = LIST1 row_field SEP "|"; "]" ->
          <:ctyp< [ > $list:rfl$ ] >>
      | "[<"; OPT "|"; rfl = LIST1 row_field SEP "|"; "]" ->
          <:ctyp< [ < $list:rfl$ ] >>
      | "[<"; OPT "|"; rfl = LIST1 row_field SEP "|"; ">";
        ntl = LIST1 name_tag; "]" ->
          <:ctyp< [ < $list:rfl$ > $list:ntl$ ] >> ] ]
  ;
  row_field:
    [ [ "`"; i = ident -> MLast.RfTag i True []
      | "`"; i = ident; "of"; ao = OPT "&"; l = LIST1 ctyp SEP "&" ->
          MLast.RfTag i (o2b ao) l
      | t = ctyp -> MLast.RfInh t ] ]
  ;
  name_tag:
    [ [ "`"; i = ident -> i ] ]
  ;
  expr: LEVEL "expr1"
    [ [ "fun"; p = labeled_patt; e = fun_def -> <:expr< fun $p$ -> $e$ >> ] ]
  ;
  expr: AFTER "apply"
    [ "label"
      [ i = LABEL; e = SELF -> <:expr< ~ $i$ : $e$ >>
      | i = TILDEIDENT -> <:expr< ~ $i$ >>
      | "~"; i = LIDENT -> <:expr< ~ $i$ >>
      | i = OPTLABEL; e = SELF -> <:expr< ? $i$ : $e$ >>
      | i = QUESTIONIDENT -> <:expr< ? $i$ >> 
      | "?"; i = LIDENT -> <:expr< ? $i$ >> ] ]
  ;
  expr: LEVEL "simple"
    [ [ "`"; s = ident -> <:expr< ` $s$ >> ] ]
  ;
  fun_def:
    [ [ p = labeled_patt; e = SELF -> <:expr< fun $p$ -> $e$ >> ] ]
  ;
  fun_binding:
    [ [ p = labeled_patt; e = SELF -> <:expr< fun $p$ -> $e$ >> ] ]
  ;
  labeled_patt:
    [ [ i = LABEL; p = patt LEVEL "simple" ->
           <:patt< ~ $i$ : $p$ >>
      | i = TILDEIDENT ->
           <:patt< ~ $i$ >>
      | "~"; i=LIDENT -> <:patt< ~ $i$ >>
      | "~"; "("; i = LIDENT; ")" ->
           <:patt< ~ $i$ >>
      | "~"; "("; i = LIDENT; ":"; t = ctyp; ")" ->
           <:patt< ~ $i$ : ($lid:i$ : $t$) >>
      | i = OPTLABEL; j = LIDENT ->
           <:patt< ? $i$ : ($lid:j$) >>
      | i = OPTLABEL; "("; p = patt; "="; e = expr; ")" ->
          <:patt< ? $i$ : ( $p$ = $e$ ) >>
      | i = OPTLABEL; "("; p = patt; ":"; t = ctyp; ")" ->
          <:patt< ? $i$ : ( $p$ : $t$ ) >>
      | i = OPTLABEL; "("; p = patt; ":"; t = ctyp; "=";
        e = expr; ")" ->
          <:patt< ? $i$ : ( $p$ : $t$ = $e$ ) >>
      | i = QUESTIONIDENT -> <:patt< ? $i$ >>
      | "?"; i = LIDENT -> <:patt< ? $i$ >>
      | "?"; "("; i = LIDENT; "="; e = expr; ")" ->
          <:patt< ? ( $lid:i$ = $e$ ) >>
      | "?"; "("; i = LIDENT; ":"; t = ctyp; "="; e = expr; ")" ->
          <:patt< ? ( $lid:i$ : $t$ = $e$ ) >>
      | "?"; "("; i = LIDENT; ")" ->
          <:patt< ? $i$ >>
      | "?"; "("; i = LIDENT; ":"; t = ctyp; ")" ->
          <:patt< ? ( $lid:i$ : $t$ ) >> ] ]
  ;
  class_type:
    [ [ i = lident_colon; t = ctyp LEVEL "star"; "->"; ct = SELF ->
          <:class_type< [ ~ $i$ : $t$ ] -> $ct$ >>
      | i = OPTLABEL; t = ctyp LEVEL "star"; "->"; ct = SELF ->
          <:class_type< [ ? $i$ : $t$ ] -> $ct$ >> 
      | i = QUESTIONIDENT; ":"; t = ctyp LEVEL "star"; "->"; ct = SELF ->
          <:class_type< [ ? $i$ : $t$ ] -> $ct$ >> 
      | "?"; i = LIDENT; ":"; t = ctyp LEVEL "star"; "->"; ct = SELF ->
          <:class_type< [ ? $i$ : $t$ ] -> $ct$ >> ] ]
  ;
  class_fun_binding:
    [ [ p = labeled_patt; cfb = SELF -> <:class_expr< fun $p$ -> $cfb$ >> ] ]
  ;
END;

(* Main entry points *)

EXTEND
  GLOBAL: interf implem use_file top_phrase expr patt;
  interf:
    [ [ si = sig_item_semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
      | "#"; n = LIDENT; dp = OPT expr; ";;" ->
          ([(<:sig_item< # $n$ $opt:dp$ >>, _loc)], True)
      | EOI -> ([], False) ] ]
  ;
  sig_item_semi:
    [ [ si = sig_item; OPT ";;" -> (si, _loc) ] ]
  ;
  implem:
    [ [ si = str_item_semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
      | "#"; n = LIDENT; dp = OPT expr; ";;" ->
          ([(<:str_item< # $n$ $opt:dp$ >>, _loc)], True)
      | EOI -> ([], False) ] ]
  ;
  str_item_semi:
    [ [ si = str_item; OPT ";;" -> (si, _loc) ] ]
  ;
  top_phrase:
    [ [ ph = phrase; ";;" -> Some ph
      | EOI -> None ] ]
  ;
  use_file:
    [ [ si = str_item; OPT ";;"; (sil, stopped) = SELF ->
          ([si :: sil], stopped)
      | "#"; n = LIDENT; dp = OPT expr; ";;" ->
          ([<:str_item< # $n$ $opt:dp$ >>], True)
      | EOI -> ([], False) ] ]
  ;
  phrase:
    [ [ sti = str_item -> sti
      | "#"; n = LIDENT; dp = OPT expr -> <:str_item< # $n$ $opt:dp$ >> ] ]
  ;
END;
