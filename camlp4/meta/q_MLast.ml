(* camlp4r pa_extend.cmo pa_extend_m.cmo q_MLast.cmo *)
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

value (gram, q_position) =
  let (lexer,pos) = Plexer.make_lexer () in
  (Grammar.gcreate lexer, pos)
;

module Qast =
  struct
    type t =
      [ Node of string and list t
      | List of list t
      | Tuple of list t
      | Option of option t
      | Int of string
      | Str of string
      | Bool of bool
      | Cons of t and t
      | Apply of string and list t
      | Record of list (string * t)
      | Loc
      | Antiquot of MLast.loc and string ]
    ;
    value _loc =
        let nowhere =
          {(Lexing.dummy_pos) with Lexing.pos_lnum = 1; Lexing.pos_cnum = 0 } in
          (nowhere,nowhere);
    value rec to_expr =
      fun
      [ Node n al ->
          List.fold_left (fun e a -> <:expr< $e$ $to_expr a$ >>)
            <:expr< MLast.$uid:n$ >> al
      | List al ->
          List.fold_right (fun a e -> <:expr< [$to_expr a$ :: $e$] >>) al
            <:expr< [] >>
      | Tuple al -> <:expr< ($list:List.map to_expr al$) >>
      | Option None -> <:expr< None >>
      | Option (Some a) -> <:expr< Some $to_expr a$ >>
      | Int s -> <:expr< $int:s$ >>
      | Str s -> <:expr< $str:s$ >>
      | Bool True -> <:expr< True >>
      | Bool False -> <:expr< False >>
      | Cons a1 a2 -> <:expr< [$to_expr a1$ :: $to_expr a2$] >>
      | Apply f al ->
          List.fold_left (fun e a -> <:expr< $e$ $to_expr a$ >>)
            <:expr< $lid:f$ >> al
      | Record lal -> <:expr< {$list:List.map to_expr_label lal$} >>
      | Loc -> <:expr< $lid:Stdpp.loc_name.val$ >>
      | Antiquot loc s ->
          let (bolpos,lnum, _) = Pcaml.position.val in
          let (bolposv,lnumv) = (bolpos.val, lnum.val) in
          let zero_pos () = do { bolpos.val := 0; lnum.val := 1 } in
          let restore_pos () = do { bolpos.val := bolposv; lnum.val := lnumv } in
          let e =
            try
              let _ = zero_pos() in
              let result = Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string s) in
              let _ = restore_pos() in
              result
            with
            [ Stdpp.Exc_located (bp, ep) exc ->
                do { restore_pos() ; raise (Stdpp.Exc_located (Reloc.adjust_loc (fst loc) (bp,ep)) exc) }
            | exc -> do { restore_pos(); raise exc } ]
          in
          <:expr< $anti:e$ >> ]
    and to_expr_label (l, a) = (<:patt< MLast.$lid:l$ >>, to_expr a);
    value rec to_patt =
      fun
      [ Node n al ->
          List.fold_left (fun e a -> <:patt< $e$ $to_patt a$ >>)
            <:patt< MLast.$uid:n$ >> al
      | List al ->
          List.fold_right (fun a p -> <:patt< [$to_patt a$ :: $p$] >>) al
            <:patt< [] >>
      | Tuple al -> <:patt< ($list:List.map to_patt al$) >>
      | Option None -> <:patt< None >>
      | Option (Some a) -> <:patt< Some $to_patt a$ >>
      | Int s -> <:patt< $int:s$ >>
      | Str s -> <:patt< $str:s$ >>
      | Bool True -> <:patt< True >>
      | Bool False -> <:patt< False >>
      | Cons a1 a2 -> <:patt< [$to_patt a1$ :: $to_patt a2$] >>
      | Apply _ _ -> failwith "bad pattern"
      | Record lal -> <:patt< {$list:List.map to_patt_label lal$} >>
      | Loc -> <:patt< _ >>
      | Antiquot loc s ->
          let (bolpos,lnum, _) = Pcaml.position.val in
          let (bolposv,lnumv) = (bolpos.val, lnum.val) in
          let zero_pos () = do { bolpos.val := 0; lnum.val := 1 } in
          let restore_pos () = do { bolpos.val := bolposv; lnum.val := lnumv } in
          let p =
            try
              let _ = zero_pos() in
              let result = Grammar.Entry.parse Pcaml.patt_eoi (Stream.of_string s) in
              let _ = restore_pos() in
              result
             with
            [ Stdpp.Exc_located (bp, ep) exc ->
                do { restore_pos() ; raise (Stdpp.Exc_located (Reloc.adjust_loc (fst loc) (bp, ep)) exc) }
            | exc -> do { restore_pos(); raise exc } ]
          in
          <:patt< $anti:p$ >> ]
    and to_patt_label (l, a) = (<:patt< MLast.$lid:l$ >>, to_patt a);
  end
;

value antiquot k (bp, ep) x =
  let shift =
    if k = "" then String.length "$"
    else String.length "$" + String.length k + String.length ":"
  in
  Qast.Antiquot (Reloc.shift_pos shift bp, Reloc.shift_pos (-1) ep) x
;

value sig_item = Grammar.Entry.create gram "signature item";
value str_item = Grammar.Entry.create gram "structure item";
value ctyp = Grammar.Entry.create gram "type";
value patt = Grammar.Entry.create gram "pattern";
value expr = Grammar.Entry.create gram "expression";

value module_type = Grammar.Entry.create gram "module type";
value module_expr = Grammar.Entry.create gram "module expression";

value class_type = Grammar.Entry.create gram "class type";
value class_expr = Grammar.Entry.create gram "class expr";
value class_sig_item = Grammar.Entry.create gram "class signature item";
value class_str_item = Grammar.Entry.create gram "class structure item";

value ipatt = Grammar.Entry.create gram "ipatt";
value let_binding = Grammar.Entry.create gram "let_binding";
value type_declaration = Grammar.Entry.create gram "type_declaration";
value with_constr = Grammar.Entry.create gram "with_constr";
value row_field = Grammar.Entry.create gram "row_field";

value a_list = Grammar.Entry.create gram "a_list";
value a_opt = Grammar.Entry.create gram "a_opt";
value a_UIDENT = Grammar.Entry.create gram "a_UIDENT";
value a_LIDENT = Grammar.Entry.create gram "a_LIDENT";
value a_INT = Grammar.Entry.create gram "a_INT";
value a_INT32 = Grammar.Entry.create gram "a_INT32";
value a_INT64 = Grammar.Entry.create gram "a_INT64";
value a_NATIVEINT = Grammar.Entry.create gram "a__NATIVEINT";
value a_FLOAT = Grammar.Entry.create gram "a_FLOAT";
value a_STRING = Grammar.Entry.create gram "a_STRING";
value a_CHAR = Grammar.Entry.create gram "a_CHAR";
value a_TILDEIDENT = Grammar.Entry.create gram "a_TILDEIDENT";
value a_LABEL = Grammar.Entry.create gram "a_LABEL";
value a_QUESTIONIDENT = Grammar.Entry.create gram "a_QUESTIONIDENT";
value a_OPTLABEL = Grammar.Entry.create gram "a_OPTLABEL";

value o2b =
  fun
  [ Qast.Option (Some _) -> Qast.Bool True
  | Qast.Option None -> Qast.Bool False
  | x -> x ]
;

value mksequence _ =
  fun
  [ Qast.List [e] -> e
  | el -> Qast.Node "ExSeq" [Qast.Loc; el] ]
;

value mkmatchcase _ p aso w e =
  let p =
    match aso with
    [ Qast.Option (Some p2) -> Qast.Node "PaAli" [Qast.Loc; p; p2]
    | Qast.Option None -> p
    | _ -> Qast.Node "PaAli" [Qast.Loc; p; aso] ]
  in
  Qast.Tuple [p; w; e]
;

value neg_string n =
  let len = String.length n in
  if len > 0 && n.[0] = '-' then String.sub n 1 (len - 1)
  else "-" ^ n
;

value mkumin _ f arg =
  match arg with
  [ Qast.Node (("ExInt" | "ExInt32" | "ExInt64" | "ExNativeInt") as exi)
      [Qast.Loc; Qast.Str n] when int_of_string n > 0 ->
        let n = neg_string n in
        Qast.Node exi [Qast.Loc; Qast.Str n]
  | Qast.Node "ExFlo" [Qast.Loc; Qast.Str n] when float_of_string n > 0.0 ->
      let n = neg_string n in
      Qast.Node "ExFlo" [Qast.Loc; Qast.Str n]
  | _ ->
      match f with
      [ Qast.Str f ->
          let f = "~" ^ f in
          Qast.Node "ExApp"
            [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str f]; arg]
      | _ -> assert False ] ]
;

value mkuminpat _ f is_int s =
  let s =
    match s with
    [ Qast.Str s -> Qast.Str (neg_string s)
    | s -> failwith "bad unary minus" ]
  in
  match is_int with
  [ Qast.Bool True -> Qast.Node "PaInt" [Qast.Loc; s]
  | Qast.Bool False -> Qast.Node "PaFlo" [Qast.Loc; s]
  | _ -> assert False ]
;

value mklistexp _ last =
  loop True where rec loop top =
    fun
    [ Qast.List [] ->
        match last with
        [ Qast.Option (Some e) -> e
        | Qast.Option None -> Qast.Node "ExUid" [Qast.Loc; Qast.Str "[]"]
        | a -> a ]
    | Qast.List [e1 :: el] ->
        Qast.Node "ExApp"
          [Qast.Loc;
           Qast.Node "ExApp"
             [Qast.Loc; Qast.Node "ExUid" [Qast.Loc; Qast.Str "::"]; e1];
           loop False (Qast.List el)]
    | a -> a ]
;

value mklistpat _ last =
  loop True where rec loop top =
    fun
    [ Qast.List [] ->
        match last with
        [ Qast.Option (Some p) -> p
        | Qast.Option None -> Qast.Node "PaUid" [Qast.Loc; Qast.Str "[]"]
        | a -> a ]
    | Qast.List [p1 :: pl] ->
        Qast.Node "PaApp"
          [Qast.Loc;
           Qast.Node "PaApp"
             [Qast.Loc; Qast.Node "PaUid" [Qast.Loc; Qast.Str "::"]; p1];
           loop False (Qast.List pl)]
    | a -> a ]
;

value mkexprident loc i j =
  loop (Qast.Node "ExUid" [Qast.Loc; i]) j where rec loop m =
    fun
    [ Qast.Node "ExAcc" [_; x; y] ->
        loop (Qast.Node "ExAcc" [Qast.Loc; m; x]) y
    | e -> Qast.Node "ExAcc" [Qast.Loc; m; e] ]
;

value mkassert _ e =
  match e with
  [ Qast.Node "ExUid" [_; Qast.Str "False"] -> Qast.Node "ExAsf" [Qast.Loc]
  | _ -> Qast.Node "ExAsr" [Qast.Loc; e] ]
;

value append_elem el e = Qast.Apply "@" [el; Qast.List [e]];

value not_yet_warned_antiq = ref True;
value warn_antiq loc vers =
  if not_yet_warned_antiq.val then do {
    not_yet_warned_antiq.val := False;
    Pcaml.warning.val loc
      (Printf.sprintf
         "use of antiquotation syntax deprecated since version %s" vers);
  }
  else ()
;

value not_yet_warned_variant = ref True;
value warn_variant _ =
  if not_yet_warned_variant.val then do {
    not_yet_warned_variant.val := False;
    Pcaml.warning.val (Lexing.dummy_pos, Reloc.shift_pos 1 Lexing.dummy_pos)
      (Printf.sprintf
         "use of syntax of variants types deprecated since version 3.05");
  }
  else ()
;

value not_yet_warned_seq = ref True;
value warn_sequence _ =
  if not_yet_warned_seq.val then do {
    not_yet_warned_seq.val := False;
    Pcaml.warning.val (Lexing.dummy_pos, Reloc.shift_pos 1 Lexing.dummy_pos)
      (Printf.sprintf
         "use of syntax of sequences deprecated since version 3.01.1");
  }
  else ()
;

EXTEND
  GLOBAL: sig_item str_item ctyp patt expr module_type module_expr class_type
    class_expr class_sig_item class_str_item let_binding type_declaration
    ipatt with_constr row_field;
  module_expr:
    [ [ "functor"; "("; i = a_UIDENT; ":"; t = module_type; ")"; "->";
        me = SELF ->
          Qast.Node "MeFun" [Qast.Loc; i; t; me]
      | "struct"; st = SLIST0 [ s = str_item; ";" -> s ]; "end" ->
          Qast.Node "MeStr" [Qast.Loc; st] ]
    | [ me1 = SELF; me2 = SELF -> Qast.Node "MeApp" [Qast.Loc; me1; me2] ]
    | [ me1 = SELF; "."; me2 = SELF ->
          Qast.Node "MeAcc" [Qast.Loc; me1; me2] ]
    | "simple"
      [ i = a_UIDENT -> Qast.Node "MeUid" [Qast.Loc; i]
      | "("; me = SELF; ":"; mt = module_type; ")" ->
          Qast.Node "MeTyc" [Qast.Loc; me; mt]
      | "("; me = SELF; ")" -> me ] ]
  ;
  str_item:
    [ "top"
      [ "declare"; st = SLIST0 [ s = str_item; ";" -> s ]; "end" ->
          Qast.Node "StDcl" [Qast.Loc; st]
      | "exception"; ctl = constructor_declaration; b = rebind_exn ->
          let (_, c, tl) =
            match ctl with
            [ Qast.Tuple [xx1; xx2; xx3] -> (xx1, xx2, xx3)
            | _ -> match () with [] ]
          in
          Qast.Node "StExc" [Qast.Loc; c; tl; b]
      | "external"; i = a_LIDENT; ":"; t = ctyp; "="; pd = SLIST1 a_STRING ->
          Qast.Node "StExt" [Qast.Loc; i; t; pd]
      | "include"; me = module_expr -> Qast.Node "StInc" [Qast.Loc; me]
      | "module"; i = a_UIDENT; mb = module_binding ->
          Qast.Node "StMod" [Qast.Loc; i; mb]
      | "module"; "rec"; nmtmes = SLIST1 module_rec_binding SEP "and" ->
          Qast.Node "StRecMod" [Qast.Loc; nmtmes]
      | "module"; "type"; i = a_UIDENT; "="; mt = module_type ->
          Qast.Node "StMty" [Qast.Loc; i; mt]
      | "open"; i = mod_ident -> Qast.Node "StOpn" [Qast.Loc; i]
      | "type"; tdl = SLIST1 type_declaration SEP "and" ->
          Qast.Node "StTyp" [Qast.Loc; tdl]
      | "value"; r = SOPT "rec"; l = SLIST1 let_binding SEP "and" ->
          Qast.Node "StVal" [Qast.Loc; o2b r; l]
      | e = expr -> Qast.Node "StExp" [Qast.Loc; e] ] ]
  ;
  rebind_exn:
    [ [ "="; sl = mod_ident -> sl
      | -> Qast.List [] ] ]
  ;
  module_binding:
    [ RIGHTA
      [ "("; m = a_UIDENT; ":"; mt = module_type; ")"; mb = SELF ->
          Qast.Node "MeFun" [Qast.Loc; m; mt; mb]
      | ":"; mt = module_type; "="; me = module_expr ->
          Qast.Node "MeTyc" [Qast.Loc; me; mt]
      | "="; me = module_expr -> me ] ]
  ;
  module_rec_binding:
    [ [ m = a_UIDENT; ":"; mt = module_type; "="; me = module_expr ->
          Qast.Tuple [m; me; mt] ] ]
  ;
  module_type:
    [ [ "functor"; "("; i = a_UIDENT; ":"; t = SELF; ")"; "->"; mt = SELF ->
          Qast.Node "MtFun" [Qast.Loc; i; t; mt] ]
    | [ mt = SELF; "with"; wcl = SLIST1 with_constr SEP "and" ->
          Qast.Node "MtWit" [Qast.Loc; mt; wcl] ]
    | [ "sig"; sg = SLIST0 [ s = sig_item; ";" -> s ]; "end" ->
          Qast.Node "MtSig" [Qast.Loc; sg] ]
    | [ m1 = SELF; m2 = SELF -> Qast.Node "MtApp" [Qast.Loc; m1; m2] ]
    | [ m1 = SELF; "."; m2 = SELF -> Qast.Node "MtAcc" [Qast.Loc; m1; m2] ]
    | "simple"
      [ i = a_UIDENT -> Qast.Node "MtUid" [Qast.Loc; i]
      | i = a_LIDENT -> Qast.Node "MtLid" [Qast.Loc; i]
      | "'"; i = ident -> Qast.Node "MtQuo" [Qast.Loc; i]
      | "("; mt = SELF; ")" -> mt ] ]
  ;
  sig_item:
    [ "top"
      [ "declare"; st = SLIST0 [ s = sig_item; ";" -> s ]; "end" ->
          Qast.Node "SgDcl" [Qast.Loc; st]
      | "exception"; ctl = constructor_declaration ->
          let (_, c, tl) =
            match ctl with
            [ Qast.Tuple [xx1; xx2; xx3] -> (xx1, xx2, xx3)
            | _ -> match () with [] ]
          in
          Qast.Node "SgExc" [Qast.Loc; c; tl]
      | "external"; i = a_LIDENT; ":"; t = ctyp; "="; pd = SLIST1 a_STRING ->
          Qast.Node "SgExt" [Qast.Loc; i; t; pd]
      | "include"; mt = module_type -> Qast.Node "SgInc" [Qast.Loc; mt]
      | "module"; i = a_UIDENT; mt = module_declaration ->
          Qast.Node "SgMod" [Qast.Loc; i; mt]
      | "module"; "type"; i = a_UIDENT; "="; mt = module_type ->
          Qast.Node "SgMty" [Qast.Loc; i; mt]
      | "module"; "rec"; mds = SLIST1 module_rec_declaration SEP "and" ->
          Qast.Node "SgRecMod" [Qast.Loc; mds]
      | "open"; i = mod_ident -> Qast.Node "SgOpn" [Qast.Loc; i]
      | "type"; tdl = SLIST1 type_declaration SEP "and" ->
          Qast.Node "SgTyp" [Qast.Loc; tdl]
      | "value"; i = a_LIDENT; ":"; t = ctyp ->
          Qast.Node "SgVal" [Qast.Loc; i; t] ] ]
  ;
  module_declaration:
    [ RIGHTA
      [ ":"; mt = module_type -> mt
      | "("; i = a_UIDENT; ":"; t = module_type; ")"; mt = SELF ->
          Qast.Node "MtFun" [Qast.Loc; i; t; mt] ] ]
  ;
  module_rec_declaration:
    [ [ m = a_UIDENT; ":"; mt = module_type -> Qast.Tuple [m; mt] ] ]
  ;
  with_constr:
    [ [ "type"; i = mod_ident; tpl = SLIST0 type_parameter; "="; t = ctyp ->
          Qast.Node "WcTyp" [Qast.Loc; i; tpl; t]
      | "module"; i = mod_ident; "="; me = module_expr ->
          Qast.Node "WcMod" [Qast.Loc; i; me] ] ]
  ;
  expr:
    [ "top" RIGHTA
      [ "let"; r = SOPT "rec"; l = SLIST1 let_binding SEP "and"; "in";
        x = SELF ->
          Qast.Node "ExLet" [Qast.Loc; o2b r; l; x]
      | "let"; "module"; m = a_UIDENT; mb = module_binding; "in"; e = SELF ->
          Qast.Node "ExLmd" [Qast.Loc; m; mb; e]
      | "fun"; "["; l = SLIST0 match_case SEP "|"; "]" ->
          Qast.Node "ExFun" [Qast.Loc; l]
      | "fun"; p = ipatt; e = fun_def ->
          Qast.Node "ExFun"
            [Qast.Loc; Qast.List [Qast.Tuple [p; Qast.Option None; e]]]
      | "match"; e = SELF; "with"; "["; l = SLIST0 match_case SEP "|"; "]" ->
          Qast.Node "ExMat" [Qast.Loc; e; l]
      | "match"; e = SELF; "with"; p1 = ipatt; "->"; e1 = SELF ->
          Qast.Node "ExMat"
            [Qast.Loc; e; Qast.List [Qast.Tuple [p1; Qast.Option None; e1]]]
      | "try"; e = SELF; "with"; "["; l = SLIST0 match_case SEP "|"; "]" ->
          Qast.Node "ExTry" [Qast.Loc; e; l]
      | "try"; e = SELF; "with"; p1 = ipatt; "->"; e1 = SELF ->
          Qast.Node "ExTry"
            [Qast.Loc; e; Qast.List [Qast.Tuple [p1; Qast.Option None; e1]]]
      | "if"; e1 = SELF; "then"; e2 = SELF; "else"; e3 = SELF ->
          Qast.Node "ExIfe" [Qast.Loc; e1; e2; e3]
      | "do"; "{"; seq = sequence; "}" -> mksequence Qast.Loc seq
      | "for"; i = a_LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; "{"; seq = sequence; "}" ->
          Qast.Node "ExFor" [Qast.Loc; i; e1; e2; df; seq]
      | "while"; e = SELF; "do"; "{"; seq = sequence; "}" ->
          Qast.Node "ExWhi" [Qast.Loc; e; seq] ]
    | "where"
      [ e = SELF; "where"; rf = SOPT "rec"; lb = let_binding ->
          Qast.Node "ExLet" [Qast.Loc; o2b rf; Qast.List [lb]; e] ]
    | ":=" NONA
      [ e1 = SELF; ":="; e2 = SELF; dummy ->
          Qast.Node "ExAss" [Qast.Loc; e1; e2] ]
    | "||" RIGHTA
      [ e1 = SELF; "||"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "||"]; e1];
             e2] ]
    | "&&" RIGHTA
      [ e1 = SELF; "&&"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "&&"]; e1];
             e2] ]
    | "<" LEFTA
      [ e1 = SELF; "<"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "<"]; e1];
             e2]
      | e1 = SELF; ">"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str ">"]; e1];
             e2]
      | e1 = SELF; "<="; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "<="]; e1];
             e2]
      | e1 = SELF; ">="; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str ">="]; e1];
             e2]
      | e1 = SELF; "="; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "="]; e1];
             e2]
      | e1 = SELF; "<>"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "<>"]; e1];
             e2]
      | e1 = SELF; "=="; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "=="]; e1];
             e2]
      | e1 = SELF; "!="; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "!="]; e1];
             e2] ]
    | "^" RIGHTA
      [ e1 = SELF; "^"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "^"]; e1];
             e2]
      | e1 = SELF; "@"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "@"]; e1];
             e2] ]
    | "+" LEFTA
      [ e1 = SELF; "+"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "+"]; e1];
             e2]
      | e1 = SELF; "-"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "-"]; e1];
             e2]
      | e1 = SELF; "+."; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "+."]; e1];
             e2]
      | e1 = SELF; "-."; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "-."]; e1];
             e2] ]
    | "*" LEFTA
      [ e1 = SELF; "*"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "*"]; e1];
             e2]
      | e1 = SELF; "/"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "/"]; e1];
             e2]
      | e1 = SELF; "*."; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "*."]; e1];
             e2]
      | e1 = SELF; "/."; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "/."]; e1];
             e2]
      | e1 = SELF; "land"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "land"]; e1];
             e2]
      | e1 = SELF; "lor"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "lor"]; e1];
             e2]
      | e1 = SELF; "lxor"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "lxor"]; e1];
             e2]
      | e1 = SELF; "mod"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "mod"]; e1];
             e2] ]
    | "**" RIGHTA
      [ e1 = SELF; "**"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "**"]; e1];
             e2]
      | e1 = SELF; "asr"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "asr"]; e1];
             e2]
      | e1 = SELF; "lsl"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "lsl"]; e1];
             e2]
      | e1 = SELF; "lsr"; e2 = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc;
             Qast.Node "ExApp"
               [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "lsr"]; e1];
             e2] ]
    | "unary minus" NONA
      [ "-"; e = SELF -> mkumin Qast.Loc (Qast.Str "-") e
      | "-."; e = SELF -> mkumin Qast.Loc (Qast.Str "-.") e ]
    | "apply" LEFTA
      [ e1 = SELF; e2 = SELF -> Qast.Node "ExApp" [Qast.Loc; e1; e2]
      | "assert"; e = SELF -> mkassert Qast.Loc e
      | "lazy"; e = SELF -> Qast.Node "ExLaz" [Qast.Loc; e] ]
    | "." LEFTA
      [ e1 = SELF; "."; "("; e2 = SELF; ")" ->
          Qast.Node "ExAre" [Qast.Loc; e1; e2]
      | e1 = SELF; "."; "["; e2 = SELF; "]" ->
          Qast.Node "ExSte" [Qast.Loc; e1; e2]
      | e1 = SELF; "."; e2 = SELF -> Qast.Node "ExAcc" [Qast.Loc; e1; e2] ]
    | "~-" NONA
      [ "~-"; e = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "~-"]; e]
      | "~-."; e = SELF ->
          Qast.Node "ExApp"
            [Qast.Loc; Qast.Node "ExLid" [Qast.Loc; Qast.Str "~-."]; e] ]
    | "simple"
      [ s = a_INT -> Qast.Node "ExInt" [Qast.Loc; s]
      | s = a_INT32 -> Qast.Node  "ExInt32" [Qast.Loc; s]
      | s = a_INT64 -> Qast.Node  "ExInt64" [Qast.Loc; s]
      | s = a_NATIVEINT -> Qast.Node  "ExNativeInt" [Qast.Loc; s]
      | s = a_FLOAT -> Qast.Node "ExFlo" [Qast.Loc; s]
      | s = a_STRING -> Qast.Node "ExStr" [Qast.Loc; s]
      | s = a_CHAR -> Qast.Node "ExChr" [Qast.Loc; s]
      | i = expr_ident -> i
      | "["; "]" -> Qast.Node "ExUid" [Qast.Loc; Qast.Str "[]"]
      | "["; el = SLIST1 expr SEP ";"; last = cons_expr_opt; "]" ->
          mklistexp Qast.Loc last el
      | "[|"; el = SLIST0 expr SEP ";"; "|]" ->
          Qast.Node "ExArr" [Qast.Loc; el]
      | "{"; lel = SLIST1 label_expr SEP ";"; "}" ->
          Qast.Node "ExRec" [Qast.Loc; lel; Qast.Option None]
      | "{"; "("; e = SELF; ")"; "with"; lel = SLIST1 label_expr SEP ";";
        "}" ->
          Qast.Node "ExRec" [Qast.Loc; lel; Qast.Option (Some e)]
      | "("; ")" -> Qast.Node "ExUid" [Qast.Loc; Qast.Str "()"]
      | "("; e = SELF; ":"; t = ctyp; ")" ->
          Qast.Node "ExTyc" [Qast.Loc; e; t]
      | "("; e = SELF; ","; el = SLIST1 expr SEP ","; ")" ->
          Qast.Node "ExTup" [Qast.Loc; Qast.Cons e el]
      | "("; e = SELF; ")" -> e ] ]
  ;
  cons_expr_opt:
    [ [ "::"; e = expr -> Qast.Option (Some e)
      | -> Qast.Option None ] ]
  ;
  dummy:
    [ [ -> () ] ]
  ;
  sequence:
    [ [ "let"; rf = SOPT "rec"; l = SLIST1 let_binding SEP "and";
        [ "in" | ";" ]; el = SELF ->
          Qast.List
            [Qast.Node "ExLet" [Qast.Loc; o2b rf; l; mksequence Qast.Loc el]]
      | e = expr; ";"; el = SELF -> Qast.Cons e el
      | e = expr; ";" -> Qast.List [e]
      | e = expr -> Qast.List [e] ] ]
  ;
  let_binding:
    [ [ p = ipatt; e = fun_binding -> Qast.Tuple [p; e] ] ]
  ;
  fun_binding:
    [ RIGHTA
      [ p = ipatt; e = SELF ->
          Qast.Node "ExFun"
            [Qast.Loc; Qast.List [Qast.Tuple [p; Qast.Option None; e]]]
      | "="; e = expr -> e
      | ":"; t = ctyp; "="; e = expr -> Qast.Node "ExTyc" [Qast.Loc; e; t] ] ]
  ;
  match_case:
    [ [ p = patt; aso = as_patt_opt; w = when_expr_opt; "->"; e = expr ->
          mkmatchcase Qast.Loc p aso w e ] ]
  ;
  as_patt_opt:
    [ [ "as"; p = patt -> Qast.Option (Some p)
      | -> Qast.Option None ] ]
  ;
  when_expr_opt:
    [ [ "when"; e = expr -> Qast.Option (Some e)
      | -> Qast.Option None ] ]
  ;
  label_expr:
    [ [ i = patt_label_ident; e = fun_binding -> Qast.Tuple [i; e] ] ]
  ;
  expr_ident:
    [ RIGHTA
      [ i = a_LIDENT -> Qast.Node "ExLid" [Qast.Loc; i]
      | i = a_UIDENT -> Qast.Node "ExUid" [Qast.Loc; i]
      | i = a_UIDENT; "."; j = SELF -> mkexprident Qast.Loc i j ] ]
  ;
  fun_def:
    [ RIGHTA
      [ p = ipatt; e = SELF ->
          Qast.Node "ExFun"
            [Qast.Loc; Qast.List [Qast.Tuple [p; Qast.Option None; e]]]
      | "->"; e = expr -> e ] ]
  ;
  patt:
    [ LEFTA
      [ p1 = SELF; "|"; p2 = SELF -> Qast.Node "PaOrp" [Qast.Loc; p1; p2] ]
    | NONA
      [ p1 = SELF; ".."; p2 = SELF -> Qast.Node "PaRng" [Qast.Loc; p1; p2] ]
    | LEFTA
      [ p1 = SELF; p2 = SELF -> Qast.Node "PaApp" [Qast.Loc; p1; p2] ]
    | LEFTA
      [ p1 = SELF; "."; p2 = SELF -> Qast.Node "PaAcc" [Qast.Loc; p1; p2] ]
    | "simple"
      [ s = a_LIDENT -> Qast.Node "PaLid" [Qast.Loc; s]
      | s = a_UIDENT -> Qast.Node "PaUid" [Qast.Loc; s]
      | s = a_INT -> Qast.Node "PaInt" [Qast.Loc; s]
      | s = a_INT32 -> Qast.Node  "PaInt32" [Qast.Loc; s]
      | s = a_INT64 -> Qast.Node  "PaInt64" [Qast.Loc; s]
      | s = a_NATIVEINT -> Qast.Node  "PaNativeInt" [Qast.Loc; s]
      | s = a_FLOAT -> Qast.Node "PaFlo" [Qast.Loc; s]
      | s = a_STRING -> Qast.Node "PaStr" [Qast.Loc; s]
      | s = a_CHAR -> Qast.Node "PaChr" [Qast.Loc; s]
      | "-"; s = a_INT -> mkuminpat Qast.Loc (Qast.Str "-") (Qast.Bool True) s
      | "-"; s = a_INT32 -> mkuminpat Qast.Loc (Qast.Str "-") (Qast.Bool True) s
      | "-"; s = a_INT64 -> mkuminpat Qast.Loc (Qast.Str "-") (Qast.Bool True) s
      | "-"; s = a_NATIVEINT -> mkuminpat Qast.Loc (Qast.Str "-") (Qast.Bool True) s
      | "-"; s = a_FLOAT ->
          mkuminpat Qast.Loc (Qast.Str "-") (Qast.Bool False) s
      | "["; "]" -> Qast.Node "PaUid" [Qast.Loc; Qast.Str "[]"]
      | "["; pl = SLIST1 patt SEP ";"; last = cons_patt_opt; "]" ->
          mklistpat Qast.Loc last pl
      | "[|"; pl = SLIST0 patt SEP ";"; "|]" ->
          Qast.Node "PaArr" [Qast.Loc; pl]
      | "{"; lpl = SLIST1 label_patt SEP ";"; "}" ->
          Qast.Node "PaRec" [Qast.Loc; lpl]
      | "("; ")" -> Qast.Node "PaUid" [Qast.Loc; Qast.Str "()"]
      | "("; p = SELF; ")" -> p
      | "("; p = SELF; ":"; t = ctyp; ")" ->
          Qast.Node "PaTyc" [Qast.Loc; p; t]
      | "("; p = SELF; "as"; p2 = SELF; ")" ->
          Qast.Node "PaAli" [Qast.Loc; p; p2]
      | "("; p = SELF; ","; pl = SLIST1 patt SEP ","; ")" ->
          Qast.Node "PaTup" [Qast.Loc; Qast.Cons p pl]
      | "_" -> Qast.Node "PaAny" [Qast.Loc] ] ]
  ;
  cons_patt_opt:
    [ [ "::"; p = patt -> Qast.Option (Some p)
      | -> Qast.Option None ] ]
  ;
  label_patt:
    [ [ i = patt_label_ident; "="; p = patt -> Qast.Tuple [i; p] ] ]
  ;
  patt_label_ident:
    [ LEFTA
      [ p1 = SELF; "."; p2 = SELF -> Qast.Node "PaAcc" [Qast.Loc; p1; p2] ]
    | "simple" RIGHTA
      [ i = a_UIDENT -> Qast.Node "PaUid" [Qast.Loc; i]
      | i = a_LIDENT -> Qast.Node "PaLid" [Qast.Loc; i] ] ]
  ;
  ipatt:
    [ [ "{"; lpl = SLIST1 label_ipatt SEP ";"; "}" ->
          Qast.Node "PaRec" [Qast.Loc; lpl]
      | "("; ")" -> Qast.Node "PaUid" [Qast.Loc; Qast.Str "()"]
      | "("; p = SELF; ")" -> p
      | "("; p = SELF; ":"; t = ctyp; ")" ->
          Qast.Node "PaTyc" [Qast.Loc; p; t]
      | "("; p = SELF; "as"; p2 = SELF; ")" ->
          Qast.Node "PaAli" [Qast.Loc; p; p2]
      | "("; p = SELF; ","; pl = SLIST1 ipatt SEP ","; ")" ->
          Qast.Node "PaTup" [Qast.Loc; Qast.Cons p pl]
      | s = a_LIDENT -> Qast.Node "PaLid" [Qast.Loc; s]
      | "_" -> Qast.Node "PaAny" [Qast.Loc] ] ]
  ;
  label_ipatt:
    [ [ i = patt_label_ident; "="; p = ipatt -> Qast.Tuple [i; p] ] ]
  ;
  type_declaration:
    [ [ n = type_patt; tpl = SLIST0 type_parameter; "="; tk = ctyp;
        cl = SLIST0 constrain ->
          Qast.Tuple [n; tpl; tk; cl] ] ]
  ;
  type_patt:
    [ [ n = a_LIDENT -> Qast.Tuple [Qast.Loc; n] ] ]
  ;
  constrain:
    [ [ "constraint"; t1 = ctyp; "="; t2 = ctyp -> Qast.Tuple [t1; t2] ] ]
  ;
  type_parameter:
    [ [ "'"; i = ident ->
          Qast.Tuple [i; Qast.Tuple [Qast.Bool False; Qast.Bool False]]
      | "+"; "'"; i = ident ->
          Qast.Tuple [i; Qast.Tuple [Qast.Bool True; Qast.Bool False]]
      | "-"; "'"; i = ident ->
          Qast.Tuple [i; Qast.Tuple [Qast.Bool False; Qast.Bool True]] ] ]
  ;
  ctyp:
    [ LEFTA
      [ t1 = SELF; "=="; t2 = SELF -> Qast.Node "TyMan" [Qast.Loc; t1; t2] ]
    | NONA
      [ "private"; t = ctyp LEVEL "alias" -> Qast.Node "TyPrv" [Qast.Loc; t] ]
    | "alias" LEFTA
      [ t1 = SELF; "as"; t2 = SELF -> Qast.Node "TyAli" [Qast.Loc; t1; t2] ]
    | LEFTA
      [ "!"; pl = SLIST1 typevar; "."; t = SELF ->
          Qast.Node "TyPol" [Qast.Loc; pl; t] ]
    | "arrow" RIGHTA
      [ t1 = SELF; "->"; t2 = SELF -> Qast.Node "TyArr" [Qast.Loc; t1; t2] ]
    | "label" NONA
      [ i = a_TILDEIDENT; ":"; t = SELF -> Qast.Node "TyLab" [Qast.Loc; i; t]
      | i = a_LABEL; t = SELF -> Qast.Node "TyLab" [Qast.Loc; i; t]
      | i = a_QUESTIONIDENT; ":"; t = SELF ->
          Qast.Node "TyOlb" [Qast.Loc; i; t]
      | i = a_OPTLABEL; t = SELF ->
          Qast.Node "TyOlb" [Qast.Loc; i; t] ]
    | LEFTA
      [ t1 = SELF; t2 = SELF -> Qast.Node "TyApp" [Qast.Loc; t1; t2] ]
    | LEFTA
      [ t1 = SELF; "."; t2 = SELF -> Qast.Node "TyAcc" [Qast.Loc; t1; t2] ]
    | "simple"
      [ "'"; i = ident -> Qast.Node "TyQuo" [Qast.Loc; i]
      | "_" -> Qast.Node "TyAny" [Qast.Loc]
      | i = a_LIDENT -> Qast.Node "TyLid" [Qast.Loc; i]
      | i = a_UIDENT -> Qast.Node "TyUid" [Qast.Loc; i]
      | "("; t = SELF; "*"; tl = SLIST1 ctyp SEP "*"; ")" ->
          Qast.Node "TyTup" [Qast.Loc; Qast.Cons t tl]
      | "("; t = SELF; ")" -> t
      | "["; cdl = SLIST0 constructor_declaration SEP "|"; "]" ->
          Qast.Node "TySum" [Qast.Loc; cdl]
      | "{"; ldl = SLIST1 label_declaration SEP ";"; "}" ->
          Qast.Node "TyRec" [Qast.Loc; ldl] ] ]
  ;
  constructor_declaration:
    [ [ ci = a_UIDENT; "of"; cal = SLIST1 ctyp SEP "and" ->
          Qast.Tuple [Qast.Loc; ci; cal]
      | ci = a_UIDENT -> Qast.Tuple [Qast.Loc; ci; Qast.List []] ] ]
  ;
  label_declaration:
    [ [ i = a_LIDENT; ":"; mf = SOPT "mutable"; t = ctyp ->
          Qast.Tuple [Qast.Loc; i; o2b mf; t] ] ]
  ;
  ident:
    [ [ i = a_LIDENT -> i
      | i = a_UIDENT -> i ] ]
  ;
  mod_ident:
    [ RIGHTA
      [ i = a_UIDENT -> Qast.List [i]
      | i = a_LIDENT -> Qast.List [i]
      | i = a_UIDENT; "."; j = SELF -> Qast.Cons i j ] ]
  ;
  (* Objects and Classes *)
  str_item:
    [ [ "class"; cd = SLIST1 class_declaration SEP "and" ->
          Qast.Node "StCls" [Qast.Loc; cd]
      | "class"; "type"; ctd = SLIST1 class_type_declaration SEP "and" ->
          Qast.Node "StClt" [Qast.Loc; ctd] ] ]
  ;
  sig_item:
    [ [ "class"; cd = SLIST1 class_description SEP "and" ->
          Qast.Node "SgCls" [Qast.Loc; cd]
      | "class"; "type"; ctd = SLIST1 class_type_declaration SEP "and" ->
          Qast.Node "SgClt" [Qast.Loc; ctd] ] ]
  ;
  class_declaration:
    [ [ vf = SOPT "virtual"; i = a_LIDENT; ctp = class_type_parameters;
        cfb = class_fun_binding ->
          Qast.Record
            [("ciLoc", Qast.Loc); ("ciVir", o2b vf); ("ciPrm", ctp);
             ("ciNam", i); ("ciExp", cfb)] ] ]
  ;
  class_fun_binding:
    [ [ "="; ce = class_expr -> ce
      | ":"; ct = class_type; "="; ce = class_expr ->
          Qast.Node "CeTyc" [Qast.Loc; ce; ct]
      | p = ipatt; cfb = SELF -> Qast.Node "CeFun" [Qast.Loc; p; cfb] ] ]
  ;
  class_type_parameters:
    [ [ -> Qast.Tuple [Qast.Loc; Qast.List []]
      | "["; tpl = SLIST1 type_parameter SEP ","; "]" ->
          Qast.Tuple [Qast.Loc; tpl] ] ]
  ;
  class_fun_def:
    [ [ p = ipatt; ce = SELF -> Qast.Node "CeFun" [Qast.Loc; p; ce]
      | "->"; ce = class_expr -> ce ] ]
  ;
  class_expr:
    [ "top"
      [ "fun"; p = ipatt; ce = class_fun_def ->
          Qast.Node "CeFun" [Qast.Loc; p; ce]
      | "let"; rf = SOPT "rec"; lb = SLIST1 let_binding SEP "and"; "in";
        ce = SELF ->
          Qast.Node "CeLet" [Qast.Loc; o2b rf; lb; ce] ]
    | "apply" NONA
      [ ce = SELF; e = expr LEVEL "label" ->
          Qast.Node "CeApp" [Qast.Loc; ce; e] ]
    | "simple"
      [ ci = class_longident; "["; ctcl = SLIST0 ctyp SEP ","; "]" ->
          Qast.Node "CeCon" [Qast.Loc; ci; ctcl]
      | ci = class_longident -> Qast.Node "CeCon" [Qast.Loc; ci; Qast.List []]
      | "object"; cspo = SOPT class_self_patt; cf = class_structure; "end" ->
          Qast.Node "CeStr" [Qast.Loc; cspo; cf]
      | "("; ce = SELF; ":"; ct = class_type; ")" ->
          Qast.Node "CeTyc" [Qast.Loc; ce; ct]
      | "("; ce = SELF; ")" -> ce ] ]
  ;
  class_structure:
    [ [ cf = SLIST0 [ cf = class_str_item; ";" -> cf ] -> cf ] ]
  ;
  class_self_patt:
    [ [ "("; p = patt; ")" -> p
      | "("; p = patt; ":"; t = ctyp; ")" ->
          Qast.Node "PaTyc" [Qast.Loc; p; t] ] ]
  ;
  class_str_item:
    [ [ "declare"; st = SLIST0 [ s = class_str_item; ";" -> s ]; "end" ->
          Qast.Node "CrDcl" [Qast.Loc; st]
      | "inherit"; ce = class_expr; pb = SOPT as_lident ->
          Qast.Node "CrInh" [Qast.Loc; ce; pb]
      | "value"; mf = SOPT "mutable"; lab = label; e = cvalue_binding ->
          Qast.Node "CrVal" [Qast.Loc; lab; o2b mf; e]
      | "method"; "virtual"; pf = SOPT "private"; l = label; ":"; t = ctyp ->
          Qast.Node "CrVir" [Qast.Loc; l; o2b pf; t]
      | "method"; pf = SOPT "private"; l = label; topt = SOPT polyt;
        e = fun_binding ->
          Qast.Node "CrMth" [Qast.Loc; l; o2b pf; e; topt]
      | "type"; t1 = ctyp; "="; t2 = ctyp ->
          Qast.Node "CrCtr" [Qast.Loc; t1; t2]
      | "initializer"; se = expr -> Qast.Node "CrIni" [Qast.Loc; se] ] ]
  ;
  as_lident:
    [ [ "as"; i = a_LIDENT -> i ] ]
  ;
  polyt:
    [ [ ":"; t = ctyp -> t ] ]
  ;
  cvalue_binding:
    [ [ "="; e = expr -> e
      | ":"; t = ctyp; "="; e = expr -> Qast.Node "ExTyc" [Qast.Loc; e; t]
      | ":"; t = ctyp; ":>"; t2 = ctyp; "="; e = expr ->
          Qast.Node "ExCoe" [Qast.Loc; e; Qast.Option (Some t); t2]
      | ":>"; t = ctyp; "="; e = expr ->
          Qast.Node "ExCoe" [Qast.Loc; e; Qast.Option None; t] ] ]
  ;
  label:
    [ [ i = a_LIDENT -> i ] ]
  ;
  class_type:
    [ [ "["; t = ctyp; "]"; "->"; ct = SELF ->
          Qast.Node "CtFun" [Qast.Loc; t; ct]
      | id = clty_longident; "["; tl = SLIST1 ctyp SEP ","; "]" ->
          Qast.Node "CtCon" [Qast.Loc; id; tl]
      | id = clty_longident -> Qast.Node "CtCon" [Qast.Loc; id; Qast.List []]
      | "object"; cst = SOPT class_self_type;
        csf = SLIST0 [ csf = class_sig_item; ";" -> csf ]; "end" ->
          Qast.Node "CtSig" [Qast.Loc; cst; csf] ] ]
  ;
  class_self_type:
    [ [ "("; t = ctyp; ")" -> t ] ]
  ;
  class_sig_item:
    [ [ "declare"; st = SLIST0 [ s = class_sig_item; ";" -> s ]; "end" ->
          Qast.Node "CgDcl" [Qast.Loc; st]
      | "inherit"; cs = class_type -> Qast.Node "CgInh" [Qast.Loc; cs]
      | "value"; mf = SOPT "mutable"; l = label; ":"; t = ctyp ->
          Qast.Node "CgVal" [Qast.Loc; l; o2b mf; t]
      | "method"; "virtual"; pf = SOPT "private"; l = label; ":"; t = ctyp ->
          Qast.Node "CgVir" [Qast.Loc; l; o2b pf; t]
      | "method"; pf = SOPT "private"; l = label; ":"; t = ctyp ->
          Qast.Node "CgMth" [Qast.Loc; l; o2b pf; t]
      | "type"; t1 = ctyp; "="; t2 = ctyp ->
          Qast.Node "CgCtr" [Qast.Loc; t1; t2] ] ]
  ;
  class_description:
    [ [ vf = SOPT "virtual"; n = a_LIDENT; ctp = class_type_parameters; ":";
        ct = class_type ->
          Qast.Record
            [("ciLoc", Qast.Loc); ("ciVir", o2b vf); ("ciPrm", ctp);
             ("ciNam", n); ("ciExp", ct)] ] ]
  ;
  class_type_declaration:
    [ [ vf = SOPT "virtual"; n = a_LIDENT; ctp = class_type_parameters; "=";
        cs = class_type ->
          Qast.Record
            [("ciLoc", Qast.Loc); ("ciVir", o2b vf); ("ciPrm", ctp);
             ("ciNam", n); ("ciExp", cs)] ] ]
  ;
  expr: LEVEL "apply"
    [ LEFTA
      [ "new"; i = class_longident -> Qast.Node "ExNew" [Qast.Loc; i] ] ]
  ;
  expr: LEVEL "."
    [ [ e = SELF; "#"; lab = label -> Qast.Node "ExSnd" [Qast.Loc; e; lab] ] ]
  ;
  expr: LEVEL "simple"
    [ [ "("; e = SELF; ":"; t = ctyp; ":>"; t2 = ctyp; ")" ->
          Qast.Node "ExCoe" [Qast.Loc; e; Qast.Option (Some t); t2]
      | "("; e = SELF; ":>"; t = ctyp; ")" ->
          Qast.Node "ExCoe" [Qast.Loc; e; Qast.Option None; t]
      | "{<"; fel = SLIST0 field_expr SEP ";"; ">}" ->
          Qast.Node "ExOvr" [Qast.Loc; fel] ] ]
  ;
  field_expr:
    [ [ l = label; "="; e = expr -> Qast.Tuple [l; e] ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ "#"; id = class_longident -> Qast.Node "TyCls" [Qast.Loc; id]
      | "<"; ml = SLIST0 field SEP ";"; v = SOPT ".."; ">" ->
          Qast.Node "TyObj" [Qast.Loc; ml; o2b v] ] ]
  ;
  field:
    [ [ lab = a_LIDENT; ":"; t = ctyp -> Qast.Tuple [lab; t] ] ]
  ;
  typevar:
    [ [ "'"; i = ident -> i ] ]
  ;
  clty_longident:
    [ [ m = a_UIDENT; "."; l = SELF -> Qast.Cons m l
      | i = a_LIDENT -> Qast.List [i] ] ]
  ;
  class_longident:
    [ [ m = a_UIDENT; "."; l = SELF -> Qast.Cons m l
      | i = a_LIDENT -> Qast.List [i] ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ "["; "="; rfl = row_field_list; "]" ->
          Qast.Node "TyVrn" [Qast.Loc; rfl; Qast.Option None]
      | "["; ">"; rfl = row_field_list; "]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl; Qast.Option (Some (Qast.Option None))]
      | "["; "<"; rfl = row_field_list; "]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl;
             Qast.Option (Some (Qast.Option (Some (Qast.List []))))]
      | "["; "<"; rfl = row_field_list; ">"; ntl = SLIST1 name_tag; "]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl; Qast.Option (Some (Qast.Option (Some ntl)))]
      | "[<"; rfl = row_field_list; "]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl;
             Qast.Option (Some (Qast.Option (Some (Qast.List []))))]
      | "[<"; rfl = row_field_list; ">"; ntl = SLIST1 name_tag; "]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl; Qast.Option (Some (Qast.Option (Some ntl)))] ] ]
  ;
  row_field_list:
    [ [ rfl = SLIST0 row_field SEP "|" -> rfl ] ]
  ;
  row_field:
    [ [ "`"; i = ident -> Qast.Node "RfTag" [i; Qast.Bool True; Qast.List []]
      | "`"; i = ident; "of"; ao = SOPT "&"; l = SLIST1 ctyp SEP "&" ->
          Qast.Node "RfTag" [i; o2b ao; l]
      | t = ctyp -> Qast.Node "RfInh" [t] ] ]
  ;
  name_tag:
    [ [ "`"; i = ident -> i ] ]
  ;
  patt: LEVEL "simple"
    [ [ "`"; s = ident -> Qast.Node "PaVrn" [Qast.Loc; s]
      | "#"; sl = mod_ident -> Qast.Node "PaTyp" [Qast.Loc; sl]
      | i = a_TILDEIDENT; ":"; p = SELF ->
          Qast.Node "PaLab" [Qast.Loc; i; Qast.Option (Some p)]
      | i = a_LABEL; p = SELF ->
          Qast.Node "PaLab" [Qast.Loc; i; Qast.Option (Some p)]
      | i = a_TILDEIDENT -> Qast.Node "PaLab" [Qast.Loc; i; Qast.Option None]
      | i = a_QUESTIONIDENT; ":"; "("; p = patt_tcon; eo = SOPT eq_expr;
        ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; i; Qast.Option (Some (Qast.Tuple [p; eo]))]
      | i = a_OPTLABEL; "("; p = patt_tcon; eo = SOPT eq_expr; ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; i; Qast.Option (Some (Qast.Tuple [p; eo]))]
      | i = a_QUESTIONIDENT ->
          Qast.Node "PaOlb" [Qast.Loc; i; Qast.Option None]
      | "?"; "("; p = patt_tcon; eo = SOPT eq_expr; ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; Qast.Str "";
             Qast.Option (Some (Qast.Tuple [p; eo]))] ] ]
  ;
  patt_tcon:
    [ [ p = patt; ":"; t = ctyp -> Qast.Node "PaTyc" [Qast.Loc; p; t]
      | p = patt -> p ] ]
  ;
  ipatt:
    [ [ i = a_TILDEIDENT; ":"; p = SELF ->
          Qast.Node "PaLab" [Qast.Loc; i; Qast.Option (Some p)]
      | i = a_LABEL; p = SELF ->
          Qast.Node "PaLab" [Qast.Loc; i; Qast.Option (Some p)]
      | i = a_TILDEIDENT -> Qast.Node "PaLab" [Qast.Loc; i; Qast.Option None]
      | i = a_QUESTIONIDENT; ":"; "("; p = ipatt_tcon; eo = SOPT eq_expr;
        ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; i; Qast.Option (Some (Qast.Tuple [p; eo]))]
      | i = a_OPTLABEL; "("; p = ipatt_tcon; eo = SOPT eq_expr; ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; i; Qast.Option (Some (Qast.Tuple [p; eo]))]
      | i = a_QUESTIONIDENT ->
          Qast.Node "PaOlb" [Qast.Loc; i; Qast.Option None]
      | "?"; "("; p = ipatt_tcon; eo = SOPT eq_expr; ")" ->
          Qast.Node "PaOlb"
            [Qast.Loc; Qast.Str "";
             Qast.Option (Some (Qast.Tuple [p; eo]))] ] ]
  ;
  ipatt_tcon:
    [ [ p = ipatt; ":"; t = ctyp -> Qast.Node "PaTyc" [Qast.Loc; p; t]
      | p = ipatt -> p ] ]
  ;
  eq_expr:
    [ [ "="; e = expr -> e ] ]
  ;
  expr: AFTER "apply"
    [ "label" NONA
      [ i = a_TILDEIDENT; ":"; e = SELF ->
          Qast.Node "ExLab" [Qast.Loc; i; Qast.Option (Some e)]
      | i = a_LABEL; e = SELF ->
          Qast.Node "ExLab" [Qast.Loc; i; Qast.Option (Some e)]
      | i = a_TILDEIDENT -> Qast.Node "ExLab" [Qast.Loc; i; Qast.Option None]
      | i = a_QUESTIONIDENT; ":"; e = SELF ->
          Qast.Node "ExOlb" [Qast.Loc; i; Qast.Option (Some e)]
      | i = a_OPTLABEL; e = SELF ->
          Qast.Node "ExOlb" [Qast.Loc; i; Qast.Option (Some e)]
      | i = a_QUESTIONIDENT ->
          Qast.Node "ExOlb" [Qast.Loc; i; Qast.Option None] ] ]
  ;
  expr: LEVEL "simple"
    [ [ "`"; s = ident -> Qast.Node "ExVrn" [Qast.Loc; s] ] ]
  ;
  direction_flag:
    [ [ "to" -> Qast.Bool True
      | "downto" -> Qast.Bool False ] ]
  ;
  (* Compatibility old syntax of variant types definitions *)
  ctyp: LEVEL "simple"
    [ [ "[|"; warning_variant; rfl = row_field_list; "|]" ->
          Qast.Node "TyVrn" [Qast.Loc; rfl; Qast.Option None]
      | "[|"; warning_variant; ">"; rfl = row_field_list; "|]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl; Qast.Option (Some (Qast.Option None))]
      | "[|"; warning_variant; "<"; rfl = row_field_list; "|]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl;
             Qast.Option (Some (Qast.Option (Some (Qast.List []))))]
      | "[|"; warning_variant; "<"; rfl = row_field_list; ">";
        ntl = SLIST1 name_tag; "|]" ->
          Qast.Node "TyVrn"
            [Qast.Loc; rfl; Qast.Option (Some (Qast.Option (Some ntl)))] ] ]
  ;
  warning_variant:
    [ [ -> warn_variant Qast.Loc ] ]
  ;
  (* Compatibility old syntax of sequences *)
  expr: LEVEL "top"
    [ [ "do"; seq = SLIST0 [ e = expr; ";" -> e ]; "return"; warning_sequence;
        e = SELF ->
          Qast.Node "ExSeq" [Qast.Loc; append_elem seq e]
      | "for"; i = a_LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; seq = SLIST0 [ e = expr; ";" -> e ]; warning_sequence; "done" ->
          Qast.Node "ExFor" [Qast.Loc; i; e1; e2; df; seq]
      | "while"; e = SELF; "do"; seq = SLIST0 [ e = expr; ";" -> e ];
        warning_sequence; "done" ->
          Qast.Node "ExWhi" [Qast.Loc; e; seq] ] ]
  ;
  warning_sequence:
    [ [ -> warn_sequence Qast.Loc ] ]
  ;
  (* Antiquotations for local entries *)
  sequence:
    [ [ a = ANTIQUOT "list" -> antiquot "list" _loc a ] ]
  ;
  expr_ident:
    [ [ a = ANTIQUOT -> antiquot "" _loc a ] ]
  ;
  patt_label_ident: LEVEL "simple"
    [ [ a = ANTIQUOT -> antiquot "" _loc a ] ]
  ;
  when_expr_opt:
    [ [ a = ANTIQUOT "when" -> antiquot "when" _loc a ] ]
  ;
  mod_ident:
    [ [ a = ANTIQUOT -> antiquot "" _loc a ] ]
  ;
  clty_longident:
    [ [ a = a_list -> a ] ]
  ;
  class_longident:
    [ [ a = a_list -> a ] ]
  ;
  direction_flag:
    [ [ a = ANTIQUOT "to" -> antiquot "to" _loc a ] ]
  ;
  (* deprecated since version 3.05; code for compatibility *)
  class_expr: LEVEL "simple"
    [ [ "object"; x = ANTIQUOT; cf = class_structure; "end" ->
          let _ = warn_antiq _loc "3.05" in
          Qast.Node "CeStr" [Qast.Loc; antiquot "" _loc x; cf]
      | "object"; x = ANTIQUOT; ";";
        csl = SLIST0 [ cf = class_str_item; ";" -> cf ] ; "end" ->
          let _ = warn_antiq _loc "3.05" in
          Qast.Node "CeStr"
            [Qast.Loc; Qast.Option None;
             Qast.Cons (antiquot "" _loc x) csl] ] ]
  ;
  class_type:
    [ [ "object"; x = ANTIQUOT;
        csf = SLIST0 [ csf = class_sig_item; ";" -> csf ]; "end" ->
          let _ = warn_antiq _loc "3.05" in
          Qast.Node "CtSig" [Qast.Loc; antiquot "" _loc x; csf]
      | "object"; x = ANTIQUOT; ";";
        csf = SLIST0 [ csf = class_sig_item; ";" -> csf ]; "end" ->
          let _ = warn_antiq _loc "3.05" in
          Qast.Node "CtSig"
            [Qast.Loc; Qast.Option None;
             Qast.Cons (antiquot "" _loc x) csf] ] ]
  ;
  (* deprecated since version 3.06+18; code for compatibility *)
  expr: LEVEL "top"
    [ [ "let"; r = ANTIQUOT "rec"; l = SLIST1 let_binding SEP "and"; "in";
        x = SELF ->
          let _ = warn_antiq _loc "3.06+18" in
          Qast.Node "ExLet" [Qast.Loc; antiquot "rec" _loc r; l; x] ] ]
  ;
  str_item: LEVEL "top"
    [ [ "value"; r = ANTIQUOT "rec"; l = SLIST1 let_binding SEP "and" ->
          let _ = warn_antiq _loc "3.06+18" in
          Qast.Node "StVal" [Qast.Loc; antiquot "rec" _loc r; l] ] ]
  ;
  class_expr: LEVEL "top"
    [ [ "let"; r = ANTIQUOT "rec"; lb = SLIST1 let_binding SEP "and"; "in";
        ce = SELF ->
          let _ = warn_antiq _loc "3.06+18" in
          Qast.Node "CeLet" [Qast.Loc; antiquot "rec" _loc r; lb; ce] ] ]
  ;
  class_str_item:
    [ [ "inherit"; ce = class_expr; pb = ANTIQUOT "as" ->
          let _ = warn_antiq _loc "3.06+18" in
          Qast.Node "CrInh" [Qast.Loc; ce; antiquot "as" _loc pb]
      | "value"; mf = ANTIQUOT "mut"; lab = label; e = cvalue_binding ->
          let _ = warn_antiq _loc "3.06+18" in
          Qast.Node "CrVal" [Qast.Loc; lab; antiquot "mut" _loc mf; e] ] ]
  ;
  class_sig_item:
    [ [ "value"; mf = ANTIQUOT "mut"; l = label; ":"; t = ctyp ->
          let _ = warn_antiq _loc "3.06+18" in
          Qast.Node "CgVal" [Qast.Loc; l; antiquot "mut" _loc mf; t] ] ]
  ;
END;

EXTEND
  GLOBAL: str_item sig_item;
  str_item:
    [ [ "#"; n = a_LIDENT; dp = dir_param ->
          Qast.Node "StDir" [Qast.Loc; n; dp] ] ]
  ;
  sig_item:
    [ [ "#"; n = a_LIDENT; dp = dir_param ->
          Qast.Node "SgDir" [Qast.Loc; n; dp] ] ]
  ;
  dir_param:
    [ [ a = ANTIQUOT "opt" -> antiquot "opt" _loc a
      | e = expr -> Qast.Option (Some e)
      | -> Qast.Option None ] ]
  ;
END;

(* Antiquotations *)

EXTEND
  module_expr: LEVEL "simple"
    [ [ a = ANTIQUOT "mexp" -> antiquot "mexp" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a ] ]
  ;
  str_item: LEVEL "top"
    [ [ a = ANTIQUOT "stri" -> antiquot "stri" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a ] ]
  ;
  module_type: LEVEL "simple"
    [ [ a = ANTIQUOT "mtyp" -> antiquot "mtyp" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a ] ]
  ;
  sig_item: LEVEL "top"
    [ [ a = ANTIQUOT "sigi" -> antiquot "sigi" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a ] ]
  ;
  expr: LEVEL "simple"
    [ [ a = ANTIQUOT "exp" -> antiquot "exp" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a
      | a = ANTIQUOT "anti" ->
          Qast.Node "ExAnt" [Qast.Loc; antiquot "anti" _loc a]
      | "("; el = a_list; ")" -> Qast.Node "ExTup" [Qast.Loc; el] ] ]
  ;
  patt: LEVEL "simple"
    [ [ a = ANTIQUOT "pat" -> antiquot "pat" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a
      | a = ANTIQUOT "anti" ->
          Qast.Node "PaAnt" [Qast.Loc; antiquot "anti" _loc a]
      | "("; pl = a_list; ")" -> Qast.Node "PaTup" [Qast.Loc; pl] ] ]
  ;
  ipatt:
    [ [ a = ANTIQUOT "pat" -> antiquot "pat" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a
      | a = ANTIQUOT "anti" ->
          Qast.Node "PaAnt" [Qast.Loc; antiquot "anti" _loc a]
      | "("; pl = a_list; ")" -> Qast.Node "PaTup" [Qast.Loc; pl] ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ a = ANTIQUOT "typ" -> antiquot "typ" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a
      | "("; tl = a_list; ")" -> Qast.Node "TyTup" [Qast.Loc; tl] ] ]
  ;
  class_expr: LEVEL "simple"
    [ [ a = ANTIQUOT -> antiquot "" _loc a ] ]
  ;
  class_str_item:
    [ [ a = ANTIQUOT -> antiquot "" _loc a ] ]
  ;
  class_sig_item:
    [ [ a = ANTIQUOT -> antiquot "" _loc a ] ]
  ;
  class_type:
    [ [ a = ANTIQUOT -> antiquot "" _loc a ] ]
  ;
  expr: LEVEL "simple"
    [ [ "{<"; fel = a_list; ">}" -> Qast.Node "ExOvr" [Qast.Loc; fel] ] ]
  ;
  patt: LEVEL "simple"
    [ [ "#"; a = a_list -> Qast.Node "PaTyp" [Qast.Loc; a] ] ]
  ;
  a_list:
    [ [ a = ANTIQUOT "list" -> antiquot "list" _loc a ] ]
  ;
  a_opt:
    [ [ a = ANTIQUOT "opt" -> antiquot "opt" _loc a ] ]
  ;
  a_UIDENT:
    [ [ a = ANTIQUOT "uid" -> antiquot "uid" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a
      | i = UIDENT -> Qast.Str i ] ]
  ;
  a_LIDENT:
    [ [ a = ANTIQUOT "lid" -> antiquot "lid" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a
      | i = LIDENT -> Qast.Str i ] ]
  ;
  a_INT:
    [ [ a = ANTIQUOT "int" -> antiquot "int" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a
      | s = INT -> Qast.Str s ] ]
  ;
  a_INT32:
    [ [ a = ANTIQUOT "int32" -> antiquot "int32" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a
      | s = INT32 -> Qast.Str s ] ]
  ;
  a_INT64:
    [ [ a = ANTIQUOT "int64" -> antiquot "int64" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a
      | s = INT64 -> Qast.Str s ] ]
  ;
  a_NATIVEINT:
    [ [ a = ANTIQUOT "nativeint" -> antiquot "nativeint" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a
      | s = NATIVEINT -> Qast.Str s ] ]
  ;
  a_FLOAT:
    [ [ a = ANTIQUOT "flo" -> antiquot "flo" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a
      | s = FLOAT -> Qast.Str s ] ]
  ;
  a_STRING:
    [ [ a = ANTIQUOT "str" -> antiquot "str" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a
      | s = STRING -> Qast.Str s ] ]
  ;
  a_CHAR:
    [ [ a = ANTIQUOT "chr" -> antiquot "chr" _loc a
      | a = ANTIQUOT -> antiquot "" _loc a
      | s = CHAR -> Qast.Str s ] ]
  ;
  a_TILDEIDENT:
    [ [ "~"; a = ANTIQUOT -> antiquot "" _loc a
      | s = TILDEIDENT -> Qast.Str s ] ]
  ;
  a_LABEL:
    [ [ s = LABEL -> Qast.Str s ] ]
  ;
  a_QUESTIONIDENT:
    [ [ "?"; a = ANTIQUOT -> antiquot "" _loc a
      | s = QUESTIONIDENT -> Qast.Str s ] ]
  ;
  a_OPTLABEL:
    [ [ s = OPTLABEL -> Qast.Str s ] ]
  ;
END;

value apply_entry e =
  let f s =
    let (bolpos,lnum,fname) = q_position in
    let (bolp,ln,_) = (bolpos.val, lnum.val, fname.val) in
    let zero_position() = do { bolpos.val := 0; lnum.val := 1 } in
    let restore_position() = do { bolpos.val := bolp; lnum.val := ln } in
    let _ = zero_position() in
    try
      let result =
        Grammar.Entry.parse e (Stream.of_string s) in
      let _ = restore_position() in
      result
    with exc -> do { restore_position(); raise exc } in
  let expr s = Qast.to_expr (f s) in
  let patt s = Qast.to_patt (f s) in
  Quotation.ExAst (expr, patt)
;


let sig_item_eoi = Grammar.Entry.create gram "signature item" in
do {
  EXTEND
    sig_item_eoi:
      [ [ x = sig_item; EOI -> x ] ]
    ;
  END;
  Quotation.add "sig_item" (apply_entry sig_item_eoi)
};

let str_item_eoi = Grammar.Entry.create gram "structure item" in
do {
  EXTEND
    str_item_eoi:
      [ [ x = str_item; EOI -> x ] ]
    ;
  END;
  Quotation.add "str_item" (apply_entry str_item_eoi)
};

let ctyp_eoi = Grammar.Entry.create gram "type" in
do {
  EXTEND
    ctyp_eoi:
      [ [ x = ctyp; EOI -> x ] ]
    ;
  END;
  Quotation.add "ctyp" (apply_entry ctyp_eoi)
};

let patt_eoi = Grammar.Entry.create gram "pattern" in
do {
  EXTEND
    patt_eoi:
      [ [ x = patt; EOI -> x ] ]
    ;
  END;
  Quotation.add "patt" (apply_entry patt_eoi)
};

let expr_eoi = Grammar.Entry.create gram "expression" in
do {
  EXTEND
    expr_eoi:
      [ [ x = expr; EOI -> x ] ]
    ;
  END;
  Quotation.add "expr" (apply_entry expr_eoi)
};

let module_type_eoi = Grammar.Entry.create gram "module type" in
do {
  EXTEND
    module_type_eoi:
      [ [ x = module_type; EOI -> x ] ]
    ;
  END;
  Quotation.add "module_type" (apply_entry module_type_eoi)
};

let module_expr_eoi = Grammar.Entry.create gram "module expression" in
do {
  EXTEND
    module_expr_eoi:
      [ [ x = module_expr; EOI -> x ] ]
    ;
  END;
  Quotation.add "module_expr" (apply_entry module_expr_eoi)
};

let class_type_eoi = Grammar.Entry.create gram "class_type" in
do {
  EXTEND
    class_type_eoi:
      [ [ x = class_type; EOI -> x ] ]
    ;
  END;
  Quotation.add "class_type" (apply_entry class_type_eoi)
};

let class_expr_eoi = Grammar.Entry.create gram "class_expr" in
do {
  EXTEND
    class_expr_eoi:
      [ [ x = class_expr; EOI -> x ] ]
    ;
  END;
  Quotation.add "class_expr" (apply_entry class_expr_eoi)
};

let class_sig_item_eoi = Grammar.Entry.create gram "class_sig_item" in
do {
  EXTEND
    class_sig_item_eoi:
      [ [ x = class_sig_item; EOI -> x ] ]
    ;
  END;
  Quotation.add "class_sig_item" (apply_entry class_sig_item_eoi)
};

let class_str_item_eoi = Grammar.Entry.create gram "class_str_item" in
do {
  EXTEND
    class_str_item_eoi:
      [ [ x = class_str_item; EOI -> x ] ]
    ;
  END;
  Quotation.add "class_str_item" (apply_entry class_str_item_eoi)
};

let with_constr_eoi = Grammar.Entry.create gram "with constr" in
do {
  EXTEND
    with_constr_eoi:
      [ [ x = with_constr; EOI -> x ] ]
    ;
  END;
  Quotation.add "with_constr" (apply_entry with_constr_eoi)
};

let row_field_eoi = Grammar.Entry.create gram "row_field" in
do {
  EXTEND
    row_field_eoi:
      [ [ x = row_field; EOI -> x ] ]
    ;
  END;
  Quotation.add "row_field" (apply_entry row_field_eoi)
};
