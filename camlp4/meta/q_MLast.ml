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

value gram = Grammar.create (Plexer.make ());

type ast =
  [ Node of string and list ast
  | List of list ast
  | Tuple of list ast
  | Option of option ast
  | Str of string
  | Chr of string
  | Bool of bool
  | Cons of ast and ast
  | Append of ast and ast
  | Record of list (string * ast)
  | Loc
  | Antiquot of MLast.loc and string ]
;
value antiquot k (bp, ep) x =
  let shift =
    if k = "" then String.length "$"
    else String.length "$" + String.length k + String.length ":"
  in
  Antiquot (shift + bp, shift + ep) x
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

value o2b =
  fun
  [ Option (Some _) -> Bool True
  | Option None -> Bool False
  | x -> x ]
;

value mksequence _ =
  fun
  [ List [e] -> e
  | el -> Node "ExSeq" [Loc; el] ]
;

value mkmatchcase _ p aso w e =
  let p =
    match aso with
    [ Option (Some p2) -> Node "PaAli" [Loc; p; p2]
    | Option None -> p
    | _ -> Node "PaAli" [Loc; p; aso] ]
  in
  Tuple [p; w; e]
;

value mkumin _ f arg =
  match arg with
  [ Node "ExInt" [Loc; Str n] when int_of_string n > 0 ->
      let n = "-" ^ n in
      Node "ExInt" [Loc; Str n]
  | Node "ExFlo" [Loc; Str n] when float_of_string n > 0.0 ->
      let n = "-" ^ n in
      Node "ExFlo" [Loc; Str n]
  | _ ->
      let f = "~" ^ f in
      Node "ExApp" [Loc; Node "ExLid" [Loc; Str f]; arg] ]
;

value mkuminpat _ f is_int s =
  if is_int then Node "PaInt" [Loc; s] else Node "PaFlo" [Loc; s]
;

value mklistexp _ last =
  loop True where rec loop top =
    fun
    [ List [] ->
        match last with
        [ Option (Some e) -> e
        | Option None -> Node "ExUid" [Loc; Str "[]"]
        | a -> a ]
    | List [e1 :: el] ->
        Node "ExApp"
          [Loc; Node "ExApp" [Loc; Node "ExUid" [Loc; Str "::"]; e1];
           loop False (List el)]
    | a -> a ]
;

value mklistpat _ last =
  loop True where rec loop top =
    fun
    [ List [] ->
        match last with
        [ Option (Some p) -> p
        | Option None -> Node "PaUid" [Loc; Str "[]"]
        | a -> a ]
    | List [p1 :: pl] ->
        Node "PaApp"
          [Loc; Node "PaApp" [Loc; Node "PaUid" [Loc; Str "::"]; p1];
           loop False (List pl)]
    | a -> a ]
;

value mkexprident loc i j =
  let rec loop m =
    fun
    [ Node "ExAcc" [_; x; y] -> loop (Node "ExAcc" [Loc; m; x]) y
    | e -> Node "ExAcc" [Loc; m; e] ]
  in
  loop (Node "ExUid" [Loc; i]) j
;

value mkassert loc e =
  let f = Node "ExStr" [Loc; Str Pcaml.input_file.val] in
  let bp = Node "ExInt" [Loc; Str (string_of_int (fst loc))] in
  let ep = Node "ExInt" [Loc; Str (string_of_int (snd loc))] in
  let raiser =
    Node "ExApp"
      [Loc; Node "ExLid" [Loc; Str "raise"];
       Node "ExApp"
         [Loc; Node "ExUid" [Loc; Str "Assert_failure"];
          Node "ExTup" [Loc; List [f; bp; ep]]]]
  in
  match e with
  [ Node "ExUid" [_; Str "False"] -> raiser
  | _ ->
      if Pcaml.no_assert.val then Node "ExUid" [Loc; Str "()"]
      else Node "ExIfe" [Loc; e; Node "ExUid" [Loc; Str "()"]; raiser] ]
;

value not_yet_warned = ref True;
value warning_seq () =
  if not_yet_warned.val then do {
    not_yet_warned.val := False;
    Printf.eprintf
      "\
*** warning: use of old syntax for sequences in expr quotation\n";
    flush stderr
  }
  else ()
;

EXTEND
  GLOBAL: sig_item str_item ctyp patt expr module_type module_expr class_type
    class_expr class_sig_item class_str_item;
  module_expr:
    [ [ "functor"; "("; i = a_UIDENT; ":"; t = module_type; ")"; "->";
        me = SELF ->
          Node "MeFun" [Loc; i; t; me]
      | "struct"; st = SLIST0 [ s = str_item; ";" -> s ]; "end" ->
          Node "MeStr" [Loc; st] ]
    | [ me1 = SELF; me2 = SELF -> Node "MeApp" [Loc; me1; me2] ]
    | [ me1 = SELF; "."; me2 = SELF -> Node "MeAcc" [Loc; me1; me2] ]
    | "simple"
      [ i = a_UIDENT -> Node "MeUid" [Loc; i]
      | "("; me = SELF; ":"; mt = module_type; ")" ->
          Node "MeTyc" [Loc; me; mt]
      | "("; me = SELF; ")" -> me ] ]
  ;
  str_item:
    [ "top"
      [ "declare"; st = SLIST0 [ s = str_item; ";" -> s ]; "end" ->
          Node "StDcl" [Loc; st]
      | "exception"; ctl = constructor_declaration; b = rebind_exn ->
          let (_, c, tl) =
            match ctl with
            [ Tuple [xx1; xx2; xx3] -> (xx1, xx2, xx3)
            | _ -> match () with [] ]
          in
          Node "StExc" [Loc; c; tl; b]
      | "external"; i = a_LIDENT; ":"; t = ctyp; "="; pd = SLIST1 a_STRING ->
          Node "StExt" [Loc; i; t; pd]
      | "include"; me = module_expr -> Node "StInc" [Loc; me]
      | "module"; i = a_UIDENT; mb = module_binding ->
          Node "StMod" [Loc; i; mb]
      | "module"; "type"; i = a_UIDENT; "="; mt = module_type ->
          Node "StMty" [Loc; i; mt]
      | "open"; i = mod_ident -> Node "StOpn" [Loc; i]
      | "type"; tdl = SLIST1 type_declaration SEP "and" ->
          Node "StTyp" [Loc; tdl]
      | "value"; r = rec_flag; l = SLIST1 let_binding SEP "and" ->
          Node "StVal" [Loc; r; l]
      | e = expr -> Node "StExp" [Loc; e] ] ]
  ;
  rebind_exn:
    [ [ "="; sl = mod_ident -> sl
      | -> List [] ] ]
  ;
  module_binding:
    [ RIGHTA
      [ "("; m = a_UIDENT; ":"; mt = module_type; ")"; mb = SELF ->
          Node "MeFun" [Loc; m; mt; mb]
      | ":"; mt = module_type; "="; me = module_expr ->
          Node "MeTyc" [Loc; me; mt]
      | "="; me = module_expr -> me ] ]
  ;
  module_type:
    [ [ "functor"; "("; i = a_UIDENT; ":"; t = SELF; ")"; "->"; mt = SELF ->
          Node "MtFun" [Loc; i; t; mt] ]
    | [ mt = SELF; "with"; wcl = SLIST1 with_constr SEP "and" ->
          Node "MtWit" [Loc; mt; wcl] ]
    | [ "sig"; sg = SLIST0 [ s = sig_item; ";" -> s ]; "end" ->
          Node "MtSig" [Loc; sg] ]
    | [ m1 = SELF; m2 = SELF -> Node "MtApp" [Loc; m1; m2] ]
    | [ m1 = SELF; "."; m2 = SELF -> Node "MtAcc" [Loc; m1; m2] ]
    | "simple"
      [ i = a_UIDENT -> Node "MtUid" [Loc; i]
      | i = a_LIDENT -> Node "MtLid" [Loc; i]
      | "("; mt = SELF; ")" -> mt ] ]
  ;
  sig_item:
    [ "top"
      [ "declare"; st = SLIST0 [ s = sig_item; ";" -> s ]; "end" ->
          Node "SgDcl" [Loc; st]
      | "exception"; ctl = constructor_declaration ->
          let (_, c, tl) =
            match ctl with
            [ Tuple [xx1; xx2; xx3] -> (xx1, xx2, xx3)
            | _ -> match () with [] ]
          in
          Node "SgExc" [Loc; c; tl]
      | "external"; i = a_LIDENT; ":"; t = ctyp; "="; pd = SLIST1 a_STRING ->
          Node "SgExt" [Loc; i; t; pd]
      | "include"; mt = module_type -> Node "SgInc" [Loc; mt]
      | "module"; i = a_UIDENT; mt = module_declaration ->
          Node "SgMod" [Loc; i; mt]
      | "module"; "type"; i = a_UIDENT; "="; mt = module_type ->
          Node "SgMty" [Loc; i; mt]
      | "open"; i = mod_ident -> Node "SgOpn" [Loc; i]
      | "type"; tdl = SLIST1 type_declaration SEP "and" ->
          Node "SgTyp" [Loc; tdl]
      | "value"; i = a_LIDENT; ":"; t = ctyp -> Node "SgVal" [Loc; i; t] ] ]
  ;
  module_declaration:
    [ RIGHTA
      [ ":"; mt = module_type -> mt
      | "("; i = a_UIDENT; ":"; t = module_type; ")"; mt = SELF ->
          Node "MtFun" [Loc; i; t; mt] ] ]
  ;
  with_constr:
    [ [ "type"; i = mod_ident; tpl = SLIST0 type_parameter; "="; t = ctyp ->
          Node "WcTyp" [Loc; i; tpl; t]
      | "module"; i = mod_ident; "="; mt = module_type ->
          Node "WcMod" [Loc; i; mt] ] ]
  ;
  expr:
    [ "top" RIGHTA
      [ "let"; r = rec_flag; l = SLIST1 let_binding SEP "and"; "in";
        x = SELF ->
          Node "ExLet" [Loc; r; l; x]
      | "let"; "module"; m = a_UIDENT; mb = module_binding; "in"; e = SELF ->
          Node "ExLmd" [Loc; m; mb; e]
      | "fun"; "["; l = SLIST0 match_case SEP "|"; "]" ->
          Node "ExFun" [Loc; l]
      | "fun"; p = ipatt; e = fun_def ->
          Node "ExFun" [Loc; List [Tuple [p; Option None; e]]]
      | "match"; e = SELF; "with"; "["; l = SLIST0 match_case SEP "|"; "]" ->
          Node "ExMat" [Loc; e; l]
      | "match"; e = SELF; "with"; p1 = ipatt; "->"; e1 = SELF ->
          Node "ExMat" [Loc; e; List [Tuple [p1; Option None; e1]]]
      | "try"; e = SELF; "with"; "["; l = SLIST0 match_case SEP "|"; "]" ->
          Node "ExTry" [Loc; e; l]
      | "try"; e = SELF; "with"; p1 = ipatt; "->"; e1 = SELF ->
          Node "ExTry" [Loc; e; List [Tuple [p1; Option None; e1]]]
      | "if"; e1 = SELF; "then"; e2 = SELF; "else"; e3 = SELF ->
          Node "ExIfe" [Loc; e1; e2; e3]
      | "do"; "{"; seq = sequence; "}" -> mksequence loc seq
      | "for"; i = a_LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; "{"; seq = sequence; "}" ->
          Node "ExFor" [Loc; i; e1; e2; df; seq]
      | "while"; e = SELF; "do"; "{"; seq = sequence; "}" ->
          Node "ExWhi" [Loc; e; seq] ]
    | "where"
      [ e = SELF; "where"; rf = rec_flag; lb = let_binding ->
          Node "ExLet" [Loc; rf; List [lb]; e] ]
    | ":=" NONA
      [ e1 = SELF; ":="; e2 = SELF; dummy -> Node "ExAss" [Loc; e1; e2] ]
    | "||" RIGHTA
      [ e1 = SELF; "||"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "||"]; e1]; e2] ]
    | "&&" RIGHTA
      [ e1 = SELF; "&&"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "&&"]; e1]; e2] ]
    | "<" LEFTA
      [ e1 = SELF; "<"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "<"]; e1]; e2]
      | e1 = SELF; ">"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str ">"]; e1]; e2]
      | e1 = SELF; "<="; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "<="]; e1]; e2]
      | e1 = SELF; ">="; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str ">="]; e1]; e2]
      | e1 = SELF; "="; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "="]; e1]; e2]
      | e1 = SELF; "<>"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "<>"]; e1]; e2]
      | e1 = SELF; "=="; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "=="]; e1]; e2]
      | e1 = SELF; "!="; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "!="]; e1]; e2] ]
    | "^" RIGHTA
      [ e1 = SELF; "^"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "^"]; e1]; e2]
      | e1 = SELF; "@"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "@"]; e1]; e2] ]
    | "+" LEFTA
      [ e1 = SELF; "+"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "+"]; e1]; e2]
      | e1 = SELF; "-"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "-"]; e1]; e2]
      | e1 = SELF; "+."; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "+."]; e1]; e2]
      | e1 = SELF; "-."; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "-."]; e1]; e2] ]
    | "*" LEFTA
      [ e1 = SELF; "*"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "*"]; e1]; e2]
      | e1 = SELF; "/"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "/"]; e1]; e2]
      | e1 = SELF; "*."; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "*."]; e1]; e2]
      | e1 = SELF; "/."; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "/."]; e1]; e2]
      | e1 = SELF; "land"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "land"]; e1]; e2]
      | e1 = SELF; "lor"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "lor"]; e1]; e2]
      | e1 = SELF; "lxor"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "lxor"]; e1]; e2]
      | e1 = SELF; "mod"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "mod"]; e1]; e2] ]
    | "**" RIGHTA
      [ e1 = SELF; "**"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "**"]; e1]; e2]
      | e1 = SELF; "asr"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "asr"]; e1]; e2]
      | e1 = SELF; "lsl"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "lsl"]; e1]; e2]
      | e1 = SELF; "lsr"; e2 = SELF ->
          Node "ExApp"
            [Loc; Node "ExApp" [Loc; Node "ExLid" [Loc; Str "lsr"]; e1]; e2] ]
    | "unary minus" NONA
      [ "-"; e = SELF -> mkumin loc "-" e
      | "-."; e = SELF -> mkumin loc "-." e ]
    | "apply" LEFTA
      [ e1 = SELF; e2 = SELF -> Node "ExApp" [Loc; e1; e2]
      | "assert"; e = SELF -> mkassert loc e
      | "lazy"; e = SELF -> Node "ExLaz" [Loc; e] ]
    | "." LEFTA
      [ e1 = SELF; "."; "("; e2 = SELF; ")" -> Node "ExAre" [Loc; e1; e2]
      | e1 = SELF; "."; "["; e2 = SELF; "]" -> Node "ExSte" [Loc; e1; e2]
      | e1 = SELF; "."; e2 = SELF -> Node "ExAcc" [Loc; e1; e2] ]
    | "~-" NONA
      [ "~-"; e = SELF -> Node "ExApp" [Loc; Node "ExLid" [Loc; Str "~-"]; e]
      | "~-."; e = SELF ->
          Node "ExApp" [Loc; Node "ExLid" [Loc; Str "~-."]; e] ]
    | "simple"
      [ s = a_INT -> Node "ExInt" [Loc; s]
      | s = a_FLOAT -> Node "ExFlo" [Loc; s]
      | s = a_STRING -> Node "ExStr" [Loc; s]
      | s = a_CHAR -> Node "ExChr" [Loc; s]
      | i = expr_ident -> i
      | "["; "]" -> Node "ExUid" [Loc; Str "[]"]
      | "["; el = SLIST1 expr SEP ";"; last = cons_expr_opt; "]" ->
          mklistexp loc last el
      | "[|"; el = SLIST0 expr SEP ";"; "|]" -> Node "ExArr" [Loc; el]
      | "{"; lel = SLIST1 label_expr SEP ";"; "}" ->
          Node "ExRec" [Loc; lel; Option None]
      | "{"; "("; e = SELF; ")"; "with"; lel = SLIST1 label_expr SEP ";";
        "}" ->
          Node "ExRec" [Loc; lel; Option (Some e)]
      | "("; ")" -> Node "ExUid" [Loc; Str "()"]
      | "("; e = SELF; ":"; t = ctyp; ")" -> Node "ExTyc" [Loc; e; t]
      | "("; e = SELF; ","; el = SLIST1 expr SEP ","; ")" ->
          Node "ExTup" [Loc; Cons e el]
      | "("; e = SELF; ")" -> e ] ]
  ;
  cons_expr_opt:
    [ [ "::"; e = expr -> Option (Some e)
      | -> Option None ] ]
  ;
  dummy:
    [ [ -> () ] ]
  ;
  sequence:
    [ [ "let"; rf = rec_flag; l = SLIST1 let_binding SEP "and";
        [ "in" | ";" ]; el = SELF ->
          List [Node "ExLet" [Loc; rf; l; mksequence loc el]]
      | e = expr; ";"; el = SELF -> Cons e el
      | e = expr; ";" -> List [e]
      | e = expr -> List [e] ] ]
  ;
  let_binding:
    [ [ p = ipatt; e = fun_binding -> Tuple [p; e] ] ]
  ;
  fun_binding:
    [ RIGHTA
      [ p = ipatt; e = SELF ->
          Node "ExFun" [Loc; List [Tuple [p; Option None; e]]]
      | "="; e = expr -> e
      | ":"; t = ctyp; "="; e = expr -> Node "ExTyc" [Loc; e; t] ] ]
  ;
  match_case:
    [ [ p = patt; aso = as_patt_opt; w = when_expr_opt; "->"; e = expr ->
          mkmatchcase loc p aso w e ] ]
  ;
  as_patt_opt:
    [ [ "as"; p = patt -> Option (Some p)
      | -> Option None ] ]
  ;
  when_expr_opt:
    [ [ "when"; e = expr -> Option (Some e)
      | -> Option None ] ]
  ;
  label_expr:
    [ [ i = patt_label_ident; e = fun_binding -> Tuple [i; e] ] ]
  ;
  expr_ident:
    [ RIGHTA
      [ i = a_LIDENT -> Node "ExLid" [Loc; i]
      | i = a_UIDENT -> Node "ExUid" [Loc; i]
      | i = a_UIDENT; "."; j = SELF -> mkexprident loc i j ] ]
  ;
  fun_def:
    [ RIGHTA
      [ p = ipatt; e = SELF ->
          Node "ExFun" [Loc; List [Tuple [p; Option None; e]]]
      | "->"; e = expr -> e ] ]
  ;
  patt:
    [ LEFTA
      [ p1 = SELF; "|"; p2 = SELF -> Node "PaOrp" [Loc; p1; p2] ]
    | NONA
      [ p1 = SELF; ".."; p2 = SELF -> Node "PaRng" [Loc; p1; p2] ]
    | LEFTA
      [ p1 = SELF; p2 = SELF -> Node "PaApp" [Loc; p1; p2] ]
    | LEFTA
      [ p1 = SELF; "."; p2 = SELF -> Node "PaAcc" [Loc; p1; p2] ]
    | "simple"
      [ s = a_LIDENT -> Node "PaLid" [Loc; s]
      | s = a_UIDENT -> Node "PaUid" [Loc; s]
      | s = a_INT -> Node "PaInt" [Loc; s]
      | s = a_FLOAT -> Node "PaFlo" [Loc; s]
      | s = a_STRING -> Node "PaStr" [Loc; s]
      | s = a_CHAR -> Node "PaChr" [Loc; s]
      | "-"; s = a_INT -> mkuminpat loc "-" True s
      | "-"; s = a_FLOAT -> mkuminpat loc "-" False s
      | "["; "]" -> Node "PaUid" [Loc; Str "[]"]
      | "["; pl = SLIST1 patt SEP ";"; last = cons_patt_opt; "]" ->
          mklistpat loc last pl
      | "[|"; pl = SLIST0 patt SEP ";"; "|]" -> Node "PaArr" [Loc; pl]
      | "{"; lpl = SLIST1 label_patt SEP ";"; "}" -> Node "PaRec" [Loc; lpl]
      | "("; ")" -> Node "PaUid" [Loc; Str "()"]
      | "("; p = SELF; ")" -> p
      | "("; p = SELF; ":"; t = ctyp; ")" -> Node "PaTyc" [Loc; p; t]
      | "("; p = SELF; "as"; p2 = SELF; ")" -> Node "PaAli" [Loc; p; p2]
      | "("; p = SELF; ","; pl = SLIST1 patt SEP ","; ")" ->
          Node "PaTup" [Loc; Cons p pl]
      | "_" -> Node "PaAny" [Loc] ] ]
  ;
  cons_patt_opt:
    [ [ "::"; p = patt -> Option (Some p)
      | -> Option None ] ]
  ;
  label_patt:
    [ [ i = patt_label_ident; "="; p = patt -> Tuple [i; p] ] ]
  ;
  patt_label_ident:
    [ LEFTA
      [ p1 = SELF; "."; p2 = SELF -> Node "PaAcc" [Loc; p1; p2] ]
    | "simple" RIGHTA
      [ i = a_UIDENT -> Node "PaUid" [Loc; i]
      | i = a_LIDENT -> Node "PaLid" [Loc; i] ] ]
  ;
  ipatt:
    [ [ "{"; lpl = SLIST1 label_ipatt SEP ";"; "}" -> Node "PaRec" [Loc; lpl]
      | "("; ")" -> Node "PaUid" [Loc; Str "()"]
      | "("; p = SELF; ")" -> p
      | "("; p = SELF; ":"; t = ctyp; ")" -> Node "PaTyc" [Loc; p; t]
      | "("; p = SELF; "as"; p2 = SELF; ")" -> Node "PaAli" [Loc; p; p2]
      | "("; p = SELF; ","; pl = SLIST1 ipatt SEP ","; ")" ->
          Node "PaTup" [Loc; Cons p pl]
      | s = a_LIDENT -> Node "PaLid" [Loc; s]
      | "_" -> Node "PaAny" [Loc] ] ]
  ;
  label_ipatt:
    [ [ i = patt_label_ident; "="; p = ipatt -> Tuple [i; p] ] ]
  ;
  type_declaration:
    [ [ n = type_patt; tpl = SLIST0 type_parameter; "="; tk = ctyp;
        cl = SLIST0 constrain ->
          Tuple [n; tpl; tk; cl] ] ]
  ;
  type_patt:
    [ [ n = a_LIDENT -> Tuple [Loc; n] ] ]
  ;
  constrain:
    [ [ "constraint"; t1 = ctyp; "="; t2 = ctyp -> Tuple [t1; t2] ] ]
  ;
  type_parameter:
    [ [ "'"; i = ident -> Tuple [i; Tuple [Bool False; Bool False]]
      | "+"; "'"; i = ident -> Tuple [i; Tuple [Bool True; Bool False]]
      | "-"; "'"; i = ident -> Tuple [i; Tuple [Bool False; Bool True]] ] ]
  ;
  ctyp:
    [ LEFTA
      [ t1 = SELF; "=="; t2 = SELF -> Node "TyMan" [Loc; t1; t2] ]
    | LEFTA
      [ t1 = SELF; "as"; t2 = SELF -> Node "TyAli" [Loc; t1; t2] ]
    | "arrow" RIGHTA
      [ t1 = SELF; "->"; t2 = SELF -> Node "TyArr" [Loc; t1; t2] ]
    | LEFTA
      [ t1 = SELF; t2 = SELF -> Node "TyApp" [Loc; t1; t2] ]
    | LEFTA
      [ t1 = SELF; "."; t2 = SELF -> Node "TyAcc" [Loc; t1; t2] ]
    | "simple"
      [ "'"; i = ident -> Node "TyQuo" [Loc; i]
      | "_" -> Node "TyAny" [Loc]
      | i = a_LIDENT -> Node "TyLid" [Loc; i]
      | i = a_UIDENT -> Node "TyUid" [Loc; i]
      | "("; t = SELF; "*"; tl = SLIST1 ctyp SEP "*"; ")" ->
          Node "TyTup" [Loc; Cons t tl]
      | "("; t = SELF; ")" -> t
      | "["; cdl = SLIST0 constructor_declaration SEP "|"; "]" ->
          Node "TySum" [Loc; cdl]
      | "{"; ldl = SLIST1 label_declaration SEP ";"; "}" ->
          Node "TyRec" [Loc; ldl] ] ]
  ;
  constructor_declaration:
    [ [ ci = a_UIDENT; "of"; cal = SLIST1 ctyp SEP "and" ->
          Tuple [Loc; ci; cal]
      | ci = a_UIDENT -> Tuple [Loc; ci; List []] ] ]
  ;
  label_declaration:
    [ [ i = a_LIDENT; ":"; mf = mutable_flag; t = ctyp ->
          Tuple [Loc; i; mf; t] ] ]
  ;
  ident:
    [ [ i = a_LIDENT -> i
      | i = a_UIDENT -> i ] ]
  ;
  mod_ident:
    [ RIGHTA
      [ i = a_UIDENT -> List [i]
      | i = a_LIDENT -> List [i]
      | i = a_UIDENT; "."; j = SELF -> Cons i j ] ]
  ;
  (* Objects and Classes *)
  str_item:
    [ [ "class"; cd = SLIST1 class_declaration SEP "and" ->
          Node "StCls" [Loc; cd]
      | "class"; "type"; ctd = SLIST1 class_type_declaration SEP "and" ->
          Node "StClt" [Loc; ctd] ] ]
  ;
  sig_item:
    [ [ "class"; cd = SLIST1 class_description SEP "and" ->
          Node "SgCls" [Loc; cd]
      | "class"; "type"; ctd = SLIST1 class_type_declaration SEP "and" ->
          Node "SgClt" [Loc; ctd] ] ]
  ;
  class_declaration:
    [ [ vf = virtual_flag; i = a_LIDENT; ctp = class_type_parameters;
        cfb = class_fun_binding ->
          Record
            [("ciLoc", Loc); ("ciVir", vf); ("ciPrm", ctp); ("ciNam", i);
             ("ciExp", cfb)] ] ]
  ;
  class_fun_binding:
    [ [ "="; ce = class_expr -> ce
      | ":"; ct = class_type; "="; ce = class_expr ->
          Node "CeTyc" [Loc; ce; ct]
      | p = ipatt; cfb = SELF -> Node "CeFun" [Loc; p; cfb] ] ]
  ;
  class_type_parameters:
    [ [ -> Tuple [Loc; List []]
      | "["; tpl = SLIST1 type_parameter SEP ","; "]" -> Tuple [Loc; tpl] ] ]
  ;
  class_fun_def:
    [ [ p = patt LEVEL "simple"; "->"; ce = class_expr ->
          Node "CeFun" [Loc; p; ce]
      | p = patt LEVEL "simple"; cfd = SELF -> Node "CeFun" [Loc; p; cfd] ] ]
  ;
  class_expr:
    [ "top"
      [ "fun"; cfd = class_fun_def -> cfd
      | "let"; rf = rec_flag; lb = SLIST1 let_binding SEP "and"; "in";
        ce = SELF ->
          Node "CeLet" [Loc; rf; lb; ce] ]
    | "apply" NONA
      [ ce = SELF; e = expr LEVEL "simple" -> Node "CeApp" [Loc; ce; e] ]
    | "simple"
      [ ci = class_longident; "["; ctcl = SLIST0 ctyp SEP ","; "]" ->
          Node "CeCon" [Loc; ci; ctcl]
      | ci = class_longident -> Node "CeCon" [Loc; ci; List []]
      | "object"; cspo = class_self_patt_opt; cf = class_structure; "end" ->
          Node "CeStr" [Loc; cspo; cf]
      | "("; ce = SELF; ":"; ct = class_type; ")" ->
          Node "CeTyc" [Loc; ce; ct]
      | "("; ce = SELF; ")" -> ce ] ]
  ;
  class_structure:
    [ [ cf = SLIST0 [ cf = class_str_item; ";" -> cf ] -> cf ] ]
  ;
  class_self_patt_opt:
    [ [ "("; p = patt; ")" -> Option (Some p)
      | "("; p = patt; ":"; t = ctyp; ")" ->
          Option (Some (Node "PaTyc" [Loc; p; t]))
      | -> Option None ] ]
  ;
  class_str_item:
    [ [ "declare"; st = SLIST0 [ s = class_str_item; ";" -> s ]; "end" ->
          Node "CrDcl" [Loc; st]
      | "inherit"; ce = class_expr; pb = as_lident_opt ->
          Node "CrInh" [Loc; ce; pb]
      | "value"; labmfe = cvalue ->
          let (lab, mf, e) =
            match labmfe with
            [ Tuple [xx1; xx2; xx3] -> (xx1, xx2, xx3)
            | _ -> match () with [] ]
          in
          Node "CrVal" [Loc; lab; mf; e]
      | "method"; "virtual"; "private"; l = label; ":"; t = ctyp ->
          Node "CrVir" [Loc; l; Bool True; t]
      | "method"; "virtual"; l = label; ":"; t = ctyp ->
          Node "CrVir" [Loc; l; Bool False; t]
      | "method"; "private"; l = label; fb = fun_binding ->
          Node "CrMth" [Loc; l; Bool True; fb]
      | "method"; l = label; fb = fun_binding ->
          Node "CrMth" [Loc; l; Bool False; fb]
      | "type"; t1 = ctyp; "="; t2 = ctyp -> Node "CrCtr" [Loc; t1; t2]
      | "initializer"; se = expr -> Node "CrIni" [Loc; se] ] ]
  ;
  as_lident_opt:
    [ [ "as"; i = a_LIDENT -> Option (Some i)
      | -> Option None ] ]
  ;
  cvalue:
    [ [ mf = mutable_flag; l = label; "="; e = expr -> Tuple [l; mf; e]
      | mf = mutable_flag; l = label; ":"; t = ctyp; "="; e = expr ->
          Tuple [l; mf; Node "ExTyc" [Loc; e; t]]
      | mf = mutable_flag; l = label; ":"; t = ctyp; ":>"; t2 = ctyp; "=";
        e = expr ->
          Tuple [l; mf; Node "ExCoe" [Loc; e; Option (Some t); t2]]
      | mf = mutable_flag; l = label; ":>"; t = ctyp; "="; e = expr ->
          Tuple [l; mf; Node "ExCoe" [Loc; e; Option None; t]] ] ]
  ;
  label:
    [ [ i = a_LIDENT -> i ] ]
  ;
  class_type:
    [ [ "["; t = ctyp; "]"; "->"; ct = SELF -> Node "CtFun" [Loc; t; ct]
      | id = clty_longident; "["; tl = SLIST1 ctyp SEP ","; "]" ->
          Node "CtCon" [Loc; id; tl]
      | id = clty_longident -> Node "CtCon" [Loc; id; List []]
      | "object"; cst = class_self_type_opt;
        csf = SLIST0 [ csf = class_sig_item; ";" -> csf ]; "end" ->
          Node "CtSig" [Loc; cst; csf] ] ]
  ;
  class_self_type_opt:
    [ [ "("; t = ctyp; ")" -> Option (Some t)
      | -> Option None ] ]
  ;
  class_sig_item:
    [ [ "declare"; st = SLIST0 [ s = class_sig_item; ";" -> s ]; "end" ->
          Node "CgDcl" [Loc; st]
      | "inherit"; cs = class_type -> Node "CgInh" [Loc; cs]
      | "value"; mf = mutable_flag; l = label; ":"; t = ctyp ->
          Node "CgVal" [Loc; l; mf; t]
      | "method"; "virtual"; "private"; l = label; ":"; t = ctyp ->
          Node "CgVir" [Loc; l; Bool True; t]
      | "method"; "virtual"; l = label; ":"; t = ctyp ->
          Node "CgVir" [Loc; l; Bool False; t]
      | "method"; "private"; l = label; ":"; t = ctyp ->
          Node "CgMth" [Loc; l; Bool True; t]
      | "method"; l = label; ":"; t = ctyp ->
          Node "CgMth" [Loc; l; Bool False; t]
      | "type"; t1 = ctyp; "="; t2 = ctyp -> Node "CgCtr" [Loc; t1; t2] ] ]
  ;
  class_description:
    [ [ vf = virtual_flag; n = a_LIDENT; ctp = class_type_parameters; ":";
        ct = class_type ->
          Record
            [("ciLoc", Loc); ("ciVir", vf); ("ciPrm", ctp); ("ciNam", n);
             ("ciExp", ct)] ] ]
  ;
  class_type_declaration:
    [ [ vf = virtual_flag; n = a_LIDENT; ctp = class_type_parameters; "=";
        cs = class_type ->
          Record
            [("ciLoc", Loc); ("ciVir", vf); ("ciPrm", ctp); ("ciNam", n);
             ("ciExp", cs)] ] ]
  ;
  expr: LEVEL "apply"
    [ LEFTA
      [ "new"; i = class_longident -> Node "ExNew" [Loc; i] ] ]
  ;
  expr: LEVEL "."
    [ [ e = SELF; "#"; lab = label -> Node "ExSnd" [Loc; e; lab] ] ]
  ;
  expr: LEVEL "simple"
    [ [ "("; e = SELF; ":"; t = ctyp; ":>"; t2 = ctyp; ")" ->
          Node "ExCoe" [Loc; e; Option (Some t); t2]
      | "("; e = SELF; ":>"; t = ctyp; ")" ->
          Node "ExCoe" [Loc; e; Option None; t]
      | "{<"; ">}" -> Node "ExOvr" [Loc; List []]
      | "{<"; fel = field_expr_list; ">}" -> Node "ExOvr" [Loc; fel] ] ]
  ;
  field_expr_list:
    [ [ l = label; "="; e = expr; ";"; fel = SELF -> Cons (Tuple [l; e]) fel
      | l = label; "="; e = expr; ";" -> List [Tuple [l; e]]
      | l = label; "="; e = expr -> List [Tuple [l; e]] ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ "#"; id = class_longident -> Node "TyCls" [Loc; id]
      | "<"; mlv = meth_list; ">" ->
          let (ml, v) =
            match mlv with
            [ Tuple [xx1; xx2] -> (xx1, xx2)
            | _ -> match () with [] ]
          in
          Node "TyObj" [Loc; ml; v]
      | "<"; ">" -> Node "TyObj" [Loc; List []; Bool False] ] ]
  ;
  meth_list:
    [ [ f = field; ";"; mlv = SELF ->
          let (ml, v) =
            match mlv with
            [ Tuple [xx1; xx2] -> (xx1, xx2)
            | _ -> match () with [] ]
          in
          Tuple [Cons f ml; v]
      | f = field; ";" -> Tuple [List [f]; Bool False]
      | f = field -> Tuple [List [f]; Bool False]
      | ".." -> Tuple [List []; Bool True] ] ]
  ;
  field:
    [ [ lab = a_LIDENT; ":"; t = ctyp -> Tuple [lab; t] ] ]
  ;
  clty_longident:
    [ [ m = a_UIDENT; "."; l = SELF -> Cons m l
      | i = a_LIDENT -> List [i] ] ]
  ;
  class_longident:
    [ [ m = a_UIDENT; "."; l = SELF -> Cons m l
      | i = a_LIDENT -> List [i] ] ]
  ;
  (* Labels *)
  ctyp: AFTER "arrow"
    [ NONA
      [ i = a_TILDEIDENTCOLON; t = SELF -> Node "TyLab" [Loc; i; t]
      | i = a_QUESTIONIDENTCOLON; t = SELF -> Node "TyOlb" [Loc; i; t] ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ "[|"; rfl = SLIST0 row_field SEP "|"; "|]" ->
          Node "TyVrn" [Loc; rfl; Option None]
      | "[|"; ">"; rfl = row_field_list; "|]" ->
          Node "TyVrn" [Loc; rfl; Option (Some (Option None))]
      | "[|"; "<"; rfl = row_field_list; "|]" ->
          Node "TyVrn" [Loc; rfl; Option (Some (Option (Some (List []))))]
      | "[|"; "<"; rfl = row_field_list; ">"; ntl = SLIST1 name_tag; "|]" ->
          Node "TyVrn" [Loc; rfl; Option (Some (Option (Some ntl)))] ] ]
  ;
  row_field_list:
    [ [ rfl = SLIST1 row_field SEP "|" -> rfl ] ]
  ;
  row_field:
    [ [ "`"; i = ident -> Node "RfTag" [i; Bool True; List []]
      | "`"; i = ident; "of"; ao = amp_flag; l = SLIST1 ctyp SEP "&" ->
          Node "RfTag" [i; ao; l]
      | t = ctyp -> Node "RfInh" [t] ] ]
  ;
  name_tag:
    [ [ "`"; i = ident -> i ] ]
  ;
  patt: LEVEL "simple"
    [ [ "`"; s = ident -> Node "PaVrn" [Loc; s]
      | "#"; sl = mod_ident -> Node "PaTyp" [Loc; sl]
      | i = a_TILDEIDENT; ":"; p = SELF -> Node "PaLab" [Loc; i; p]
      | i = a_TILDEIDENT -> Node "PaLab" [Loc; i; Node "PaLid" [Loc; i]]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; ")" ->
          Node "PaOlb" [Loc; i; p; Option None]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; "="; e = expr; ")" ->
          Node "PaOlb" [Loc; i; p; Option (Some e)]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; ":"; t = ctyp; ")" ->
          Node "PaOlb" [Loc; i; Node "PaTyc" [Loc; p; t]; Option None]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; ":"; t = ctyp; "="; e = expr;
        ")" ->
          Node "PaOlb" [Loc; i; Node "PaTyc" [Loc; p; t]; Option (Some e)]
      | i = a_QUESTIONIDENT ->
          Node "PaOlb" [Loc; i; Node "PaLid" [Loc; i]; Option None]
      | "?"; "("; i = a_LIDENT; "="; e = expr; ")" ->
          Node "PaOlb" [Loc; i; Node "PaLid" [Loc; i]; Option (Some e)]
      | "?"; "("; i = a_LIDENT; ":"; t = ctyp; "="; e = expr; ")" ->
          Node "PaOlb"
            [Loc; i; Node "PaTyc" [Loc; Node "PaLid" [Loc; i]; t];
             Option (Some e)] ] ]
  ;
  ipatt:
    [ [ i = a_TILDEIDENT; ":"; p = SELF -> Node "PaLab" [Loc; i; p]
      | i = a_TILDEIDENT -> Node "PaLab" [Loc; i; Node "PaLid" [Loc; i]]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; ")" ->
          Node "PaOlb" [Loc; i; p; Option None]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; "="; e = expr; ")" ->
          Node "PaOlb" [Loc; i; p; Option (Some e)]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; ":"; t = ctyp; ")" ->
          Node "PaOlb" [Loc; i; Node "PaTyc" [Loc; p; t]; Option None]
      | i = a_QUESTIONIDENT; ":"; "("; p = SELF; ":"; t = ctyp; "="; e = expr;
        ")" ->
          Node "PaOlb" [Loc; i; Node "PaTyc" [Loc; p; t]; Option (Some e)]
      | i = a_QUESTIONIDENT ->
          Node "PaOlb" [Loc; i; Node "PaLid" [Loc; i]; Option None]
      | "?"; "("; i = a_LIDENT; "="; e = expr; ")" ->
          Node "PaOlb" [Loc; i; Node "PaLid" [Loc; i]; Option (Some e)]
      | "?"; "("; i = a_LIDENT; ":"; t = ctyp; "="; e = expr; ")" ->
          Node "PaOlb"
            [Loc; i; Node "PaTyc" [Loc; Node "PaLid" [Loc; i]; t];
             Option (Some e)] ] ]
  ;
  expr: AFTER "apply"
    [ "label" NONA
      [ lab = TILDEIDENTCOLON; e = SELF -> Node "ExLab" [Loc; Str lab; e]
      | lab = TILDEIDENT ->
          Node "ExLab" [Loc; Str lab; Node "ExLid" [Loc; Str lab]]
      | lab = QUESTIONIDENTCOLON; e = SELF -> Node "ExOlb" [Loc; Str lab; e]
      | lab = QUESTIONIDENT ->
          Node "ExOlb" [Loc; Str lab; Node "ExLid" [Loc; Str lab]]
      | "~"; a = anti_; ":"; e = SELF -> Node "ExLab" [Loc; a; e]
      | "~"; a = anti_ -> Node "ExLab" [Loc; a; Node "ExLid" [Loc; a]]
      | "?"; a = anti_; ":"; e = SELF -> Node "ExOlb" [Loc; a; e]
      | "?"; a = anti_ -> Node "ExOlb" [Loc; a; Node "ExLid" [Loc; a]] ] ]
  ;
  expr: LEVEL "simple"
    [ [ "`"; s = ident -> Node "ExVrn" [Loc; s] ] ]
  ;
  rec_flag:
    [ [ "rec" -> Bool True
      | -> Bool False ] ]
  ;
  direction_flag:
    [ [ "to" -> Bool True
      | "downto" -> Bool False ] ]
  ;
  mutable_flag:
    [ [ "mutable" -> Bool True
      | -> Bool False ] ]
  ;
  virtual_flag:
    [ [ "virtual" -> Bool True
      | -> Bool False ] ]
  ;
  amp_flag:
    [ [ "&" -> Bool True
      | -> Bool False ] ]
  ;
  (* Antiquotations *)
  str_item:
    [ [ "#"; n = a_LIDENT; dp = dir_param -> Node "StDir" [Loc; n; dp] ] ]
  ;
  sig_item:
    [ [ "#"; n = a_LIDENT; dp = dir_param -> Node "SgDir" [Loc; n; dp] ] ]
  ;
  dir_param:
    [ [ a = anti_opt -> a
      | e = expr -> Option (Some e)
      | -> Option None ] ]
  ;
  module_expr: LEVEL "simple"
    [ [ a = ANTIQUOT "mexp" -> antiquot "mexp" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a ] ]
  ;
  str_item: LEVEL "top"
    [ [ a = ANTIQUOT "stri" -> antiquot "stri" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a ] ]
  ;
  module_type: LEVEL "simple"
    [ [ a = ANTIQUOT "mtyp" -> antiquot "mtyp" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a ] ]
  ;
  sig_item: LEVEL "top"
    [ [ a = ANTIQUOT "sigi" -> antiquot "sigi" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a ] ]
  ;
  expr: LEVEL "simple"
    [ [ a = ANTIQUOT "exp" -> antiquot "exp" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a
      | a = ANTIQUOT "anti" -> Node "ExAnt" [Loc; antiquot "anti" loc a]
      | "("; el = anti_list; ")" -> Node "ExTup" [Loc; el] ] ]
  ;
  sequence:
    [ [ a = ANTIQUOT "list" -> antiquot "list" loc a ] ]
  ;
  expr_ident:
    [ [ a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  patt: LEVEL "simple"
    [ [ a = ANTIQUOT "pat" -> antiquot "pat" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a
      | a = ANTIQUOT "anti" -> Node "PaAnt" [Loc; antiquot "anti" loc a]
      | "("; pl = anti_list; ")" -> Node "PaTup" [Loc; pl] ] ]
  ;
  patt_label_ident: LEVEL "simple"
    [ [ a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  when_expr_opt:
    [ [ a = ANTIQUOT "when" -> antiquot "when" loc a ] ]
  ;
  ipatt:
    [ [ a = ANTIQUOT "pat" -> antiquot "pat" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a
      | a = ANTIQUOT "anti" -> Node "PaAnt" [Loc; antiquot "anti" loc a]
      | "("; pl = anti_list; ")" -> Node "PaTup" [Loc; pl] ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ a = ANTIQUOT "typ" -> antiquot "typ" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a
      | "("; tl = anti_list; ")" -> Node "TyTup" [Loc; tl] ] ]
  ;
  mod_ident:
    [ [ a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  class_expr: LEVEL "simple"
    [ [ a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  class_str_item:
    [ [ a = ANTIQUOT "" -> antiquot "" loc a ] ]
  ;
  class_sig_item:
    [ [ a = ANTIQUOT "" -> antiquot "" loc a ] ]
  ;
  class_type:
    [ [ a = ANTIQUOT "" -> antiquot "" loc a ] ]
  ;
  class_self_patt_opt:
    [ [ a = ANTIQUOT "opt" -> antiquot "opt" loc a ] ]
  ;
  as_lident_opt:
    [ [ a = ANTIQUOT "as" -> antiquot "as" loc a ] ]
  ;
  class_self_type_opt:
    [ [ a = ANTIQUOT "opt" -> antiquot "opt" loc a ] ]
  ;
  meth_list:
    [ [ a = anti_list -> Tuple [a; Bool False]
      | a = anti_list; b = anti_ -> Tuple [a; b] ] ]
  ;
  expr: LEVEL "simple"
    [ [ "{<"; fel = anti_list; ">}" -> Node "ExOvr" [Loc; fel] ] ]
  ;
  clty_longident:
    [ [ a = anti_list -> a ] ]
  ;
  class_longident:
    [ [ a = anti_list -> a ] ]
  ;
  patt: LEVEL "simple"
    [ [ "#"; a = anti_list -> Node "PaTyp" [Loc; a] ] ]
  ;
  a_UIDENT:
    [ [ a = ANTIQUOT "uid" -> antiquot "uid" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a
      | i = UIDENT -> Str i ] ]
  ;
  a_LIDENT:
    [ [ a = ANTIQUOT "lid" -> antiquot "lid" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a
      | i = LIDENT -> Str i ] ]
  ;
  a_INT:
    [ [ a = ANTIQUOT "int" -> antiquot "int" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a
      | s = INT -> Str s ] ]
  ;
  a_FLOAT:
    [ [ a = ANTIQUOT "flo" -> antiquot "flo" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a
      | s = FLOAT -> Str s ] ]
  ;
  a_STRING:
    [ [ a = ANTIQUOT "str" -> antiquot "str" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a
      | s = STRING -> Str s ] ]
  ;
  a_CHAR:
    [ [ a = ANTIQUOT "chr" -> antiquot "chr" loc a
      | a = ANTIQUOT "" -> antiquot "" loc a
      | s = CHAR -> Str s ] ]
  ;
  a_TILDEIDENTCOLON:
    [ [ "~"; a = ANTIQUOT ""; ":" -> antiquot "" loc a
      | s = TILDEIDENTCOLON -> Str s ] ]
  ;
  a_TILDEIDENT:
    [ [ "~"; a = ANTIQUOT "" -> antiquot "" loc a
      | s = TILDEIDENT -> Str s ] ]
  ;
  a_QUESTIONIDENTCOLON:
    [ [ "?"; a = ANTIQUOT ""; ":" -> antiquot "" loc a
      | s = QUESTIONIDENTCOLON -> Str s ] ]
  ;
  a_QUESTIONIDENT:
    [ [ "?"; a = ANTIQUOT "" -> antiquot "" loc a
      | s = QUESTIONIDENT -> Str s ] ]
  ;
  rec_flag:
    [ [ a = ANTIQUOT "rec" -> antiquot "rec" loc a ] ]
  ;
  direction_flag:
    [ [ a = ANTIQUOT "to" -> antiquot "to" loc a ] ]
  ;
  mutable_flag:
    [ [ a = ANTIQUOT "mut" -> antiquot "mut" loc a ] ]
  ;
  virtual_flag:
    [ [ a = ANTIQUOT "virt" -> antiquot "virt" loc a ] ]
  ;
  amp_flag:
    [ [ a = ANTIQUOT "opt" -> antiquot "opt" loc a ] ]
  ;
  anti_:
    [ [ a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  anti_list:
    [ [ a = ANTIQUOT "list" -> antiquot "list" loc a ] ]
  ;
  anti_opt:
    [ [ a = ANTIQUOT "opt" -> antiquot "opt" loc a ] ]
  ;
  (* Compatibility old syntax of sequences *)
  expr: LEVEL "top"
    [ [ "do"; seq = SLIST0 [ e = expr; ";" -> e ]; "return"; e = SELF ->
          let _ = warning_seq () in
          Node "ExSeq" [Loc; Append seq e]
      | "for"; i = a_LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; seq = SLIST0 [ e = expr; ";" -> e ]; "done" ->
          let _ = warning_seq () in
          Node "ExFor" [Loc; i; e1; e2; df; seq]
      | "while"; e = SELF; "do"; seq = SLIST0 [ e = expr; ";" -> e ];
        "done" ->
          let _ = warning_seq () in
          Node "ExWhi" [Loc; e; seq] ] ]
  ;
END;

value loc = (0, 0);

value rec expr_of_ast =
  fun
  [ Node n al ->
      List.fold_left (fun e a -> <:expr< $e$ $expr_of_ast a$ >>)
        <:expr< MLast.$uid:n$ >> al
  | List al ->
      List.fold_right (fun a e -> <:expr< [$expr_of_ast a$ :: $e$] >>) al
        <:expr< [] >>
  | Tuple al -> <:expr< ($list:List.map expr_of_ast al$) >>
  | Option None -> <:expr< None >>
  | Option (Some a) -> <:expr< Some $expr_of_ast a$ >>
  | Str s -> <:expr< $str:s$ >>
  | Chr c -> <:expr< $chr:c$ >>
  | Bool True -> <:expr< True >>
  | Bool False -> <:expr< False >>
  | Cons a1 a2 -> <:expr< [$expr_of_ast a1$ :: $expr_of_ast a2$] >>
  | Append a1 a2 -> <:expr< $expr_of_ast a1$ @ [$expr_of_ast a2$] >>
  | Record lal -> <:expr< {$list:List.map label_expr_of_ast lal$} >>
  | Loc -> <:expr< $lid:Stdpp.loc_name.val$ >>
  | Antiquot loc s ->
      let e =
        try Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string s) with
        [ Stdpp.Exc_located (bp, ep) exc ->
            raise (Stdpp.Exc_located (fst loc + bp, fst loc + ep) exc) ]
      in
      <:expr< $anti:e$ >> ]
and label_expr_of_ast (l, a) = (<:patt< MLast.$lid:l$ >>, expr_of_ast a);

value rec patt_of_ast =
  fun
  [ Node n al ->
      List.fold_left (fun e a -> <:patt< $e$ $patt_of_ast a$ >>)
        <:patt< MLast.$uid:n$ >> al
  | List al ->
      List.fold_right (fun a p -> <:patt< [$patt_of_ast a$ :: $p$] >>) al
        <:patt< [] >>
  | Tuple al -> <:patt< ($list:List.map patt_of_ast al$) >>
  | Option None -> <:patt< None >>
  | Option (Some a) -> <:patt< Some $patt_of_ast a$ >>
  | Str s -> <:patt< $str:s$ >>
  | Chr c -> <:patt< $chr:c$ >>
  | Bool True -> <:patt< True >>
  | Bool False -> <:patt< False >>
  | Cons a1 a2 -> <:patt< [$patt_of_ast a1$ :: $patt_of_ast a2$] >>
  | Append _ _ -> failwith "bad pattern"
  | Record lal -> <:patt< {$list:List.map label_patt_of_ast lal$} >>
  | Loc -> <:patt< _ >>
  | Antiquot loc s ->
      let p =
        try Grammar.Entry.parse Pcaml.patt_eoi (Stream.of_string s) with
        [ Stdpp.Exc_located (bp, ep) exc ->
            raise (Stdpp.Exc_located (fst loc + bp, fst loc + ep) exc) ]
      in
      <:patt< $anti:p$ >> ]
and label_patt_of_ast (l, a) = (<:patt< MLast.$lid:l$ >>, patt_of_ast a);

value apply_entry e =
  let f s = Grammar.Entry.parse e (Stream.of_string s) in
  let expr s = expr_of_ast (f s) in
  let patt s = patt_of_ast (f s) in
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
