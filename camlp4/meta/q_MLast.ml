(* camlp4r pa_extend.cmo pa_extend_m.cmo q_MLast.cmo *)
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
value list l = List l;

value sig_item = Grammar.Entry.create gram "signature item";
value str_item = Grammar.Entry.create gram "structure item";
value ctyp = Grammar.Entry.create gram "type";
value patt = Grammar.Entry.create gram "pattern";
value expr = Grammar.Entry.create gram "expression";
value directive = Grammar.Entry.create gram "directive";

value module_type = Grammar.Entry.create gram "module type";
value module_expr = Grammar.Entry.create gram "module expression";

value class_type = Grammar.Entry.create gram "class type";
value class_expr = Grammar.Entry.create gram "class expr";
value class_sig_item = Grammar.Entry.create gram "class signature item";
value class_str_item = Grammar.Entry.create gram "class structure item";

value antiquot k (bp, ep) x =
  let shift =
    if k = "" then String.length "$"
    else String.length "$" + String.length k + String.length ":"
  in
  Antiquot (shift + bp, shift + ep) x
;

value mkumin f arg =
  match arg with
  [ Node "ExInt" [Str n] when int_of_string n > 0 ->
      let n = "-" ^ n in
      Node "ExInt" [Str n]
  | Node "ExFlo" [Str n] when float_of_string n > 0.0 ->
      let n = "-" ^ n in
      Node "ExFlo" [Str n]
  | _ ->
      let f = "~" ^ f in
      Node "ExApp" [Node "ExLid" [Str f]; arg] ]
;

value mklistexp last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some e -> e
        | None -> Node "ExUid" [Str "[]"] ]
    | [e1 :: el] ->
        Node "ExApp"
          [Node "ExApp" [Node "ExUid" [Str "::"]; e1]; loop False el] ]
;

value mklistpat last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some p -> p
        | None -> Node "PaUid" [Str "[]"] ]
    | [p1 :: pl] ->
        Node "PaApp"
          [Node "PaApp" [Node "PaUid" [Str "::"]; p1]; loop False pl] ]
;

value neg s = string_of_int (- int_of_string s);

value not_yet_warned = ref True;
value warning_seq () =
  if not_yet_warned.val then do {
    not_yet_warned.val := False;
    Printf.eprintf "\
*** warning: use of old syntax for sequences in expr quotation\n";
    flush stderr
  }
  else ()
;

EXTEND
  GLOBAL: sig_item str_item ctyp patt expr directive module_type module_expr
    class_type class_expr class_sig_item class_str_item;
  module_expr:
    [ [ "functor"; "("; i = uident; ":"; t = module_type; ")"; "->";
        me = SELF ->
          Node "MeFun" [i; t; me]
      | "struct"; st = SLIST0 [ s = str_item; ";" -> s ]; "end" ->
          Node "MeStr" [st] ]
    | [ me1 = SELF; me2 = SELF -> Node "MeApp" [me1; me2] ]
    | [ me1 = SELF; "."; me2 = SELF -> Node "MeAcc" [me1; me2] ]
    | [ i = UIDENT -> Node "MeUid" [Str i]
      | a = anti_uid -> Node "MeUid" [a]
      | a = anti_ -> a
      | "("; me = SELF; ":"; mt = module_type; ")" -> Node "MeTyc" [me; mt]
      | "("; me = SELF; ")" -> me ] ]
  ;
  str_item:
    [ [ "declare"; st = SLIST0 [ s = str_item; ";" -> s ]; "end" ->
          Node "StDcl" [st]
      | "#"; n = lident; dp = dir_param -> Node "StDir" [n; dp]
      | "exception"; ctl = constructor_declaration; b = rebind_exn ->
          match ctl with
          [ Tuple [Loc; c; tl] -> Node "StExc" [c; tl; b]
          | _ -> match () with [] ]
      | "external"; i = lident; ":"; t = ctyp; "="; p = SLIST1 string ->
          Node "StExt" [i; t; p]
      | "include"; me = module_expr -> Node "StInc" [me]
      | "module"; i = uident; mb = module_binding -> Node "StMod" [i; mb]
      | "module"; "type"; i = uident; "="; mt = module_type ->
          Node "StMty" [i; mt]
      | "open"; m = mod_ident -> Node "StOpn" [m]
      | "type"; l = SLIST1 type_declaration SEP "and" -> Node "StTyp" [l]
      | "value"; r = rec_flag; l = SLIST1 let_binding SEP "and" ->
          Node "StVal" [r; l]
      | a = anti_ -> a
      | e = expr -> Node "StExp" [e]
      | e = anti_exp -> Node "StExp" [e] ] ]
  ;
  rebind_exn:
    [ [ "="; sl = mod_ident -> sl
      | -> List [] ] ]
  ;
  module_binding:
    [ RIGHTA
      [ "("; m = uident; ":"; mt = module_type; ")"; mb = SELF ->
          Node "MeFun" [m; mt; mb]
      | ":"; mt = module_type; "="; me = module_expr -> Node "MeTyc" [me; mt]
      | "="; me = module_expr -> me ] ]
  ;
  module_type:
    [ [ "functor"; "("; i = uident; ":"; t = SELF; ")"; "->"; mt = SELF ->
          Node "MtFun" [i; t; mt] ]
    | [ mt = SELF; "with"; wcl = SLIST1 with_constr SEP "and" ->
          Node "MtWit" [mt; wcl] ]
    | [ "sig"; sg = SLIST0 [ s = sig_item; ";" -> s ]; "end" ->
          Node "MtSig" [sg] ]
    | [ m1 = SELF; m2 = SELF -> Node "MtApp" [m1; m2] ]
    | [ m1 = SELF; "."; m2 = SELF -> Node "MtAcc" [m1; m2] ]
    | [ i = UIDENT -> Node "MtUid" [Str i]
      | i = LIDENT -> Node "MtLid" [Str i]
      | a = anti_uid -> Node "MtUid" [a]
      | a = anti_lid -> Node "MtLid" [a]
      | a = anti_ -> a
      | "("; mt = SELF; ")" -> mt ] ]
  ;
  sig_item:
    [ [ "declare"; st = SLIST0 [ s = sig_item; ";" -> s ]; "end" ->
          Node "SgDcl" [st]
      | "#"; n = lident; dp = dir_param -> Node "SgDir" [n; dp]
      | "exception"; ctl = constructor_declaration ->
          match ctl with
          [ Tuple [Loc; c; tl] -> Node "SgExc" [c; tl]
          | _ -> match () with [] ]
      | "external"; i = lident; ":"; t = ctyp; "="; p = SLIST1 string ->
          Node "SgExt" [i; t; p]
      | "include"; mt = module_type -> Node "SgInc" [mt]
      | "module"; i = uident; mt = module_declaration -> Node "SgMod" [i; mt]
      | "module"; "type"; i = uident; "="; mt = module_type ->
          Node "SgMty" [i; mt]
      | "open"; m = mod_ident -> Node "SgOpn" [m]
      | "type"; l = SLIST1 type_declaration SEP "and" -> Node "SgTyp" [l]
      | "value"; i = lident; ":"; t = ctyp -> Node "SgVal" [i; t]
      | a = anti_ -> a ] ]
  ;
  module_declaration:
    [ RIGHTA
      [ ":"; mt = module_type -> mt
      | "("; i = uident; ":"; t = module_type; ")"; mt = SELF ->
          Node "MtFun" [i; t; mt] ] ]
  ;
  with_constr:
    [ [ "type"; i = mod_ident; tp = SLIST0 type_parameter; "="; t = ctyp ->
          Node "WcTyp" [i; tp; t]
      | "module"; i = mod_ident; "="; mt = module_type ->
          Node "WcMod" [i; mt] ] ]
  ;
  dir_param:
    [ [ a = anti_opt -> a
      | e = expr -> Option (Some e)
      | -> Option None ] ]
  ;
  expr:
    [ RIGHTA
      [ "let"; r = rec_flag; l = SLIST1 let_binding SEP "and"; "in";
        x = SELF ->
          Node "ExLet" [r; l; x]
      | "let"; "module"; m = uident; mb = module_binding; "in"; x = SELF ->
          Node "ExLmd" [m; mb; x]
      | "fun"; "["; l = SLIST0 match_case SEP "|"; "]" -> Node "ExFun" [l]
      | "fun"; p = ipatt; e = fun_def ->
          Node "ExFun" [List [Tuple [p; Option None; e]]]
      | "match"; e = SELF; "with"; "["; l = SLIST0 match_case SEP "|"; "]" ->
          Node "ExMat" [e; l]
      | "match"; x = SELF; "with"; p = ipatt; "->"; e = SELF ->
          Node "ExMat" [x; List [Tuple [p; Option None; e]]]
      | "try"; e = SELF; "with"; "["; l = SLIST0 match_case SEP "|"; "]" ->
          Node "ExTry" [e; l]
      | "try"; x = SELF; "with"; p = ipatt; "->"; e = SELF ->
          Node "ExTry" [x; List [Tuple [p; Option None; e]]]
      | "if"; e1 = SELF; "then"; e2 = SELF; "else"; e3 = SELF ->
          Node "ExIfe" [e1; e2; e3]
      | "do"; "{"; seq = SLIST0 expr SEP ";"; "}" -> Node "ExSeq" [seq]
      | "for"; i = lident; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; "{"; seq = SLIST0 [ e = expr; ";" -> e ]; "}" ->
          Node "ExFor" [i; e1; e2; df; seq]
      | "while"; e = SELF; "do"; "{"; seq = SLIST0 [ e = expr; ";" -> e ];
        "}" ->
          Node "ExWhi" [e; seq] ]
    | NONA
      [ e1 = SELF; ":="; e2 = SELF; dummy -> Node "ExAss" [e1; e2] ]
    | RIGHTA
      [ e1 = SELF; f = "||"; e2 = SELF ->
          Node "ExApp" [Node "ExApp" [Node "ExLid" [Str f]; e1]; e2] ]
    | RIGHTA
      [ e1 = SELF; f = "&&"; e2 = SELF ->
          Node "ExApp" [Node "ExApp" [Node "ExLid" [Str f]; e1]; e2] ]
    | LEFTA
      [ e1 = SELF;
        f =
          [ op = "<" -> op
          | op = ">" -> op
          | op = "<=" -> op
          | op = ">=" -> op
          | op = "=" -> op
          | op = "<>" -> op
          | op = "==" -> op
          | op = "!=" -> op ];
        e2 = SELF ->
          Node "ExApp" [Node "ExApp" [Node "ExLid" [Str f]; e1]; e2] ]
    | RIGHTA
      [ e1 = SELF; f = [ op = "^" -> op | op = "@" -> op ]; e2 = SELF ->
          Node "ExApp" [Node "ExApp" [Node "ExLid" [Str f]; e1]; e2] ]
    | LEFTA
      [ e1 = SELF;
        f =
          [ op = "+" -> op
          | op = "-" -> op
          | op = "+." -> op
          | op = "-." -> op ];
        e2 = SELF ->
          Node "ExApp" [Node "ExApp" [Node "ExLid" [Str f]; e1]; e2] ]
    | LEFTA
      [ e1 = SELF;
        f =
          [ op = "*" -> op
          | op = "/" -> op
          | op = "*." -> op
          | op = "/." -> op
          | op = "land" -> op
          | op = "lor" -> op
          | op = "lxor" -> op
          | op = "mod" -> op ];
        e2 = SELF ->
          Node "ExApp" [Node "ExApp" [Node "ExLid" [Str f]; e1]; e2] ]
    | RIGHTA
      [ e1 = SELF;
        f =
          [ op = "**" -> op
          | op = "asr" -> op
          | op = "lsl" -> op
          | op = "lsr" -> op ];
        e2 = SELF ->
          Node "ExApp" [Node "ExApp" [Node "ExLid" [Str f]; e1]; e2] ]
    | "unary minus" NONA
      [ f = [ op = "-" -> op | op = "-." -> op ]; e = SELF -> mkumin f e ]
    | "apply" LEFTA
      [ e1 = SELF; e2 = SELF -> Node "ExApp" [e1; e2] ]
    | "label" NONA
      [ lab = TILDEIDENTCOLON; e = SELF -> Node "ExLab" [Str lab; e]
      | lab = TILDEIDENT -> Node "ExLab" [Str lab; Node "ExLid" [Str lab]]
      | lab = QUESTIONIDENTCOLON; e = SELF -> Node "ExOlb" [Str lab; e]
      | lab = QUESTIONIDENT -> Node "ExOlb" [Str lab; Node "ExLid" [Str lab]]
      | "~"; a = anti_; ":"; e = SELF -> Node "ExLab" [a; e]
      | "~"; a = anti_ -> Node "ExLab" [a; Node "ExLid" [a]]
      | "?"; a = anti_; ":"; e = SELF -> Node "ExOlb" [a; e]
      | "?"; a = anti_ -> Node "ExOlb" [a; Node "ExLid" [a]] ]
    | "." LEFTA
      [ e1 = SELF; "."; "("; e2 = SELF; ")" -> Node "ExAre" [e1; e2]
      | e1 = SELF; "."; "["; e2 = SELF; "]" -> Node "ExSte" [e1; e2]
      | e1 = SELF; "."; e2 = SELF -> Node "ExAcc" [e1; e2] ]
    | NONA
      [ f = [ op = "~-" -> op | op = "~-." -> op ]; e = SELF ->
          Node "ExApp" [Node "ExLid" [Str f]; e] ]
    | "simple"
      [ s = INT -> Node "ExInt" [Str s]
      | s = FLOAT -> Node "ExFlo" [Str s]
      | s = STRING -> Node "ExStr" [Str s]
      | s = CHAR -> Node "ExChr" [Str s]
      | s = UIDENT -> Node "ExUid" [Str s]
      | s = LIDENT -> Node "ExLid" [Str s]
      | "`"; s = ident -> Node "ExVrn" [s]
      | a = anti_int -> Node "ExInt" [a]
      | a = anti_flo -> Node "ExFlo" [a]
      | a = anti_str -> Node "ExStr" [a]
      | a = anti_chr -> Node "ExChr" [a]
      | a = anti_uid -> Node "ExUid" [a]
      | a = anti_lid -> Node "ExLid" [a]
      | a = anti_anti -> Node "ExAnt" [a]
      | a = anti_ -> a
      | "["; "]" -> Node "ExUid" [Str "[]"]
      | "["; el = LIST1 expr SEP ";"; last = OPT [ "::"; e = expr -> e ];
        "]" ->
          mklistexp last el
      | "[|"; el = SLIST0 expr SEP ";"; "|]" -> Node "ExArr" [el]
      | "{"; lel = SLIST1 label_expr SEP ";"; "}" ->
          Node "ExRec" [lel; Option None]
      | "{"; "("; e = SELF; ")"; "with"; lel = SLIST1 label_expr SEP ";";
        "}" ->
          Node "ExRec" [lel; Option (Some e)]
      | "("; ")" -> Node "ExUid" [Str "()"]
      | "("; e = SELF; ":"; t = ctyp; ")" -> Node "ExTyc" [e; t]
      | "("; e = SELF; ","; el = SLIST1 expr SEP ","; ")" ->
          Node "ExTup" [Cons e el]
      | "("; el = anti_list; ")" -> Node "ExTup" [el]
      | "("; e = SELF; ")" -> e ] ]
  ;
  expr:
    [ [ "do"; seq = SLIST0 [ e = expr; ";" -> e ]; "return"; e = SELF ->
          let _ = warning_seq () in
          Node "ExSeq" [Append seq e]
      | "for"; i = lident; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; seq = SLIST0 [ e = expr; ";" -> e ]; "done" ->
          let _ = warning_seq () in
          Node "ExFor" [i; e1; e2; df; seq]
      | "while"; e = SELF; "do"; seq = SLIST0 [ e = expr; ";" -> e ];
        "done" ->
          let _ = warning_seq () in
          Node "ExWhi" [e; seq] ] ]
  ;
  dummy:
    [ [ -> () ] ]
  ;
  let_binding:
    [ [ p = ipatt; e = fun_binding -> Tuple [p; e] ] ]
  ;
  fun_binding:
    [ RIGHTA
      [ p = ipatt; e = SELF -> Node "ExFun" [List [Tuple [p; Option None; e]]]
      | "="; e = expr -> e
      | ":"; t = ctyp; "="; e = expr -> Node "ExTyc" [e; t] ] ]
  ;
  match_case:
    [ [ p = patt; aso = as_opt; w = when_opt; "->"; e = expr ->
          let p =
            match aso with
            [ Option (Some p2) -> Node "PaAli" [p; p2]
            | Option None -> p
            | _ -> Node "PaAli" [p; aso] ]
          in
          Tuple [p; w; e] ] ]
  ;
  label_expr:
    [ [ i = patt_label_ident; "="; e = expr -> Tuple [i; e] ] ]
  ;
  fun_def:
    [ [ p = ipatt; e = SELF -> Node "ExFun" [List [Tuple [p; Option None; e]]]
      | "->"; e = expr -> e ] ]
  ;
  patt:
    [ [ p1 = SELF; "|"; p2 = SELF -> Node "PaOrp" [p1; p2] ]
    | [ p1 = SELF; ".."; p2 = SELF -> Node "PaRng" [p1; p2] ]
    | [ p1 = SELF; p2 = SELF -> Node "PaApp" [p1; p2] ]
    | [ p1 = SELF; "."; p2 = SELF -> Node "PaAcc" [p1; p2] ]
    | NONA
      [ "~"; i = lident; ":"; p = SELF ->
          Node "PaLab" [i; p]
      | "~"; i = lident ->
          Node "PaLab" [i; Node "PaLid" [i]]
      | "?"; i = lident; ":"; "("; p = patt; e = OPT [ "="; e = expr -> e ];
        ")" ->
          Node "PaOlb" [i; p; Option e]
      | "?"; i = lident; ":"; "("; p = patt; ":"; t = ctyp;
        e = OPT [ "="; e = expr -> e ]; ")" ->
          let p = Node "PaTyc" [p; t] in
          Node "PaOlb" [i; p; Option e]
      | "?"; i = lident ->
          Node "PaOlb" [i; Node "PaLid" [i]; Option None] ]
    | "simple"
      [ v = LIDENT -> Node "PaLid" [Str v]
      | v = UIDENT -> Node "PaUid" [Str v]
      | s = INT -> Node "PaInt" [Str s]
      | "-"; s = INT -> Node "PaInt" [Str (neg s)]
      | s = FLOAT -> Node "PaFlo" [Str s]
      | s = STRING -> Node "PaStr" [Str s]
      | s = CHAR -> Node "PaChr" [Chr s]
      | "`"; s = ident -> Node "PaVrn" [s]
      | "#"; a = anti_list -> Node "PaTyp" [a]
      | "#"; s = mod_ident -> Node "PaTyp" [s]
      | a = anti_lid -> Node "PaLid" [a]
      | a = anti_uid -> Node "PaUid" [a]
      | a = anti_int -> Node "PaInt" [a]
      | a = anti_flo -> Node "PaFlo" [a]
      | a = anti_str -> Node "PaStr" [a]
      | a = anti_chr -> Node "PaChr" [a]
      | a = anti_anti -> Node "PaAnt" [a]
      | a = anti_ -> a
      | "["; "]" -> Node "PaUid" [Str "[]"]
      | "["; pl = LIST1 patt SEP ";"; last = OPT [ "::"; p = patt -> p ];
        "]" ->
          mklistpat last pl
      | "[|"; pl = SLIST0 patt SEP ";"; "|]" -> Node "PaArr" [pl]
      | "{"; lpl = SLIST1 label_patt SEP ";"; "}" -> Node "PaRec" [lpl]
      | "("; ")" -> Node "PaUid" [Str "()"]
      | "("; p = SELF; ")" -> p
      | "("; p = SELF; ":"; t = ctyp; ")" -> Node "PaTyc" [p; t]
      | "("; p = SELF; "as"; p2 = SELF; ")" -> Node "PaAli" [p; p2]
      | "("; p = SELF; ","; pl = SLIST1 patt SEP ","; ")" ->
          Node "PaTup" [Cons p pl]
      | "("; pl = anti_list; ")" -> Node "PaTup" [pl]
      | "_" -> Node "PaAny" [] ] ]
  ;
  label_patt:
    [ [ i = patt_label_ident; "="; p = patt -> Tuple [i; p] ] ]
  ;
  patt_label_ident:
    [ LEFTA
      [ p1 = SELF; "."; p2 = SELF -> Node "PaAcc" [p1; p2] ]
    | RIGHTA
      [ a = anti_ -> a
      | a = anti_lid -> Node "PaLid" [a]
      | a = anti_uid -> Node "PaUid" [a]
      | i = UIDENT -> Node "PaUid" [Str i]
      | i = LIDENT -> Node "PaLid" [Str i] ] ]
  ;
  ipatt:
    [ [ "{"; lpl = SLIST1 label_ipatt SEP ";"; "}" -> Node "PaRec" [lpl]
      | "("; ")" -> Node "PaUid" [Str "()"]
      | "("; p = SELF; ")" -> p
      | "("; p = SELF; ":"; t = ctyp; ")" -> Node "PaTyc" [p; t]
      | "("; p1 = SELF; "as"; p2 = SELF; ")" -> Node "PaAli" [p1; p2]
      | "("; p = SELF; ","; pl = SLIST1 ipatt SEP ","; ")" ->
          Node "PaTup" [Cons p pl]
      | "("; pl = anti_list; ")" -> Node "PaTup" [pl]
      | v = LIDENT -> Node "PaLid" [Str v]
      | a = anti_lid -> Node "PaLid" [a]
      | a = anti_anti -> Node "PaAnt" [a]
      | a = anti_ -> a
      | "_" -> Node "PaAny" [] ] ]
  ;
  label_ipatt:
    [ [ i = patt_label_ident; "="; p = ipatt -> Tuple [i; p] ] ]
  ;
  type_declaration:
    [ [ n = lident; tpl = SLIST0 type_parameter; "="; tk = ctyp;
        cl = SLIST0 constrain ->
          Tuple [n; tpl; tk; cl] ] ]
  ;
  constrain:
    [ [ "constraint"; t1 = ctyp; "="; t2 = ctyp -> Tuple [t1; t2] ] ]
  ;
  type_parameter:
    [ [ "'"; i = ident -> Tuple [i; Tuple [Bool False; Bool False]]
      | "_"; "'"; i = ident -> Tuple [i; Tuple [Bool True; Bool False]]
      | "-"; "'"; i = ident -> Tuple [i; Tuple [Bool False; Bool True]] ] ]
  ;
  ctyp:
    [ LEFTA
      [ t1 = SELF; "=="; t2 = SELF -> Node "TyMan" [t1; t2] ]
    | LEFTA
      [ t1 = SELF; "as"; t2 = SELF -> Node "TyAli" [t1; t2] ]
    | RIGHTA
      [ t1 = SELF; "->"; t2 = SELF -> Node "TyArr" [t1; t2] ]
    | NONA
      [ a = TILDEIDENTCOLON; ":"; t = SELF -> Node "TyLab" [Str a; t]
      | "~"; a = anti_; ":"; t = SELF -> Node "TyLab" [a; t]
      | "?"; a = lident; ":"; t = SELF -> Node "TyOlb" [a; t] ]
    | LEFTA
      [ t1 = SELF; t2 = SELF -> Node "TyApp" [t1; t2] ]
    | LEFTA
      [ t1 = SELF; "."; t2 = SELF -> Node "TyAcc" [t1; t2] ]
    | "simple"
      [ "'"; a = lident -> Node "TyQuo" [a]
      | "_" -> Node "TyAny" []
      | a = LIDENT -> Node "TyLid" [Str a]
      | a = UIDENT -> Node "TyUid" [Str a]
      | a = anti_lid -> Node "TyLid" [a]
      | a = anti_uid -> Node "TyUid" [a]
      | a = anti_ -> a
      | "("; t = SELF; "*"; tl = SLIST1 ctyp SEP "*"; ")" ->
          Node "TyTup" [Cons t tl]
      | "("; tl = anti_list; ")" -> Node "TyTup" [tl]
      | "("; t = SELF; ")" -> t
      | "["; cdl = SLIST0 constructor_declaration SEP "|"; "]" ->
          Node "TySum" [cdl]
      | "{"; ldl = SLIST1 label_declaration SEP ";"; "}" -> Node "TyRec" [ldl]
      | "[|"; rfl = SLIST0 row_field SEP "|"; "|]" ->
          Node "TyVrn" [rfl; Option None]
      | "[|"; ">"; rfl = SLIST1 row_field SEP "|"; "|]" ->
          Node "TyVrn" [rfl; Option (Some (Option None))]
      | "[|"; "<"; rfl = SLIST1 row_field SEP "|"; sl = opt_tag_list; "|]" ->
          Node "TyVrn"
            [rfl; Option (Some (Option (Some sl)))] ] ]
  ;
  row_field:
    [ [ "`"; i = lident -> Tuple [i; Bool True; List []]
      | "`"; i = lident; "of"; oa = OPT "&"; l = SLIST1 ctyp SEP "&" ->
          Tuple [i; Bool (oa <> None); l] ] ]
  ;
  opt_tag_list:
    [ [ ">"; sl = SLIST1 lident -> sl
      | -> List [] ] ]
  ;
  constructor_declaration:
    [ [ ci = uident; "of"; cal = SLIST1 ctyp SEP "and" ->
          Tuple [Loc; ci; cal]
      | ci = uident -> Tuple [Loc; ci; List []] ] ]
  ;
  label_declaration:
    [ [ i = lident; ":"; mf = mutable_flag; t = ctyp ->
          Tuple [Loc; i; mf; t] ] ]
  ;
  ident:
    [ [ i = LIDENT -> Str i
      | i = UIDENT -> Str i
      | a = anti_ -> a ] ]
  ;
  lident:
    [ [ i = LIDENT -> Str i
      | a = anti_ -> a ] ]
  ;
  uident:
    [ [ i = UIDENT -> Str i
      | a = anti_ -> a ] ]
  ;
  mod_ident:
    [ RIGHTA
      [ i = UIDENT -> List [Str i]
      | i = LIDENT -> List [Str i]
      | i = anti_ -> i
      | m = anti_lid -> List [m]
      | m = anti_uid; "."; i = SELF -> Cons m i
      | m = UIDENT; "."; i = SELF -> Cons (Str m) i ] ]
  ;
  direction_flag:
    [ [ "to" -> Bool True
      | "downto" -> Bool False
      | a = anti_to -> a ] ]
  ;
  string:
    [ [ s = STRING -> Str s
      | a = anti_ -> a ] ]
  ;
  rec_flag:
    [ [ a = anti_rec -> a
      | "rec" -> Bool True
      | -> Bool False ] ]
  ;
  as_opt:
    [ [ "as"; p = patt -> Option (Some p)
      | a = anti_as -> a
      | -> Option None ] ]
  ;
  when_opt:
    [ [ "when"; e = expr -> Option (Some e)
      | a = anti_when -> a
      | -> Option None ] ]
  ;
  mutable_flag:
    [ [ a = anti_mut -> a
      | "mutable" -> Bool True
      | -> Bool False ] ]
  ;
  anti_:
    [ [ a = ANTIQUOT -> antiquot "" loc a ] ]
  ;
  anti_anti:
    [ [ a = ANTIQUOT "anti" -> antiquot "anti" loc a ] ]
  ;
  anti_as:
    [ [ a = ANTIQUOT "as" -> antiquot "as" loc a ] ]
  ;
  anti_chr:
    [ [ a = ANTIQUOT "chr" -> antiquot "chr" loc a ] ]
  ;
  anti_exp:
    [ [ a = ANTIQUOT "exp" -> antiquot "exp" loc a ] ]
  ;
  anti_flo:
    [ [ a = ANTIQUOT "flo" -> antiquot "flo" loc a ] ]
  ;
  anti_int:
    [ [ a = ANTIQUOT "int" -> antiquot "int" loc a ] ]
  ;
  anti_lid:
    [ [ a = ANTIQUOT "lid" -> antiquot "lid" loc a ] ]
  ;
  anti_list:
    [ [ a = ANTIQUOT "list" -> antiquot "list" loc a ] ]
  ;
  anti_mut:
    [ [ a = ANTIQUOT "mut" -> antiquot "mut" loc a ] ]
  ;
  anti_opt:
    [ [ a = ANTIQUOT "opt" -> antiquot "mut" loc a ] ]
  ;
  anti_rec:
    [ [ a = ANTIQUOT "rec" -> antiquot "rec" loc a ] ]
  ;
  anti_str:
    [ [ a = ANTIQUOT "str" -> antiquot "str" loc a ] ]
  ;
  anti_to:
    [ [ a = ANTIQUOT "to" -> antiquot "to" loc a ] ]
  ;
  anti_uid:
    [ [ a = ANTIQUOT "uid" -> antiquot "uid" loc a ] ]
  ;
  anti_when:
    [ [ a = ANTIQUOT "when" -> antiquot "when" loc a ] ]
  ;

(* Objects and Classes *)

  str_item:
    [ [ "class"; cd = SLIST1 class_declaration SEP "and" -> Node "StCls" [cd]
      | "class"; "type"; ctd = SLIST1 class_type_declaration SEP "and" ->
          Node "StClt" [ctd] ] ]
  ;
  sig_item:
    [ [ "class"; cd = SLIST1 class_description SEP "and" -> Node "SgCls" [cd]
      | "class"; "type"; ctd = SLIST1 class_type_declaration SEP "and" ->
          Node "SgClt" [ctd] ] ]
  ;

  (* Class expressions *)

  class_declaration:
    [ [ vf = virtual_flag; i = lident; ctp = class_type_parameters;
        cfb = class_fun_binding ->
          Record
            [("ciLoc", Loc); ("ciVir", vf); ("ciPrm", ctp); ("ciNam", i);
             ("ciExp", cfb)] ] ]
  ;
  class_fun_binding:
    [ [ "="; ce = class_expr -> ce
      | ":"; ct = class_type; "="; ce = class_expr -> Node "CeTyc" [ce; ct]
      | p = patt LEVEL "simple"; cfb = SELF -> Node "CeFun" [p; cfb] ] ]
  ;
  class_type_parameters:
    [ [ -> Tuple [Loc; List []]
      | "["; tpl = SLIST1 type_parameter SEP ","; "]" -> Tuple [Loc; tpl] ] ]
  ;
  class_fun_def:
    [ [ p = patt LEVEL "simple"; "->"; ce = class_expr -> Node "CeFun" [p; ce]
      | p = patt LEVEL "simple"; cfd = SELF -> Node "CeFun" [p; cfd] ] ]
  ;
  class_expr:
    [ "top"
      [ "fun"; cfd = class_fun_def -> cfd
      | "let"; rf = rec_flag; lb = SLIST1 let_binding SEP "and"; "in";
        ce = SELF ->
          Node "CeLet" [rf; lb; ce] ]
    | "apply" NONA
      [ ce = SELF; e = expr LEVEL "simple" ->
          Node "CeApp" [ce; e] ]
    | "simple"
      [ a = anti_ -> a
      | ci = class_longident; "["; ctcl = SLIST1 ctyp SEP ","; "]" ->
          Node "CeCon" [ci; ctcl]
      | ci = class_longident -> Node "CeCon" [ci; List []]
      | "object"; csp = class_self_patt_opt; cf = class_structure; "end" ->
          Node "CeStr" [csp; cf]
      | "("; ce = SELF; ":"; ct = class_type; ")" -> Node "CeTyc" [ce; ct]
      | "("; ce = SELF; ")" -> ce ] ]
  ;
  class_structure:
    [ [ cf = SLIST0 [ cf = class_str_item; ";" -> cf ] -> cf ] ]
  ;
  class_self_patt_opt:
    [ [ a = anti_ -> a
      | "("; p = patt; ")" -> Option (Some p)
      | "("; p = patt; ":"; t = ctyp; ")" ->
          Option (Some (Node "PaTyc" [p; t])) ] ]
  ;
  class_str_item:
    [ [ "declare"; st = SLIST0 [ s = class_str_item; ";" -> s ]; "end" ->
          Node "CrDcl" [st]
      | "inherit"; ce = class_expr; pb = as_ident_opt -> Node "CrInh" [ce; pb]
      | "value"; (lab, mf, e) = cvalue -> Node "CrVal" [lab; mf; e]
      | "method"; "virtual"; "private"; l = label; ":"; t = ctyp ->
          Node "CrVir" [l; Bool True; t]
      | "method"; "virtual"; l = label; ":"; t = ctyp ->
          Node "CrVir" [l; Bool False; t]
      | "method"; "private"; l = label; fb = fun_binding ->
          Node "CrMth" [l; Bool True; fb]
      | "method"; l = label; fb = fun_binding ->
          Node "CrMth" [l; Bool False; fb]
      | "type"; t1 = ctyp; "="; t2 = ctyp -> Node "CrCtr" [t1; t2]
      | "initializer"; se = expr -> Node "CrIni" [se] ] ]
  ;
  cvalue:
    [ [ mf = mutable_flag; l = label; "="; e = expr -> (l, mf, e)
      | mf = mutable_flag; l = label; ":"; t = ctyp; "="; e = expr ->
          (l, mf, Node "ExTyc" [e; t])
      | mf = mutable_flag; l = label; ":"; t1 = ctyp; ":>"; t2 = ctyp; "=";
        e = expr ->
          (l, mf, Node "ExCoe" [e; Option (Some t1); t2])
      | mf = mutable_flag; l = label; ":>"; t = ctyp; "="; e = expr ->
          (l, mf, Node "ExCoe" [e; Option None; t]) ] ]
  ;
  label:
    [ [ i = lident -> i ] ]
  ;

  (* Class types *)

  class_type:
    [ [ a = anti_ -> a
      | "["; t = ctyp; "]"; "->"; ct = SELF -> Node "CtFun" [t; ct]
      | id = clty_longident; "["; tl = SLIST1 ctyp SEP ","; "]" ->
          Node "CtCon" [id; tl]
      | id = clty_longident -> Node "CtCon" [id; List []]
      | "object"; cst = class_self_type_opt;
        csf = SLIST0 [ csf = class_sig_item; ";" -> csf ]; "end" ->
          Node "CtSig" [cst; csf] ] ]
  ;
  class_self_type_opt:
    [ [ a = anti_ -> a
      | "("; t = ctyp; ")" -> Option (Some t) ] ]
  ;
  class_sig_item:
    [ [ "declare"; st = SLIST0 [ s = class_sig_item; ";" -> s ]; "end" ->
          Node "CgDcl" [st]
      | "inherit"; cs = class_type -> Node "CgInh" [cs]
      | "value"; mf = mutable_flag; l = label; ":"; t = ctyp ->
          Node "CgVal" [l; mf; t]
      | "method"; "virtual"; "private"; l = label; ":"; t = ctyp ->
          Node "CgVir" [l; Bool True; t]
      | "method"; "virtual"; l = label; ":"; t = ctyp ->
          Node "CgVir" [l; Bool False; t]
      | "method"; "private"; l = label; ":"; t = ctyp ->
          Node "CgMth" [l; Bool True; t]
      | "method"; l = label; ":"; t = ctyp -> Node "CgMth" [l; Bool False; t]
      | "type"; t1 = ctyp; "="; t2 = ctyp -> Node "CgCtr" [t1; t2] ] ]
  ;
  class_description:
    [ [ vf = virtual_flag; n = lident; ctp = class_type_parameters; ":";
        ct = class_type ->
          Record
            [("ciLoc", Loc); ("ciVir", vf); ("ciPrm", ctp); ("ciNam", n);
             ("ciExp", ct)] ] ]
  ;
  class_type_declaration:
    [ [ vf = virtual_flag; n = lident; ctp = class_type_parameters; "=";
        cs = class_type ->
          Record
            [("ciLoc", Loc); ("ciVir", vf); ("ciPrm", ctp); ("ciNam", n);
             ("ciExp", cs)] ] ]
  ;

  (* Expressions *)

  expr: LEVEL "apply"
    [ LEFTA
      [ "new"; i = class_longident -> Node "ExNew" [i] ] ]
  ;
  expr: LEVEL "."
    [ [ e = SELF; "#"; lab = label -> Node "ExSnd" [e; lab] ] ]
  ;
  expr: LEVEL "simple"
    [ [ "("; e = SELF; ":"; t1 = ctyp; ":>"; t2 = ctyp; ")" ->
          Node "ExCoe" [e; Option (Some t1); t2]
      | "("; e = SELF; ":>"; t = ctyp; ")" ->
          Node "ExCoe" [e; Option None; t]
      | "{<"; ">}" -> Node "ExOvr" [List []]
      | "{<"; fel = field_expr_list; ">}" -> Node "ExOvr" [List fel]
      | "{<"; fel = anti_list; ">}" -> Node "ExOvr" [fel] ] ]
  ;
  field_expr_list:
    [ [ l = label; "="; e = expr; ";"; fel = SELF -> [Tuple [l; e] :: fel]
      | l = label; "="; e = expr; ";" -> [Tuple [l; e]]
      | l = label; "="; e = expr -> [Tuple [l; e]] ] ]
  ;

  (* Core types *)

  ctyp: LEVEL "simple"
    [ [ "#"; id = class_longident -> Node "TyCls" [id]
      | "<"; (ml, v) = meth_list; ">" -> Node "TyObj" [ml; v]
      | "<"; ">" -> Node "TyObj" [List []; Bool False] ] ]
  ;
  meth_list:
    [ [ a = anti_list -> (a, Bool False)
      | a = anti_list; b = anti_ -> (a, b)
      | f = field; ";"; (ml, v) = SELF -> (Cons f ml, v)
      | f = field; ";" -> (List [f], Bool False)
      | f = field -> (List [f], Bool False)
      | ".." -> (List [], Bool True) ] ]
  ;
  field:
    [ [ lab = lident; ":"; t = ctyp -> Tuple [lab; t] ] ]
  ;

  (* Identifiers *)

  longid:
    [ [ m = uident; "."; l = SELF -> [m :: l]
      | i = lident -> [i] ] ]
  ;
  clty_longident:
    [ [ l = longid -> List l
      | a = anti_list -> a ] ]
  ;
  class_longident:
    [ [ l = longid -> List l
      | a = anti_list -> a ] ]
  ;
  virtual_flag:
    [ [ a = anti_virt -> a
      | "virtual" -> Bool True
      | -> Bool False ] ]
  ;
  as_ident_opt:
    [ [ "as"; p = lident -> Option (Some p)
      | a = anti_as -> a
      | -> Option None ] ]
  ;
  anti_virt:
    [ [ a = ANTIQUOT "virt" -> antiquot "virt" loc a ] ]
  ;
END;

value loc = (0, 0);

value rec expr_of_ast =
  fun
  [ Node n al ->
      List.fold_left (fun e a -> <:expr< $e$ $expr_of_ast a$ >>)
        <:expr< MLast.$uid:n$ $lid:Stdpp.loc_name.val$ >> al
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
        <:patt< MLast.$uid:n$ _ >> al
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
  | Loc -> <:patt< $lid:Stdpp.loc_name.val$ >>
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

let directive_eoi = Grammar.Entry.create gram "directive" in
do {
  EXTEND
    directive_eoi:
      [ [ x = directive; EOI -> x ] ]
    ;
  END;
  Quotation.add "directive" (apply_entry directive_eoi)
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
