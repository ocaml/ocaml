(* camlp4r pa_extend.cmo q_MLast.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Stdpp;
open Pcaml;

Pcaml.no_constructors_arity.val := False;

do {
  Grammar.Unsafe.reinit_gram gram (Plexer.make ());
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

value not_impl loc s =
  raise_with_loc loc (Stream.Error ("not implemented feature [" ^ s ^ "]"))
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

external loc_of_node : 'a -> (int * int) = "%field0";

value mklistexp loc last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some e -> e
        | None -> <:expr< [] >> ]
    | [e1 :: el] ->
        let loc = if top then loc else (fst (loc_of_node e1), snd loc) in
        <:expr< [$e1$ :: $loop False el$] >> ]
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

value rec separate_fun_val =
  fun
  [ [((_, <:expr< fun [$list:_$] >>) as x) :: l] ->
      let (f, v) = separate_fun_val l in ([x :: f], v)
  | [x :: l] ->
      let (f, v) = separate_fun_val l in (f, [x :: v])
  | [] -> ([], []) ]
;

value extract_label_types loc tn tal cdol =
  let (cdl, aux) =
    List.fold_right
      (fun (c, tl, aux_opt) (cdl, aux) ->
         match aux_opt with
         [ Some anon_record_type ->
             let new_tn = tn ^ "_" ^ c in
             let loc = loc_of_node anon_record_type in
             let aux_def = ((loc, new_tn), [], anon_record_type, []) in
             let tl = [<:ctyp< $lid:new_tn$ >>] in
             ([(loc, c, tl) :: cdl], [aux_def :: aux])
         | None -> ([(loc, c, tl) :: cdl], aux) ])
      cdol ([], [])
  in
  [((loc, tn), tal, <:ctyp< [ $list:cdl$ ] >>, []) :: aux]
;

value special x =
  do {
    assert (String.length x > 0);
    match x.[0] with
    [ '+' | '<' -> True
    | _ -> False ]
  }
;

value idd =
  let p =
    parser
    [ [: `("LIDENT", x) :] -> x
    | [: `("UIDENT", x) :] -> x
    | [: `("", x) when special x :] -> x ]
  in
  Grammar.Entry.of_parser Pcaml.gram "ID" p
;

value uncap s = String.uncapitalize s;

value op = Grammar.Entry.of_parser gram "op" (parser [] : Stream.t 'a -> unit);

EXTEND
  GLOBAL: implem top_phrase use_file sig_item str_item ctyp patt expr
    module_type module_expr;

  implem:
    [ [ x = interdec; EOI -> x ] ]
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

  sdecs: [ [ -> not_impl loc "sdecs" ] ];
  fsigb: [ [ -> not_impl loc "fsigb" ] ];
  fsigconstraint_op: [ [ -> not_impl loc "fsigconstraint_op" ] ];
  fct_exp: [ [ -> not_impl loc "fct_exp" ] ];
  exp_pa: [ [ -> not_impl loc "exp_pa" ] ];
  rvb: [ [ -> not_impl loc "rvb" ] ];
  tyvarseq: [ [ -> not_impl loc "tyvarseq" ] ];
  tyvar_pc: [ [ -> not_impl loc "tyvar_pc" ] ];

  lident_loc:
    [ [ x1 = LIDENT -> (x1, loc) ] ]
  ;
  id:
    [ [ x1 = idd -> x1
      | "*" -> "*" ] ]
  ;
  ident:
    [ [ x1 = idd -> x1
      | "*" -> "*"
      | "=" -> "=" ] ]
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
  tqid:
    [ [ x1 = idd; "."; x2 = tqid -> <:module_type< $uid:x1$ . $x2$ >>
      | x1 = idd -> <:module_type< $uid:x1$ >>
      | x1 = "*" -> <:module_type< $uid:x1$ >>
      | x1 = "=" -> <:module_type< $uid:x1$ >> ] ]
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
      | x1 = idd; "."; x2 = tycon -> <:ctyp< $uid:x1$ . $x2$ >>
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
      | "{"; x1 = LIST1 tlabel SEP ","; "}" -> <:ctyp< {$list:x1$} >>
      | "{"; "}" -> not_impl loc "ty' 3"
      | "("; x1 = ctyp; ","; x2 = LIST1 ctyp SEP ","; ")"; x3 = tycon ->
          not_impl loc "ty' 4"
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
      | x1 = expr; ";"; x2 = expr ->
          <:expr< do { $list:[x1 :: get_seq x2]$ } >> ] ]
  ;
  expr:
    [ [ "if"; x1 = expr; "then"; x2 = expr; "else"; x3 = expr ->
          <:expr< if $x1$ then $x2$ else $x3$ >>
      | "fn"; x1 = LIST1 rule SEP "|" -> <:expr< fun [$list:x1$] >>
      | "case"; x1 = expr; "of"; x2 = LIST1 rule SEP "|" ->
          <:expr< match $x1$ with [$list:x2$] >> ]
    | LEFTA
      [ x1 = expr; "orelse"; x2 = expr -> <:expr< $x1$ || $x2$ >> ]
    | LEFTA
      [ x1 = expr; "andalso"; x2 = expr -> <:expr< $x1$ && $x2$ >> ]
    | LEFTA
      [ x1 = expr; ":"; x2 = ctyp -> <:expr< ($x1$ : $x2$) >> ]
    | "4" NONA
      [ x1 = expr; "<"; x2 = expr -> <:expr< $x1$ < $x2$ >>
      | x1 = expr; ">"; x2 = expr -> <:expr< $x1$ > $x2$ >>
      | x1 = expr; "="; x2 = expr -> <:expr< $x1$ = $x2$ >>
      | x1 = expr; ">="; x2 = expr -> <:expr< $x1$ >= $x2$ >>
      | x1 = expr; "<="; x2 = expr -> <:expr< $x1$ <= $x2$ >> ]
    | "5" RIGHTA
      [ x1 = expr; "::"; x2 = expr -> <:expr< [$x1$ :: $x2$] >> ]
    | "6" LEFTA
      [ x1 = expr; "+"; x2 = expr -> <:expr< $x1$ + $x2$ >>
      | x1 = expr; "-"; x2 = expr -> <:expr< $x1$ - $x2$ >> ]
    | "7" LEFTA
      [ x1 = expr; "*"; x2 = expr -> <:expr< $x1$ * $x2$ >>
      | x1 = expr; "/"; x2 = expr -> <:expr< $x1$ / $x2$ >> ]
    | LEFTA
      [ x1 = expr; x2 = expr -> <:expr< $x1$ $x2$ >> ]
    | [ "!"; x1 = expr -> <:expr< $x1$ . val >> ]
    | [ "~"; x1 = expr -> <:expr< - $x1$ >> ]
    | [ "#"; x1 = selector; x2 = expr -> <:expr< $x2$ . $lid:x1$ >> ]
    | [ x1 = LIDENT ->
          match x1 with
          [ "true" | "false" -> <:expr< $uid:x1$ >>
          | _ -> <:expr< $lid:x1$ >> ]
      | x1 = UIDENT -> <:expr< $uid:x1$ >>
      | x1 = UIDENT; "."; x2 = eqid -> <:expr< $uid:x1$ . $x2$ >>
      | x1 = INT -> <:expr< $int:x1$ >>
      | x1 = FLOAT -> <:expr< $flo:x1$ >>
      | x1 = STRING -> <:expr< $str:x1$ >>
      | "let"; x1 = ldecs; "in"; x2 = exp_ps; "end" ->
          let (let_fun, let_val) = separate_fun_val x1 in
          let x2 =
            List.fold_right (fun (p, e) x2 -> <:expr< let $p$ = $e$ in $x2$ >>)
              let_val x2
          in
          let x2 =
            match let_fun with
            [ [] -> x2
            | _ -> <:expr< let rec $list:let_fun$ in $x2$ >> ]
          in
          x2
      | "{"; x1 = LIST1 elabel SEP ","; "}" -> <:expr< {$list:x1$} >>
      | "["; "]" -> <:expr< [] >>
      | "["; x1 = expr; "]" -> <:expr< [$x1$] >>
      | "["; x1 = expr; ","; x2 = LIST1 SELF SEP ","; "]" ->
          mklistexp loc None [x1 :: x2]
      | "("; ")" -> <:expr< () >>
      | "("; x1 = expr; ","; x2 = LIST1 SELF SEP ","; ")" ->
          <:expr< ($list:[x1::x2]$) >>
      | "("; x1 = expr; ")" -> x1 ] ]
  ;
  fixity:
    [ [ "infix" -> not_impl loc "fixity 1"
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
    | [ x1 = patt; x2 = patt -> <:patt< $x1$ $x2$ >> ]
    | "apat"
      [ x1 = INT -> <:patt< $int:x1$ >>
      | x1 = UIDENT -> <:patt< $uid:x1$ >>
      | x1 = id -> <:patt< $lid:x1$ >>
      | "_" -> <:patt< _ >>
      | "["; "]" -> <:patt< [] >>
      | "["; x1 = patt; "]" -> <:patt< [$x1$] >>
      | "("; ")" -> <:patt< () >>
      | "("; x1 = patt; ","; x2 = LIST1 SELF SEP ","; ")" ->
          <:patt< ($list:[x1::x2]$) >>
      | "("; x1 = patt; ")" -> x1 ] ]
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
    [ [ xl = LIST1 clause SEP "|" ->
          let (fname, l) =
            List.fold_left
              (fun (fname, l) ((x1, loc), x2, x3, x4) ->
                 let fname =
                   match fname with
                   [ Some fname ->
                       if x1 <> fname then
                         raise_with_loc loc
                           (Stream.Error ("'" ^ fname ^ "' expected"))
                       else Some fname
                   | _ -> Some x1 ]
                 in
                 let x4 =
                   match x3 with
                   [ Some t -> <:expr< ($x4$ : $t$) >>
                   | _ -> x4 ]
                 in
                 let l = [(x2, None, x4) :: l] in
                 (fname, l))
              (None, []) xl
          in
          match fname with
          [ Some fname ->
              (<:patt< $lid:fname$ >>, <:expr< fun [ $list:List.rev l$ ] >>)
          | None -> assert False ]
      | "lazy"; x1 = LIST1 clause SEP "|" -> not_impl loc "fb 2" ] ]
  ;
  clause:
    [ [ x1 = lident_loc; x2 = patt LEVEL "apat"; x3 = patt LEVEL "apat";
        x4 = constrain; "="; x5 = expr ->
          let x2 =
            match x2 with
            [ <:patt< $lid:s$ >> -> s
            | _ -> raise (Stream.Error "bad clause") ]
          in
          ((x2, loc), <:patt< ($lid:fst x1$, $x3$) >>, x4, x5)
      | x1 = lident_loc; x2 = patt LEVEL "apat"; x3 = constrain; "=";
        x4 = expr ->
          (x1, x2, x3, x4)
      | x1 = patt LEVEL "apat"; x2 = idd_loc; x3 = patt LEVEL "apat";
        x4 = constrain; "="; x5 = expr ->
          (x2, <:patt< ($x1$, $x3$) >>, x4, x5) ] ]
  ;
  idd_loc:
    [ [ x1 = idd -> (x1, loc) ] ]
  ;
  tb:
    [ [ x1 = tyvars; x2 = idd; "="; x3 = ctyp ->
          ((loc, uncap x2), x1, x3, []) ] ]
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
    [ [ x1 = op_op; x2 = ident -> (x2, [], None)
      | x1 = op_op; x2 = ident; "of"; x3 = ctyp ->
          match x3 with
          [ <:ctyp< {$list:_$} >> -> (x2, [], Some x3)
          | _ -> (x2, [x3], None) ] ] ]
  ;
  eb:
    [ [ x1 = op_op; x2 = ident -> (x2, [])
      | x1 = op_op; x2 = ident; "of"; x3 = ctyp -> (x2, [x3]) ] ]
  ;
  ldec1:
    [ [ "val"; x1 = LIST1 vb SEP "and" -> x1
      | "fun"; x1 = LIST1 fb SEP "and" -> x1 ] ]
  ;
  ldecs:
    [ [ -> []
      | x1 = ldec1; x2 = ldecs -> x1 @ x2
      | ";"; x1 = ldecs -> x1
      | "local"; x1 = ldecs; "in"; x2 = ldecs; "end"; x3 = ldecs ->
          not_impl loc "ldecs 4" ] ]
  ;
  spec_s:
    [ [ -> []
      | x1 = sig_item; x2 = spec_s -> [x1 :: x2]
      | ";"; x1 = spec_s -> x1 ] ]
  ;
  sig_item:
   [ [ "structure"; x1 = LIST1 strspec SEP "and" -> sig_declare loc x1
     | "functor"; x1 = LIST1 fctspec SEP "and" -> sig_declare loc x1
     | "datatype"; x1 = db -> <:sig_item< type $list:x1$ >>
     | "type"; x1 = LIST1 tyspec SEP "and" -> <:sig_item< type $list:x1$ >>
     | "val"; x1 = LIST1 valspec SEP "and" ->  sig_declare loc x1
     | "exception"; x1 = LIST1 exnspec SEP "and" -> sig_declare loc x1
     | "sharing"; x1 = LIST1 sharespec SEP "and" -> not_impl loc "sig_item 5"
     | "include"; x1 = module_type -> <:sig_item< include $x1$ >> ] ]
  ;
  strspec:
    [ [ x1 = ident; ":"; x2 = module_type ->
          <:sig_item< module $x1$ : $x2$ >> ] ]
  ;
  fctspec:
    [ [ x1 = ident; x2 = fsig -> <:sig_item< module $x1$ : $x2$ >> ] ]
  ;
  tyspec:
    [ [ x1 = tyvars; x2 = idd ->
          ((loc, uncap x2), x1, <:ctyp< '$choose_tvar x1$ >>, [])
      | x1 = tyvars; x2 = idd; "="; x3 = ctyp -> not_impl loc "tyspec 2" ] ]
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
    [ [ "type"; x1 = patheqn -> not_impl loc "sharespec 1"
      | x1 = patheqn -> not_impl loc "sharespec 2" ] ]
  ;
  patheqn:
    [ [ x1 = qid; "="; x2 = qid -> [x1; x2]
      | x1 = qid; "="; x2 = patheqn -> [x1 :: x2] ] ]
  ;
  whspec:
    [ [ "type"; x1 = tyvars; x2 = sqid; "="; x3 = ctyp ->
          MLast.WcTyp loc x2 x1 x3
      | x1 = sqid; "="; x2 = tqid -> MLast.WcMod loc x1 x2 ] ]
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
      | x1 = qid; x2 = arg_fct -> <:module_expr< $x1$ $x2$ >>
      | "let"; x1 = strdecs; "in"; x2 = module_expr; "end" ->
          not_impl loc "str 4"	
      | x1 = module_expr; ":"; x2 = module_type -> not_impl loc "str 5"
      | x1 = module_expr; x2 = ":>"; x3 = module_type ->
          not_impl loc "str 6" ] ]
  ;
  arg_fct:
    [ [ "("; x1 = strdecs; ")"; x2 = arg_fct -> not_impl loc "arg_fct 1"
      | "("; x1 = module_expr; ")"; x2 = arg_fct -> not_impl loc "arg_fct 2"
      | "("; x1 = module_expr; ")" -> x1
      | "("; x2 = strdecs; ")" -> not_impl loc "arg_fct 4" ] ]
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
          not_impl loc "sdec 5" ]
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
              (fun (s, tl) -> <:str_item< exception $s$ of $list:tl$ >>) x1
          in
          str_declare loc dl
      | "open"; x1 = LIST1 sqid ->
          let dl = List.map (fun sl -> <:str_item< open $sl$ >>) x1 in
          str_declare loc dl
      | x1 = fixity; x2 = idd ->
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
                  [x2];
                  str_declare loc []
              }
          | _ -> not_impl loc "ldec 14" ]
      | "overload"; x1 = ident; ":"; x2 = ctyp; "as"; x3 = exp_pa ->
          not_impl loc "ldec 15" ] ]
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
    [ [ x1 = idd; ":"; x2 = module_type -> (x1, x2)
      | x1 = spec_s ->
          match x1 with
          [ [<:sig_item< module $x1$ : $x2$ >>] -> (x1, x2)
          | _ -> not_impl loc "fparam 2" ] ] ]
  ;
  fparamList:
    [ [ "("; x1 = fparam; ")" -> [x1]
      | "("; x1 = fparam; ")"; x2 = fparamList -> [x1 :: x2] ] ]
  ;
  fctb:
    [ [ x1 = ident; x2 = fparamList; x3 = sigconstraint_op; "=";
        x4 = module_expr ->
          let x4 =
            match x3 with
            [ Some x3 -> <:module_expr< ($x4$ : $x3$) >>
            | None -> x4 ]
          in
          let x4 =
            List.fold_right
              (fun (i, s) x4 -> <:module_expr< functor ($i$ : $s$) -> $x4$ >>)
              x2 x4
          in
          <:str_item< module $x1$ = $x4$ >>
      | x1 = ident; x2 = fsigconstraint_op; "="; x3 = fct_exp ->
          not_impl loc "fctb 2" ] ]
  ;
  interdec:
    [ [ x = LIST1 [ s = str_item -> (s, loc) ] -> (x, False)
      | x = expr -> not_impl loc "interdec 2" ] ]
  ;
END;
