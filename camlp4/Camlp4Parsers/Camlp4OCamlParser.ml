open Camlp4;                                        (* -*- camlp4r -*- *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)


module Id : Sig.Id = struct
  value name = "Camlp4OCamlParser";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  Camlp4_config.constructors_arity.val := False;

  (*FIXME remove this and use OCaml ones *)
  value bigarray_set _loc var newval =
    match var with
    [ <:expr< Bigarray.Array1.get $arr$ $c1$ >> ->
        Some <:expr< Bigarray.Array1.set $arr$ $c1$ $newval$ >>
    | <:expr< Bigarray.Array2.get $arr$ $c1$ $c2$ >> ->
        Some <:expr< Bigarray.Array2.set $arr$ $c1$ $c2$ $newval$ >>
    | <:expr< Bigarray.Array3.get $arr$ $c1$ $c2$ $c3$ >> ->
        Some <:expr< Bigarray.Array3.set $arr$ $c1$ $c2$ $c3$ $newval$ >>
    | <:expr< Bigarray.Genarray.get $arr$ [| $coords$ |] >> ->
        Some <:expr< Bigarray.Genarray.set $arr$ [| $coords$ |] $newval$ >>
    | _ -> None ];
  value mk_anti ?(c = "") n s = "\\$"^n^c^":"^s;
  (*FIXME*)

  value conc_seq e1 e2 =
    match (e1, e2) with
    [ (<:expr@_loc< do { $e1$ } >>, <:expr< do { $e2$ } >>) ->
        <:expr< do { $e1$; $e2$ } >>
    | (<:expr@_loc< do { $e1$ } >>, _) ->
        <:expr< do { $e1$; $e2$ } >>
    | (_, <:expr@_loc< do { $e2$ } >>) ->
        <:expr< do { $e1$; $e2$ } >>
    | _ ->
        let _loc =
          Loc.merge (Ast.loc_of_expr e1)
                    (Ast.loc_of_expr e2) in
        <:expr< do { $e1$; $e2$ } >> ];

  value test_constr_decl =
    Gram.Entry.of_parser "test_constr_decl"
      (fun strm ->
        match Stream.npeek 1 strm with
        [ [(UIDENT _, _)] ->
            match Stream.npeek 2 strm with
            [ [_; (KEYWORD ".", _)] -> raise Stream.Failure
            | [_; (KEYWORD "(", _)] -> raise Stream.Failure
            | [_ :: _] -> ()
            | _ -> raise Stream.Failure ]
        | [(KEYWORD "|", _)] -> ()
        | _ -> raise Stream.Failure ])
  ;

  value stream_peek_nth n strm =
    loop n (Stream.npeek n strm) where rec loop n =
      fun
      [ [] -> None
      | [(x, _)] -> if n == 1 then Some x else None
      | [_ :: l] -> loop (n - 1) l ]
  ;

  (* horrible hacks to be able to parse class_types *)

  value test_ctyp_minusgreater =
    Gram.Entry.of_parser "test_ctyp_minusgreater"
      (fun strm ->
        let rec skip_simple_ctyp n =
          match stream_peek_nth n strm with
          [ Some (KEYWORD "->") -> n
          | Some (KEYWORD ("[" | "[<")) ->
              skip_simple_ctyp (ignore_upto "]" (n + 1) + 1)
          | Some (KEYWORD "(") -> skip_simple_ctyp (ignore_upto ")" (n + 1) + 1)
          | Some
              (KEYWORD
                ("as" | "'" | ":" | "*" | "." | "#" | "<" | ">" | ".." | ";" |
                "_" | "?")) ->
              skip_simple_ctyp (n + 1)
          | Some (LIDENT _ | UIDENT _) ->
              skip_simple_ctyp (n + 1)
          | Some _ | None -> raise Stream.Failure ]
        and ignore_upto end_kwd n =
          match stream_peek_nth n strm with
          [ Some (KEYWORD prm) when prm = end_kwd -> n
          | Some (KEYWORD ("[" | "[<")) ->
              ignore_upto end_kwd (ignore_upto "]" (n + 1) + 1)
          | Some (KEYWORD "(") -> ignore_upto end_kwd (ignore_upto ")" (n + 1) + 1)
          | Some _ -> ignore_upto end_kwd (n + 1)
          | None -> raise Stream.Failure ]
        in
        match Stream.peek strm with
        [ Some ((KEYWORD "[" | LIDENT _ | UIDENT _), _) -> skip_simple_ctyp 1
        | Some (KEYWORD "object", _) -> raise Stream.Failure
        | _ -> 1 ])
  ;

  value test_label_eq =
    Gram.Entry.of_parser "test_label_eq"
      (test 1 where rec test lev strm =
        match stream_peek_nth lev strm with
        [ Some (UIDENT _ | LIDENT _ | KEYWORD ".") ->
            test (lev + 1) strm
        | Some (KEYWORD "=") -> ()
        | _ -> raise Stream.Failure ])
  ;

  value test_typevar_list_dot =
    Gram.Entry.of_parser "test_typevar_list_dot"
      (let rec test lev strm =
        match stream_peek_nth lev strm with
        [ Some (KEYWORD "'") -> test2 (lev + 1) strm
        | Some (KEYWORD ".") -> ()
        | _ -> raise Stream.Failure ]
      and test2 lev strm =
        match stream_peek_nth lev strm with
        [ Some (UIDENT _ | LIDENT _) -> test (lev + 1) strm
        | _ -> raise Stream.Failure ]
      in
      test 1)
  ;

  value lident_colon =
    Gram.Entry.of_parser "lident_colon"
      (fun strm ->
        match Stream.npeek 2 strm with
        [ [(LIDENT i, _); (KEYWORD ":", _)] ->
            do { Stream.junk strm; Stream.junk strm; i }
        | _ -> raise Stream.Failure ])
  ;

  value rec is_ident_constr_call =
    fun
    [ <:ident< $uid:_$ >> -> True
    | <:ident< $_$.$i$ >> -> is_ident_constr_call i
    | _ -> False ];

  value rec is_expr_constr_call =
    fun
    [ <:expr< $id:i$ >> -> is_ident_constr_call i
    | <:expr< `$_$ >> -> True
    | <:expr< $_$.$e$ >> -> is_expr_constr_call e
    | <:expr@_loc< $e$ $_$ >> ->
        let res = is_expr_constr_call e in
        if (not Camlp4_config.constructors_arity.val) && res then
          Loc.raise _loc (Stream.Error "currified constructor")
        else res
    | _ -> False ];

  DELETE_RULE Gram expr: SELF; "where"; opt_rec; let_binding END;
  DELETE_RULE Gram value_let: "value" END;
  DELETE_RULE Gram value_val: "value" END;
  DELETE_RULE Gram str_item: value_let; opt_rec; binding END;
  DELETE_RULE Gram module_type: "'"; a_ident END;
  DELETE_RULE Gram module_type: SELF; SELF; dummy END;
  DELETE_RULE Gram module_type: SELF; "."; SELF END;
  DELETE_RULE Gram label_expr: label_longident; fun_binding END;
  DELETE_RULE Gram meth_list: meth_decl; opt_dot_dot END;
  DELETE_RULE Gram expr: "let"; opt_rec; binding; "in"; SELF END;
  DELETE_RULE Gram expr: "let"; "module"; a_UIDENT; module_binding0; "in"; SELF END;
  DELETE_RULE Gram expr: "fun"; "["; LIST0 match_case0 SEP "|"; "]" END;
  DELETE_RULE Gram expr: "if"; SELF; "then"; SELF; "else"; SELF END;
  DELETE_RULE Gram expr: "do"; do_sequence END;
  DELETE_RULE Gram expr: SELF; SELF END;
  DELETE_RULE Gram expr: "new"; class_longident END;
  DELETE_RULE Gram expr: "["; sem_expr_for_list; "::"; expr; "]" END;
  DELETE_RULE Gram expr: "{"; label_expr_list; "}" END;
  DELETE_RULE Gram expr: "{"; "("; SELF; ")"; "with"; label_expr_list; "}" END;
  DELETE_RULE Gram expr: "("; SELF; ","; comma_expr; ")" END;
  DELETE_RULE Gram expr: SELF; ":="; SELF; dummy END;
  DELETE_RULE Gram expr: "~"; a_LIDENT; ":"; SELF END;
  DELETE_RULE Gram expr: "?"; a_LIDENT; ":"; SELF END;
  (* Some other DELETE_RULE are after the grammar *)

  value clear = Gram.Entry.clear;
  clear ctyp;
  clear patt;
  clear a_UIDENT;
  clear type_longident_and_parameters;
  clear type_parameters;
  clear ipatt;
  clear labeled_ipatt;
  clear semi;
  clear do_sequence;
  clear type_kind;
  clear constructor_arg_list;
  clear poly_type;
  clear class_name_and_param;
  clear class_longident_and_param;
  clear class_type_longident_and_param;
  clear class_type_plus;
  clear type_constraint;
  clear comma_patt;
  clear sequence;
  clear sem_expr_for_list;
  clear sem_expr;
  clear label_declaration;
  clear star_ctyp;
  clear match_case;
  clear with_constr;
  clear top_phrase;

  EXTEND Gram
    GLOBAL:
      a_CHAR a_FLOAT a_INT a_INT32 a_INT64 a_LABEL a_LIDENT
      a_NATIVEINT a_OPTLABEL a_STRING a_UIDENT a_ident
      amp_ctyp and_ctyp match_case match_case0 match_case_quot binding binding_quot
      class_declaration class_description class_expr class_expr_quot
      class_fun_binding class_fun_def class_info_for_class_expr
      class_info_for_class_type class_longident class_longident_and_param
      class_name_and_param class_sig_item class_sig_item_quot class_signature
      class_str_item class_str_item_quot class_structure class_type
      class_type_declaration class_type_longident
      class_type_longident_and_param class_type_plus class_type_quot
      comma_ctyp comma_expr comma_ipatt comma_patt comma_type_parameter
      constrain constructor_arg_list constructor_declaration
      constructor_declarations ctyp ctyp_quot cvalue_binding direction_flag
      dummy eq_expr expr expr_eoi expr_quot fun_binding
      fun_def ident ident_quot implem interf ipatt ipatt_tcon label
      label_declaration label_declaration_list label_expr label_expr_list
      label_longident label_patt_list meth_list
      labeled_ipatt let_binding module_binding module_binding0
      module_binding_quot module_declaration module_expr module_expr_quot
      module_longident module_longident_with_app module_rec_declaration
      module_type module_type_quot more_ctyp name_tags opt_as_lident
      opt_class_self_patt opt_class_self_type
      opt_comma_ctyp opt_dot_dot opt_eq_ctyp opt_expr
      opt_meth_list opt_mutable opt_polyt opt_private opt_rec
      opt_virtual opt_when_expr patt patt_as_patt_opt patt_eoi
      patt_quot patt_tcon phrase poly_type row_field
      sem_expr sem_expr_for_list sem_patt sem_patt_for_list semi sequence
      sig_item sig_item_quot sig_items star_ctyp str_item str_item_quot
      str_items top_phrase type_constraint type_declaration
      type_ident_and_parameters type_kind type_longident
      type_longident_and_parameters type_parameter type_parameters typevars
      use_file val_longident value_let value_val with_constr with_constr_quot
      infixop0 infixop1 infixop2 infixop3 infixop4 do_sequence
    ;
    sem_expr:
      [ [ e1 = expr LEVEL "top"; ";"; e2 = SELF -> <:expr< $e1$; $e2$ >>
        | e = expr LEVEL "top"; ";" -> e
        | e = expr LEVEL "top" -> e ] ]
    ;
    sequence:
      [ [ e = sem_expr -> e ] ]
    ;
    do_sequence:
      [ [ seq = sequence; "done" -> seq
      ] ]
    ;
    sem_expr_for_list:
      [ [ e = expr LEVEL "top"; ";"; el = SELF -> fun acc -> <:expr< [ $e$ :: $el acc$ ] >>
        | e = expr LEVEL "top"; ";" -> fun acc -> <:expr< [ $e$ :: $acc$ ] >>
        | e = expr LEVEL "top" -> fun acc -> <:expr< [ $e$ :: $acc$ ] >>
      ] ]
    ;
    str_item:
      [ "top"
          [ "let"; r = opt_rec; bi = binding; "in"; x = expr ->
              <:str_item< let $rec:r$ $bi$ in $x$ >>
          | "let"; r = opt_rec; bi = binding ->
              match bi with
              [ <:binding< _ = $e$ >> -> <:str_item< $exp:e$ >>
              | _ -> <:str_item< value $rec:r$ $bi$ >> ]
          | "let"; "module"; m = a_UIDENT; mb = module_binding0; "in"; e = expr ->
              <:str_item< let module $m$ = $mb$ in $e$ >>
      ] ]
    ;
    seq_expr:
      [ [ e1 = expr LEVEL "top"; ";"; e2 = SELF ->
            conc_seq e1 e2
        | e1 = expr LEVEL "top"; ";" -> e1
        | e1 = expr LEVEL "top" -> e1 ] ];
    expr: BEFORE "top"
      [ ";" [ e = seq_expr -> e ] ];
    expr: LEVEL "top"
      [ [ "let"; r = opt_rec; bi = binding; "in";
          x = expr LEVEL ";" ->
            <:expr< let $rec:r$ $bi$ in $x$ >>
        | "let"; "module"; m = a_UIDENT; mb = module_binding0; "in";
          e = expr LEVEL ";" ->
            <:expr< let module $m$ = $mb$ in $e$ >>
        | "function"; a = match_case ->
            <:expr< fun [ $a$ ] >>
        | "if"; e1 = SELF; "then"; e2 = expr LEVEL "top";
          "else"; e3 = expr LEVEL "top" ->
            <:expr< if $e1$ then $e2$ else $e3$ >>
        | "if"; e1 = SELF; "then"; e2 = expr LEVEL "top" ->
            <:expr< if $e1$ then $e2$ else () >>
      ] ];
    expr: BEFORE "||"
      [ ","
        [ e1 = SELF; ","; e2 = comma_expr ->
            <:expr< ( $e1$, $e2$ ) >> ]
      | ":=" NONA
        [ e1 = SELF; ":="; e2 = expr LEVEL "top" ->
            <:expr< $e1$.val := $e2$ >>
        | e1 = SELF; "<-"; e2 = expr LEVEL "top" ->
            match bigarray_set _loc e1 e2 with
            [ Some e -> e
            | None -> <:expr< $e1$ := $e2$ >> ]
      ] ];
    expr: AFTER "^"
      [ "::" RIGHTA
        [ e1 = SELF; "::"; e2 = SELF -> <:expr< [$e1$ :: $e2$] >> ]
      ];
    expr: LEVEL "apply" (* LEFTA *)
      [ [ e1 = SELF; e2 = SELF ->
            match (is_expr_constr_call e1, e2) with
            [ (True, <:expr< ( $tup:e$ ) >>) ->
                List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>) e1
                                (Ast.list_of_expr e [])
            | _ -> <:expr< $e1$ $e2$ >> ]
      ] ];
    expr: LEVEL "simple" (* LEFTA *)
      [ [ "false" -> <:expr< False >>
        | "true" -> <:expr< True >>
        | "{"; test_label_eq; lel = label_expr_list; "}" ->
            <:expr< { $lel$ } >>
        | "{"; e = expr LEVEL "."; "with"; lel = label_expr_list; "}" ->
            <:expr< { ($e$) with $lel$ } >>
        | "new"; i = class_longident -> <:expr< new $i$ >>
      ] ]
    ;
    val_longident:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $anti:mk_anti ~c:"ident" n s$ >>
        | i = a_UIDENT -> <:ident< $uid:i$ >>
        | i = a_LIDENT -> <:ident< $lid:i$ >>
        | `ANTIQUOT (""|"id"|"anti"|"list" as n) s; "."; i = SELF ->
            <:ident< $anti:mk_anti ~c:"ident" n s$.$i$ >>
        | i = a_UIDENT; "."; j = SELF -> <:ident< $uid:i$.$j$ >> ] ]
    ;
    match_case:
      [ [ OPT "|"; l = LIST1 match_case0 SEP "|" -> Ast.mcOr_of_list l ] ]
    ;
    patt_constr:
      [ [ i = module_longident -> <:patt< $id:i$ >>
        | "`"; s = a_ident -> <:patt< `$s$ >> ] ]
    ;
    (* Patterns *)
    patt:
      [ "as" LEFTA
        [ p1 = SELF; "as"; i = a_LIDENT -> <:patt< ($p1$ as $lid:i$) >> ]
      | "|" LEFTA
        [ p1 = SELF; "|"; p2 = SELF -> <:patt< $p1$ | $p2$ >> ]
      | ","
        [ p = SELF; ","; pl = (*FIXME comma_patt*) LIST1 NEXT SEP "," ->
            <:patt< ( $p$, $Ast.paCom_of_list pl$ ) >> ]
      | "::" RIGHTA
        [ p1 = SELF; "::"; p2 = SELF -> <:patt< [$p1$ :: $p2$] >> ]
      | "apply" RIGHTA
        [ p1 = patt_constr; p2 = SELF ->
            match p2 with
            [ <:patt< ( $tup:p$ ) >> ->
                List.fold_left (fun p1 p2 -> <:patt< $p1$ $p2$ >>) p1
                                (Ast.list_of_patt p [])
            | _ -> <:patt< $p1$ $p2$ >> ]
        | "lazy"; p = SELF -> <:patt< lazy $p$ >>
        | `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $anti:mk_anti ~c:"patt" n s$ >>
        | p = patt_constr -> p ]
      | "simple"
        [ `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $anti:mk_anti ~c:"patt" n s$ >>
        | `ANTIQUOT ("tup" as n) s -> <:patt< ($tup:<:patt< $anti:mk_anti ~c:"patt" n s$ >>$) >>
        | `ANTIQUOT ("`bool" as n) s -> <:patt< $anti:mk_anti n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.patt_tag
        | i = ident -> <:patt< $id:i$ >>
        | s = a_INT -> <:patt< $int:s$ >>
        | s = a_INT32 -> <:patt< $int32:s$ >>
        | s = a_INT64 -> <:patt< $int64:s$ >>
        | s = a_NATIVEINT -> <:patt< $nativeint:s$ >>
        | "-"; s = a_INT -> <:patt< $int:"-" ^ s$ >>
        | "-"; s = a_INT32 -> <:patt< $int32:"-" ^ s$ >>
        | "-"; s = a_INT64 -> <:patt< $int64:"-" ^ s$ >>
        | "-"; s = a_NATIVEINT -> <:patt< $nativeint:"-" ^ s$ >>
        | "-"; s = a_FLOAT -> <:patt< $flo:"-" ^ s$ >>
        | s = a_FLOAT -> <:patt< $flo:s$ >>
        | s = a_STRING -> <:patt< $str:s$ >>
        | s1 = a_CHAR; ".."; s2 = a_CHAR -> <:patt< $chr:s1$ .. $chr:s2$ >>
        | s = a_CHAR -> <:patt< $chr:s$ >>
        | "false" -> <:patt< False >>
        | "true" -> <:patt< True >>
        | "["; "]" -> <:patt< [] >>
        | "["; mk_list = sem_patt_for_list; "::"; last = patt; "]" ->
            mk_list last
        | "["; mk_list = sem_patt_for_list; "]" ->
            mk_list <:patt< [] >>
        | "[|"; "|]" -> <:patt< [||] >>
        | "[|"; pl = sem_patt; "|]" -> <:patt< [| $pl$ |] >>
        | "{"; pl = label_patt_list; "}" -> <:patt< { $pl$ } >>
        | "("; ")" -> <:patt< () >>
        | "("; p = patt; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
        | "("; p = patt; ")" -> <:patt< $p$ >>
        | "_" -> <:patt< _ >>
        | "`"; s = a_ident -> <:patt< ` $s$ >>
        | "#"; i = type_longident -> <:patt< # $i$ >> ] ]
    ;
    comma_expr:
      [ [ e1 = expr LEVEL ":="; ","; e2 = SELF -> <:expr< $e1$, $e2$ >>
        | e1 = expr LEVEL ":=" -> e1 ] ]
    ;
    (* comma_patt:
      [ [ p1 = SELF; ","; p2 = SELF -> <:patt< $p1$, $p2$ >>
        | p = patt LEVEL ".." -> p ] ]
    ;                                                           *)
    type_constraint:
      [ [ "constraint" -> () ] ]
    ;
    with_constr:
      [ LEFTA
        [ wc1 = SELF; "and"; wc2 = SELF -> <:with_constr< $wc1$ and $wc2$ >>
        | `ANTIQUOT (""|"with_constr"|"anti"|"list" as n) s ->
            <:with_constr< $anti:mk_anti ~c:"with_constr" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.with_constr_tag
        | "type"; `ANTIQUOT (""|"typ"|"anti" as n) s; "="; t = opt_private_ctyp ->
            <:with_constr< type $anti:mk_anti ~c:"ctyp" n s$ = $t$ >>
        | "type"; t1 = type_longident_and_parameters; "="; t2 = opt_private_ctyp ->
            <:with_constr< type $t1$ = $t2$ >>
        | "module"; i1 = module_longident; "="; i2 = module_longident_with_app ->
            <:with_constr< module $i1$ = $i2$ >> ] ]
    ;
    opt_private_ctyp:
      [ [ "private"; t = ctyp -> <:ctyp< private $t$ >>
        | t = ctyp -> t ] ]
    ;
    class_type_plus:
      [ [ i = lident_colon; t = ctyp LEVEL "star"; "->"; ct = SELF ->
            <:class_type< [ ~ $i$ : $t$ ] -> $ct$ >>
        | "?"; i = a_LIDENT; ":"; t = ctyp LEVEL "star"; "->"; ct = SELF ->
            <:class_type< [ ? $i$ : $t$ ] -> $ct$ >> 
        | i = OPTLABEL (* FIXME inline a_OPTLABEL *); t = ctyp LEVEL "star"; "->"; ct = SELF ->
            <:class_type< [ ? $i$ : $t$ ] -> $ct$ >>
        | test_ctyp_minusgreater; t = ctyp LEVEL "star"; "->"; ct = SELF ->
            <:class_type< [ $t$ ] -> $ct$ >>
        | ct = class_type -> ct ] ]
    ;
    class_type_longident_and_param:
      [ [ "["; t = comma_ctyp; "]"; i = class_type_longident ->
            <:class_type< $id:i$ [ $t$ ] >>
        | i = class_type_longident -> <:class_type< $id:i$ >> ] ]
    ;
    class_longident_and_param:
      [ [ "["; t = comma_ctyp; "]"; ci = class_longident ->
          <:class_expr< $id:ci$ [ $t$ ] >>
        | ci = class_longident -> <:class_expr< $id:ci$ >>
      ] ]
    ;
    class_name_and_param:
      [ [ "["; x = comma_type_parameter; "]"; i = a_LIDENT -> (i, x)
        | i = a_LIDENT -> (i, <:ctyp<>>)
      ] ]
    ;
    ctyp:
      [ [ t1 = SELF; "as"; "'"; i = a_ident -> <:ctyp< $t1$ as '$i$ >> ]
      | "arrow" RIGHTA
        [ t1 = SELF; "->"; t2 = SELF -> <:ctyp< $t1$ -> $t2$ >>
        | i = lident_colon; t1 = ctyp LEVEL "star"; "->"; t2 = SELF ->
            <:ctyp< ( ~ $i$ : $t1$ ) -> $t2$ >>
        | i = a_OPTLABEL; t1 = ctyp LEVEL "star"; "->"; t2 = SELF ->
            <:ctyp< ( ? $i$ : $t1$ ) -> $t2$ >>
        | "?"; i = a_LIDENT; ":"; t1 = ctyp LEVEL "star"; "->"; t2 = SELF ->
            <:ctyp< ( ? $i$ : $t1$ ) -> $t2$ >> ]
      | "star"
        [ t = SELF; "*"; tl = star_ctyp ->
            <:ctyp< ( $t$ * $tl$ ) >> ]
      | "ctyp1"
        [ t1 = SELF; t2 = SELF -> <:ctyp< $t2$ $t1$ >> ]
      | "ctyp2"
        [ t1 = SELF; "."; t2 = SELF ->
            try <:ctyp< $id:Ast.ident_of_ctyp t1$.$id:Ast.ident_of_ctyp t2$ >>
            with [ Invalid_argument s -> raise (Stream.Error s) ]
        | t1 = SELF; "("; t2 = SELF; ")" ->
            let t = <:ctyp< $t1$ $t2$ >> in
            try <:ctyp< $id:Ast.ident_of_ctyp t$ >>
            with [ Invalid_argument s -> raise (Stream.Error s) ] ]
      | "simple"
        [ "'"; i = a_ident -> <:ctyp< '$i$ >>
        | "_" -> <:ctyp< _ >>
        | i = a_LIDENT -> <:ctyp< $lid:i$ >>
        | i = a_UIDENT -> <:ctyp< $uid:i$ >>
        | `ANTIQUOT (""|"typ"|"anti" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | `ANTIQUOT ("tup" as n) s ->
            <:ctyp< ($tup:<:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>$) >>
        | `ANTIQUOT ("id" as n) s ->
            <:ctyp< $id:<:ident< $anti:mk_anti ~c:"ident" n s$ >>$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | "("; t = SELF; ","; mk = comma_ctyp_app; ")";
          i = ctyp LEVEL "ctyp2" ->
            mk <:ctyp< $i$ $t$ >>
        | "("; t = SELF; ")" -> <:ctyp< $t$ >>
        | "#"; i = class_longident -> <:ctyp< # $i$ >>
        | "<"; t = opt_meth_list; ">" -> t
        | "["; OPT "|"; rfl = row_field; "]" ->
            <:ctyp< [ = $rfl$ ] >>
        | "["; ">"; "]" -> <:ctyp< [ > $<:ctyp<>>$ ] >>
        | "["; ">"; OPT "|"; rfl = row_field; "]" ->
            <:ctyp< [ > $rfl$ ] >>
        | "[<"; OPT "|"; rfl = row_field; "]" ->
            <:ctyp< [ < $rfl$ ] >>
        | "[<"; OPT "|"; rfl = row_field; ">"; ntl = name_tags; "]" ->
            <:ctyp< [ < $rfl$ > $ntl$ ] >>
        ] ]
    ;
    meth_list:
      [ [ m = meth_decl -> (m, Ast.BFalse) ] ];
    comma_ctyp_app:
      [ [ t1 = ctyp; ","; t2 = SELF -> fun acc -> t2 <:ctyp< $acc$ $t1$ >>
        | t = ctyp -> fun acc -> <:ctyp< $acc$ $t$ >>
      ] ]
    ;
    star_ctyp:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp*" n s$ >>
        | t1 = ctyp LEVEL "ctyp1"; "*"; t2 = SELF ->
            <:ctyp< $t1$ * $t2$ >>
        | t = ctyp LEVEL "ctyp1" -> t
      ] ]
    ;
    semi:
      [ [ ";;" -> () | -> () ] ]
    ;
    ipatt:
      [ [ p = patt -> p ] ]
    ;
    type_longident_and_parameters:
      [ [ "("; tpl = type_parameters; ")"; i = type_longident ->
            tpl <:ctyp< $id:i$ >>
        | tp = type_parameter; i = type_longident ->
            <:ctyp< $id:i$ $tp$ >>
        | i = type_longident ->
            <:ctyp< $id:i$ >>
      ] ]
    ;
    type_parameters:
      [ [ t1 = type_parameter; ","; t2 = SELF ->
            fun acc -> t2 <:ctyp< $acc$ $t1$ >>
        | t = type_parameter -> fun acc -> <:ctyp< $acc$ $t$ >>
      ] ]
    ;
    type_ident_and_parameters:
      [ [ "("; tpl = LIST1 type_parameter SEP ","; ")"; i = a_LIDENT -> (i, tpl)
        | t = type_parameter; i = a_LIDENT -> (i, [t])
        | i = a_LIDENT -> (i, [])
      ] ]
    ;
    type_kind:
      [ [ "private"; tk = type_kind -> <:ctyp< private $tk$ >>
        | test_constr_decl; OPT "|";
          t = constructor_declarations -> <:ctyp< [ $t$ ] >>
        | t = ctyp -> <:ctyp< $t$ >>
        | t = ctyp; "="; "private"; tk = type_kind ->
            <:ctyp< $t$ == private $tk$ >>
        | t1 = ctyp; "="; "{"; t2 = label_declaration_list; "}" ->
            <:ctyp< $t1$ == { $t2$ } >>
        | t1 = ctyp; "="; OPT "|"; t2 = constructor_declarations ->
            <:ctyp< $t1$ == [ $t2$ ] >>
        | "{"; t = label_declaration_list; "}" ->
            <:ctyp< { $t$ } >> ] ]
    ;
    module_expr: LEVEL "apply"
      [ [ i = SELF; "("; j = SELF; ")" -> <:module_expr< $i$ $j$ >> ] ]
    ;
    ident_quot: LEVEL "apply"
      [ [ i = SELF; "("; j = SELF; ")" -> <:ident< $i$ $j$ >> ] ]
    ;
    module_longident_with_app: LEVEL "apply"
      [ [ i = SELF; "("; j = SELF; ")" -> <:ident< $i$ $j$ >> ] ]
    ;
    type_longident: LEVEL "apply"
      [ [ i = SELF; "("; j = SELF; ")" -> <:ident< $i$ $j$ >> ] ]
    ;
    constructor_arg_list:
      [ [ t1 = SELF; "*"; t2 = SELF -> <:ctyp< $t1$ and $t2$ >>
        | t = ctyp LEVEL "ctyp1" -> t
      ] ]
    ;
    value_let:
      [ [ "let" -> () ] ]
    ;
    value_val:
      [ [ "val" -> () ] ]
    ;
    label_declaration:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | s = a_LIDENT; ":"; t = poly_type ->  <:ctyp< $lid:s$ : $t$ >>
        | "mutable"; s = a_LIDENT; ":"; t = poly_type ->
            <:ctyp< $lid:s$ : mutable $t$ >>
      ] ]
    ;
    poly_type:
      [ [ test_typevar_list_dot; t1 = typevars; "."; t2 = ctyp ->
            <:ctyp< ! $t1$ . $t2$ >>
        | t = ctyp -> t ] ]
    ;
    labeled_ipatt:
      [ [ i = a_LABEL; p = patt LEVEL "simple" ->
            <:patt< ~ $i$ : $p$ >>
        | "~"; i = a_LIDENT -> <:patt< ~ $i$ >>
        | "~"; "("; i = a_LIDENT; ")" ->
            <:patt< ~ $i$ >>
        | "~"; "("; i = a_LIDENT; ":"; t = ctyp; ")" ->
            <:patt< ~ $i$ : ($lid:i$ : $t$) >>
        | i = a_OPTLABEL; j = a_LIDENT -> (* ?a:b <> ?a : b *)
            <:patt< ? $i$ : ($lid:j$) >>
        | i = a_OPTLABEL; "("; p = patt; ")" ->
            <:patt< ? $i$ : ($p$) >>
        | i = a_OPTLABEL; "("; p = patt; "="; e = expr; ")" ->
            <:patt< ? $i$ : ( $p$ = $e$ ) >>
        | i = a_OPTLABEL; "("; p = patt; ":"; t = ctyp; ")" ->
            <:patt< ? $i$ : ( $p$ : $t$ ) >>
        | i = a_OPTLABEL; "("; p = patt; ":"; t = ctyp; "=";
          e = expr; ")" ->
            <:patt< ? $i$ : ( $p$ : $t$ = $e$ ) >>
        | "?"; i = a_LIDENT -> <:patt< ? $i$ >>
        | "?"; "("; i = a_LIDENT; "="; e = expr; ")" ->
            <:patt< ? ( $lid:i$ = $e$ ) >>
        | "?"; "("; i = a_LIDENT; ":"; t = ctyp; "="; e = expr; ")" ->
            <:patt< ? ( $lid:i$ : $t$ = $e$ ) >>
        | "?"; "("; i = a_LIDENT; ")" ->
            <:patt< ? $i$ >>
        | "?"; "("; i = a_LIDENT; ":"; t = ctyp; ")" ->
            <:patt< ? ( $lid:i$ : $t$ ) >>
        | p = patt LEVEL "simple" -> p
      ] ]
    ;
    label_expr:
      [ [ i = label_longident; "="; e = expr LEVEL "top" ->
            <:rec_binding< $i$ = $e$ >> ] ]
    ;
    a_UIDENT:
      [ [ `ANTIQUOT (""|"uid" as n) s -> mk_anti n s
        | `UIDENT "True" -> " True"
        | `UIDENT "False" -> " False"
        | `UIDENT s -> s
      ] ]
    ;
    top_phrase:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; ";;" ->
            Some <:str_item< # $n$ $dp$ >>
        | l = LIST1 str_item; ";;" -> Some (Ast.stSem_of_list l)
        | `EOI -> None
      ] ]
    ;
  END;

  (* Some other DELETE_RULE are before the grammar *)
  DELETE_RULE Gram module_longident_with_app: "("; SELF; ")" END;
  DELETE_RULE Gram type_longident: "("; SELF; ")" END;
  DELETE_RULE Gram ident_quot: "("; SELF; ")" END;
  DELETE_RULE Gram module_longident_with_app: SELF; SELF END;
  DELETE_RULE Gram type_longident: SELF; SELF END;
  DELETE_RULE Gram ident_quot: SELF; SELF END;
  DELETE_RULE Gram module_expr: SELF; SELF END;
end;
let module M = Register.OCamlSyntaxExtension Id Make in ();
