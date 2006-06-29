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


module Id : Sig.Id.S = struct
  value name = "Camlp4Parsers.OCaml";
  value version = "$Id$";
end;

module Make (Syntax : Sig.Camlp4Syntax.S) = struct
  open Sig.Camlp4Token;
  include Syntax;

  Config.constructors_arity.val := False;

  (*FIXME remove this and use OCaml ones *)
  value bigarray_get _loc arr arg =
    let coords =
      match arg with
      [ <:expr< ($e1$, $e2$) >> | <:expr< $e1$, $e2$ >> ->
          Ast.list_of_expr e1 (Ast.list_of_expr e2 [])
      | _ -> [arg] ]
    in
    match coords with
    [ [c1] -> <:expr< Bigarray.Array1.get $arr$ $c1$ >>
    | [c1; c2] -> <:expr< Bigarray.Array2.get $arr$ $c1$ $c2$ >>
    | [c1; c2; c3] -> <:expr< Bigarray.Array3.get $arr$ $c1$ $c2$ $c3$ >>
    (* | coords -> <:expr< Bigarray.Genarray.get $arr$ [| $`list:coords$ |] >> ] *)
    | coords ->
       <:expr< Bigarray.Genarray.get $arr$ [| $Ast.exSem_of_list coords$ |] >> ];

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
  value mkumin _loc f arg =
    match (f, arg) with
    [ ("-", <:expr< $int:n$ >>) when int_of_string n > 0 ->
        <:expr< $int:"-" ^ n$ >>
    | ("-", <:expr< $int32:n$ >>) when (Int32.of_string n) > 0l ->
        <:expr< $int32:"-" ^ n$ >>
    | ("-", <:expr< $int64:n$ >>) when (Int64.of_string n) > 0L ->
        <:expr< $int64:"-" ^ n$ >>
    | ("-", <:expr< $nativeint:n$ >>) when (Nativeint.of_string n) > 0n ->
        <:expr< $nativeint:"-" ^ n$ >>
    | (_, <:expr< $flo:n$ >>) when float_of_string n > 0.0 ->
        <:expr< $flo:"-" ^ n$ >>
    | _ ->
        let f = "~" ^ f in
        <:expr< $lid:f$ $arg$ >> ]
  ;
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

  value get_seq =
    fun
    [ <:expr< do { $e$ } >> -> e
    | e -> e ];

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
        [ Not_found -> try Hashtbl.find ct x.[0] with [ Not_found -> False ] ]
    }
  ;

  value operator_rparen =
    Gram.Entry.of_parser "operator_rparen"
      (fun strm ->
        match Stream.npeek 2 strm with
        [ [(KEYWORD s | SYMBOL s, _); (KEYWORD ")", _)] when is_operator s ->
            do { Stream.junk strm; Stream.junk strm; s }
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
    Gram.Entry.of_parser "prefixop"
      (parser
        [: `(KEYWORD x | SYMBOL x, _loc)
            when
              not (List.mem x excl) && String.length x >= 2 &&
              List.mem x.[0] list && symbolchar x 1 :] ->
          <:expr< $lid:x$ >>)
  ;

  value infixop0 =
    let list_ok = ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$"] in 
    let list_first_char_ok = ['='; '<'; '>'; '|'; '&'; '$'; '!'] in
    let excl = ["<-"; "||"; "&&"] in
    Gram.Entry.of_parser "infixop0"
      (parser
        [: `(KEYWORD x | SYMBOL x, _loc)
            when
              (List.mem x list_ok) ||
              (not (List.mem x excl) && String.length x >= 2 &&
                List.mem x.[0] list_first_char_ok && symbolchar x 1) :] ->
          <:expr< $lid:x$ >>)
  ;

  value infixop1 =
    let list = ['@'; '^'] in
    Gram.Entry.of_parser "infixop1"
      (parser
        [: `(KEYWORD x | SYMBOL x, _loc)
            when
              String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1 :] ->
          <:expr< $lid:x$ >>)
  ;

  value infixop2 =
    let list = ['+'; '-'] in
    Gram.Entry.of_parser "infixop2"
      (parser
        [: `(KEYWORD x | SYMBOL x, _loc)
            when
              x <> "->" && String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1 :] ->
          <:expr< $lid:x$ >>)
  ;

  value infixop3 =
    let list = ['*'; '/'; '%'] in
    Gram.Entry.of_parser "infixop3"
      (parser
        [: `(KEYWORD x | SYMBOL x, _loc)
            when
              String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1 :] ->
          <:expr< $lid:x$ >>)
  ;

  value infixop4 =
    Gram.Entry.of_parser "infixop4"
      (parser
        [: `(KEYWORD x | SYMBOL x, _loc)
            when
              String.length x >= 3 && x.[0] == '*' && x.[1] == '*' &&
              symbolchar x 2 :] ->
          <:expr< $lid:x$ >>)
  ;

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

  (* horrible hack to be able to parse class_types *)

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
        [ Some ((KEYWORD "[" | (LIDENT _ | UIDENT _)), _) -> skip_simple_ctyp 1
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

  value constr_arity = ref [("Some", 1); ("Match_Failure", 1)];

  value rec is_ident_constr_call =
    fun
    [ <:ident< $uid:_$ >> -> True
    | <:ident< $_$.$i$ >> -> is_ident_constr_call i
    | _ -> False ];

  value rec is_expr_constr_call =
    fun
    [ <:expr< $id:i$ >> -> is_ident_constr_call i
    | <:expr< $_$.$e$ >> -> is_expr_constr_call e
    | <:expr< $e$ $_$ >> -> is_expr_constr_call e
    | _ -> False ];

  value rec constr_ident_arity =
    fun
    [ <:ident< $uid:c$ >> ->
        try List.assoc c constr_arity.val with [ Not_found -> 0 ]
    | <:ident< $_$.$i$ >> -> constr_ident_arity i
    | _ -> 1 ];


  value rec constr_expr_arity =
    fun
    [ <:expr< $id:i$ >> -> constr_ident_arity i
    | <:expr< $_$.$e$ >> -> constr_expr_arity e
    | <:expr@_loc< $e$ $_$ >> ->
        if Config.constructors_arity.val && is_expr_constr_call e then
          Loc.raise _loc (Stream.Error "currified constructor")
        else 1
    | _ -> 1 ];

  value rec is_patt_constr_call =
    fun
    [ <:patt< $id:i$ >> -> is_ident_constr_call i
    | <:patt< $p$ $_$ >> -> is_patt_constr_call p
    | _ -> False ];

  value rec constr_patt_arity =
    fun
    [ <:patt< $id:i$ >> -> constr_ident_arity i
    | <:patt@_loc< $p$ $_$ >> ->
        if Config.constructors_arity.val && is_patt_constr_call p then
          Loc.raise _loc (Stream.Error "currified constructor")
        else 1
    | _ -> 1 ];

  value rec patt_lid =
    fun
    [ <:patt< $p1$ $p2$ >> ->
        match p1 with
        [ <:patt< $lid:i$ >> -> Some (Ast.loc_of_patt p1, i, [p2])
        | _ ->
            match patt_lid p1 with
            [ Some (loc, i, pl) -> Some (loc, i, [p2 :: pl])
            | None -> None ] ]
    | _ -> None ]
  ;


  value clear = Gram.Entry.clear;
  clear ctyp;
  clear expr;
  clear patt;
  clear a_LIDENT_or_operator;
  clear type_longident_and_parameters;
  clear type_parameters;
  clear ipatt;
  clear labeled_ipatt;
  clear semi;
  clear let_binding;
  clear type_kind;
  clear constructor_arg_list;
  clear poly_type;
  clear class_name_and_param;
  clear class_fun_def;
  clear class_longident_and_param;
  clear class_type_longident_and_param;
  clear class_type_plus;
  clear type_constraint;
  clear comma_expr;
  clear comma_patt;
  clear sem_expr_for_list;
  clear sem_expr;
  clear label_declaration;
  clear star_ctyp;

  DELETE_RULE Gram value_let: "value" END;
  DELETE_RULE Gram value_val: "value" END;
  DELETE_RULE Gram str_item: value_let; opt_rec; binding END;
  DELETE_RULE Gram module_type: "'"; a_ident END;
  DELETE_RULE Gram assoc: END;
  DELETE_RULE Gram label_expr: label_longident; fun_binding END;

  EXTEND Gram
    GLOBAL:
      a_CHAR a_FLOAT a_INT a_INT32 a_INT64 a_LABEL a_LIDENT
      a_LIDENT_or_operator a_NATIVEINT a_OPTLABEL a_STRING a_UIDENT a_ident
      amp_ctyp and_ctyp assoc assoc_quot binding binding_quot
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
      dummy eq_expr expr expr_eoi expr_quot field field_expr fun_binding
      fun_def ident ident_quot implem interf ipatt ipatt_tcon label
      label_declaration label_expr label_ipatt label_longident label_patt
      labeled_ipatt let_binding meth_list module_binding module_binding0
      module_binding_quot module_declaration module_expr module_expr_quot
      module_longident module_longident_with_app module_rec_declaration
      module_type module_type_quot more_ctyp name_tags opt_as_lident
      opt_class_self_patt opt_class_self_type
      opt_comma_ctyp opt_dot_dot opt_eq_ctyp opt_expr
      opt_meth_list opt_mutable opt_polyt opt_private opt_rec
      opt_virtual opt_when_expr patt patt_as_patt_opt patt_eoi
      patt_quot patt_tcon phrase pipe_ctyp poly_type row_field sem_ctyp
      sem_expr sem_expr_for_list sem_patt sem_patt_for_list semi sequence
      sig_item sig_item_quot sig_items star_ctyp str_item str_item_quot
      str_items top_phrase type_constraint type_declaration
      type_ident_and_parameters type_kind type_longident
      type_longident_and_parameters type_parameter type_parameters typevars
      use_file val_longident value_let value_val with_constr with_constr_quot

      infixop0 infixop1 infixop2 infixop3 infixop4
    ;
    sem_expr:
      [ [ e1 = expr LEVEL "top"; ";"; e2 = SELF -> <:expr< $e1$; $e2$ >>
        | e = expr LEVEL "top"; ";" -> e
        | e = expr LEVEL "top" -> e ] ]
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
              <:str_item< let $opt:r$ $bi$ in $x$ >>
          | "let"; r = opt_rec; bi = binding ->
              match bi with
              [ <:binding< _ = $e$ >> -> <:str_item< $exp:e$ >>
              | _ -> <:str_item< value $opt:r$ $bi$ >> ]
          | "let"; "module"; m = a_UIDENT; mb = module_binding0; "in"; e = expr ->
              <:str_item< let module $m$ = $mb$ in $e$ >>
      ] ]
    ;
    expr:
      [ ";" RIGHTA
        [ e1 = SELF; ";"; e2 = SELF ->
            conc_seq e1 e2
        | e1 = SELF; ";" -> e1 ]
      | "top"
        [ "let"; r = opt_rec; bi = binding; "in";
          x = expr LEVEL ";" ->
            <:expr< let $opt:r$ $bi$ in $x$ >>
        | "let"; "module"; m = a_UIDENT; mb = module_binding0; "in";
          e = expr LEVEL ";" ->
            <:expr< let module $m$ = $mb$ in $e$ >>
        | "function"; OPT "|"; a = assoc ->
            <:expr< fun [ $a$ ] >>
        | "fun"; p = labeled_ipatt; e = fun_def ->
            <:expr< fun $p$ -> $e$ >>
        | "match"; e = SELF; "with"; OPT "|"; a = assoc ->
            <:expr< match $e$ with [ $a$ ] >>
        | "try"; e = SELF; "with"; OPT "|"; a = assoc ->
            <:expr< try $e$ with [ $a$ ] >>
        | "if"; e1 = SELF; "then"; e2 = expr LEVEL "top";
          "else"; e3 = expr LEVEL "top" ->
            <:expr< if $e1$ then $e2$ else $e3$ >>
        | "if"; e1 = SELF; "then"; e2 = expr LEVEL "top" ->
            <:expr< if $e1$ then $e2$ else () >>
        | "for"; i = LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
          "do"; el = SELF; "done" ->
            <:expr< for $i$ = $e1$ $to:df$ $e2$ do { $get_seq el$ } >>
        | "while"; e1 = SELF; "do"; e2 = SELF; "done" ->
            <:expr< while $e1$ do { $get_seq e2$ } >>
        | "object"; csp = opt_class_self_patt; cst = class_structure; "end" ->
            <:expr< object ($csp$) $cst$ end >> ]
      | [ e = SELF; ","; el = (*FIXME comma_expr*)LIST1 NEXT SEP "," ->
            <:expr< ( $e$, $Ast.exCom_of_list el$ ) >> ]
      | ":=" NONA
        [ e1 = SELF; ":="; e2 = expr LEVEL "top" ->
            <:expr< $e1$.val := $e2$ >>
        | e1 = SELF; "<-"; e2 = expr LEVEL "top" ->
            match bigarray_set _loc e1 e2 with
            [ Some e -> e
            | None -> <:expr< $e1$ := $e2$ >> ] ]
      | "||" RIGHTA
        [ e1 = SELF; op = infixop6; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "&&" RIGHTA
        [ e1 = SELF; op = infixop5; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "<" LEFTA
        [ e1 = SELF; op = infixop0; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "^" RIGHTA
        [ e1 = SELF; op = infixop1; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | RIGHTA
        [ e1 = SELF; "::"; e2 = SELF -> <:expr< [$e1$ :: $e2$] >> ]
      | "+" LEFTA
        [ e1 = SELF; op = infixop2; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "*" LEFTA
        [ e1 = SELF; "*"; e2 = SELF -> <:expr< $e1$ * $e2$ >>
        | e1 = SELF; "/"; e2 = SELF -> <:expr< $e1$ / $e2$ >>
        | e1 = SELF; "%"; e2 = SELF -> <:expr< $lid:"%"$ $e1$ $e2$ >>
        | e1 = SELF; "land"; e2 = SELF -> <:expr< $e1$ land $e2$ >>
        | e1 = SELF; "lor"; e2 = SELF -> <:expr< $e1$ lor $e2$ >>
        | e1 = SELF; "lxor"; e2 = SELF -> <:expr< $e1$ lxor $e2$ >>
        | e1 = SELF; "mod"; e2 = SELF -> <:expr< $e1$ mod $e2$ >>
        | e1 = SELF; op = infixop3; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "**" RIGHTA
        [ e1 = SELF; "**"; e2 = SELF -> <:expr< $e1$ ** $e2$ >>
        | e1 = SELF; "asr"; e2 = SELF -> <:expr< $e1$ asr $e2$ >>
        | e1 = SELF; "lsl"; e2 = SELF -> <:expr< $e1$ lsl $e2$ >>
        | e1 = SELF; "lsr"; e2 = SELF -> <:expr< $e1$ lsr $e2$ >>
        | e1 = SELF; op = infixop4; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "unary minus" NONA
        [ "-"; e = SELF -> <:expr< $mkumin _loc "-" e$ >>
        | "-."; e = SELF -> <:expr< $mkumin _loc "-." e$ >> ]
      | "apply" LEFTA
        [ e1 = SELF; e2 = SELF ->
            match constr_expr_arity e1 with
            [ 1 -> <:expr< $e1$ $e2$ >>
            | _ ->
                match e2 with
                [ <:expr< ( $tup:e$ ) >> ->
                    List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>) e1
                                   (Ast.list_of_expr e [])
                | _ -> <:expr< $e1$ $e2$ >> ] ]
        | "assert"; e = SELF ->
            match e with
            [ <:expr< False >> -> <:expr< assert False >>
            | _ -> <:expr< assert $e$ >> ]
        | "lazy"; e = SELF ->
            <:expr< lazy $e$ >> ]
      | "label"
        [ i = LABEL; e = SELF -> <:expr< ~ $i$ : $e$ >> (* Here it's LABEL and not
                                                          tilde_label since ~a:b
                                                          is different than ~a : b *)
        | "~"; i = LIDENT -> <:expr< ~ $i$ >>
        | i = OPTLABEL; e = SELF -> <:expr< ? $i$ : $e$ >> (* Same remark for ?a:b *)
        | "?"; i = LIDENT -> <:expr< ? $i$ >> ]
      | "." LEFTA
        [ e1 = SELF; "."; "("; e2 = SELF; ")" -> <:expr< $e1$ .( $e2$ ) >>
        | e1 = SELF; "."; "["; e2 = SELF; "]" -> <:expr< $e1$ .[ $e2$ ] >>
        | e1 = SELF; "."; "{"; e2 = SELF; "}" -> bigarray_get _loc e1 e2
        | e1 = SELF; "."; e2 = SELF -> <:expr< $e1$ . $e2$ >>
        | e = SELF; "#"; lab = label -> <:expr< $e$ # $lab$ >> ]
      | "~-" NONA
        [ "!"; e = SELF -> <:expr< $e$ . val>>
        | "~-"; e = SELF -> <:expr< ~- $e$ >>
        | "~-."; e = SELF -> <:expr< ~-. $e$ >>
        | f = prefixop; e = SELF -> <:expr< $f$ $e$ >> ]
      | "simple" LEFTA
        [ `QUOTATION x -> Quotation.expand_expr (Gram.parse_string expr) _loc x
        | `ANTIQUOT ("exp"|""|"anti" as n) s ->
            <:expr< $anti:mk_anti ~c:"expr" n s$ >>
        | `ANTIQUOT ("tup" as n) s ->
            <:expr< ($tup: <:expr< $anti:mk_anti ~c:"expr" n s$ >>$) >>
        | s = a_INT -> <:expr< $int:s$ >>
        | s = a_INT32 -> <:expr< $int32:s$ >>
        | s = a_INT64 -> <:expr< $int64:s$ >>
        | s = a_NATIVEINT -> <:expr< $nativeint:s$ >>
        | s = a_FLOAT -> <:expr< $flo:s$ >>
        | s = a_STRING -> <:expr< $str:s$ >>
        | c = a_CHAR -> <:expr< $chr:c$ >>
        | `UIDENT "True" -> <:expr< $uid:" True"$ >>
        | `UIDENT "False" -> <:expr< $uid:" False"$ >>
        | i = val_longident -> <:expr< $id:i$ >>
        | "false" -> <:expr< False >>
        | "true" -> <:expr< True >>
        | "["; "]" -> <:expr< [] >>
        | "["; mk = sem_expr_for_list; "]" -> mk <:expr< [] >>
        | "[|"; "|]" -> <:expr< [| |] >>
        | "[|"; el = sem_expr; "|]" -> <:expr< [| $el$ |] >>
        | "{"; test_label_eq; lel = label_expr; "}" ->
            <:expr< { $lel$ } >>
        | "{"; e = expr LEVEL "."; "with"; lel = label_expr; "}" ->
            <:expr< { ($e$) with $lel$ } >>
        | "{<"; ">}" -> <:expr< {< >} >>
        | "{<"; fel = field_expr; ">}" -> <:expr< {< $fel$ >} >>
        | "("; ")" -> <:expr< () >>
        | "("; op = operator_rparen -> <:expr< $lid:op$ >>
        | "("; e = SELF; ":"; t = ctyp; ")" -> <:expr< ($e$ : $t$) >>
        | "("; e = SELF; ":"; t = ctyp; ":>"; t2 = ctyp; ")" ->
            <:expr< ($e$ : $t$ :> $t2$) >>
        | "("; e = SELF; ":>"; t = ctyp; ")" -> <:expr< ($e$ :> $t$) >>
        | "("; e = SELF; ")" -> <:expr< $e$ >>
        | "begin"; e = SELF; "end" -> <:expr< $e$ >>
        | "begin"; "end" -> <:expr< () >>
        | "new"; i = class_longident -> <:expr< new $i$ >>
        | "`"; s = a_ident -> <:expr< ` $s$ >>
      ] ]
    ;
    val_longident:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $anti:mk_anti ~c:"ident" n s$ >>
        | i = a_UIDENT -> <:ident< $uid:i$ >>
        | i = a_LIDENT -> <:ident< $lid:i$ >>
        | `ANTIQUOT (""|"id"|"anti"|"list" as n) s; "."; i = SELF ->
            <:ident< $anti:mk_anti ~c:"ident" n s$.$i$ >>
        | i = a_UIDENT; "."; "("; j = operator_rparen ->
            <:ident< $uid:i$.$lid:j$ >>
        | i = a_UIDENT; "."; j = SELF -> <:ident< $uid:i$.$j$ >> ] ]
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
      | ".." NONA
        [ p1 = SELF; ".."; p2 = SELF -> <:patt< $p1$ .. $p2$ >> ]
      | "::" RIGHTA
        [ p1 = SELF; "::"; p2 = SELF -> <:patt< [$p1$ :: $p2$] >> ]
      | LEFTA
        [ p1 = SELF; p2 = SELF ->
            match constr_patt_arity p1 with
            [ 1 -> <:patt< $p1$ $p2$ >>
            | n ->
                let p2 =
                  match p2 with
                  [ <:patt@_loc< _ >> when n > 1 -> <:patt< ($tup:p2$) >>
                  | _ -> p2 ]
                in
                match p2 with
                [ <:patt< ( $tup:p$ ) >> ->
                    List.fold_left (fun p1 p2 -> <:patt< $p1$ $p2$ >>) p1
                                   (Ast.list_of_patt p [])
                | _ -> <:patt< $p1$ $p2$ >> ] ] ]
      | "simple"
        [ `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $anti:mk_anti ~c:"patt" n s$ >>
        | `ANTIQUOT ("tup" as n) s -> <:patt< ($tup:<:patt< $anti:mk_anti ~c:"patt" n s$ >>$) >>
        | `UIDENT "True" -> <:patt< $uid:" True"$ >>
        | `UIDENT "False" -> <:patt< $uid:" False"$ >>
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
        | "{"; pl = label_patt; "}" -> <:patt< { $pl$ } >>
        | "("; ")" -> <:patt< () >>
        | "("; op = operator_rparen -> <:patt< $lid:op$ >>
        | "("; p = patt; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
        | "("; p = patt; ")" -> <:patt< $p$ >>
        | "_" -> <:patt< _ >>
        | "`"; s = a_ident -> <:patt< ` $s$ >>
        | "#"; i = type_longident -> <:patt< # $i$ >>
        | `QUOTATION x ->
            Quotation.expand_patt (Gram.parse_string patt) _loc x ] ]
    ;
    infixop5:
      [ [ x = [ "&" | "&&" ] -> <:expr< $lid:x$ >> ] ]
    ;
    infixop6:
      [ [ x = [ "or" | "||" ] -> <:expr< $lid:x$ >> ] ]
    ;
    (* comma_expr:
      [ [ e1 = SELF; ","; e2 = SELF -> <:expr< $e1$, $e2$ >>
        | e = expr LEVEL ":=" -> e ] ]
    ;                                                           *)
    let_binding:
      [ [ p = patt; e = fun_binding ->
            match patt_lid p with
            [ Some (_loc, i, pl) ->
                let e =
                  List.fold_left (fun e p -> <:expr< fun $p$ -> $e$ >>) e pl
                in <:binding< $lid:i$ = $e$ >>
            | None -> <:binding< $p$ = $e$ >> ] ] ]
    ;
    (* comma_patt:
      [ [ p1 = SELF; ","; p2 = SELF -> <:patt< $p1$, $p2$ >>
        | p = patt LEVEL ".." -> p ] ]
    ;                                                           *)
    type_constraint:
      [ [ "constraint" -> () ] ]
    ;
    class_type_plus:
      [ [ test_ctyp_minusgreater; t = ctyp LEVEL "star"; "->"; ct = SELF ->
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
        (*FIXME factor this*)

        (*FIXME a_LIDENT don't works since the dynamic factorisation applies
                only on tokens patt *)
        (* | i = LIDENT; ":"; t1 = ctyp LEVEL "star"; "->"; t2 = SELF -> *)
            (* <:ctyp< ( ~ $i$ : $t1$ ) -> $t2$ >> *)
        | "~"; i = a_LIDENT; ":"; t1 = ctyp LEVEL "star"; "->"; t2 = SELF ->
            <:ctyp< (~ $i$ : $t1$) -> $t2$ >>
        | i = a_LABEL; t1 =  ctyp LEVEL "star"; "->"; t2 = SELF ->
            <:ctyp< (~ $i$ : $t1$) -> $t2$ >>
        | "?"; i = a_LIDENT; ":"; t1 = ctyp LEVEL "star"; "->"; t2 = SELF ->
            <:ctyp< (? $i$ : $t1$) -> $t2$ >>
        | i = a_OPTLABEL; t1 = ctyp LEVEL "star"; "->"; t2 = SELF ->
            <:ctyp< (? $i$ : $t1$) -> $t2$ >> ]
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
        | "("; t = SELF; ","; mk = comma_ctyp_app; ")";
          i = ctyp LEVEL "ctyp2" ->
            mk <:ctyp< $i$ $t$ >>
        | "("; t = SELF; ")" -> <:ctyp< $t$ >>
        | "#"; i = class_longident -> <:ctyp< # $i$ >>
        | "<"; ml = opt_meth_list; v = opt_dot_dot; ">" ->
            <:ctyp< < $ml$ $opt:v$ > >>
        | "["; rfl = row_field; "]" ->
            <:ctyp< [ = $rfl$ ] >>
        | "["; ">"; "]" -> <:ctyp< [ > $<:ctyp<>>$ ] >>
        | "["; ">"; rfl = row_field; "]" ->
            <:ctyp< [ > $rfl$ ] >>
        | "[<"; rfl = row_field; "]" ->
            <:ctyp< [ < $rfl$ ] >>
        | "[<"; rfl = row_field; ">"; ntl = name_tags; "]" ->
            <:ctyp< [ < $rfl$ > $ntl$ ] >>
        ] ]
    ;
    comma_ctyp_app:
      [ [ t1 = ctyp; ","; t2 = SELF -> fun acc -> t2 <:ctyp< $acc$ $t1$ >>
        | t = ctyp -> fun acc -> <:ctyp< $acc$ $t$ >>
      ] ]
    ;
    star_ctyp:
      [ [ t1 = ctyp LEVEL "ctyp1"; "*"; t2 = SELF -> <:ctyp< $t1$ * $t2$ >>
        | t = ctyp LEVEL "ctyp1" -> t
      ] ]
    ;
    semi:
      [ [ ";;" -> () | -> () ] ]
    ;
    ipatt:
      [ [ p = patt LEVEL "simple" -> p ] ]
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
        | t1 = ctyp; "="; "{"; t2 = label_declaration; "}" ->
            <:ctyp< $t1$ == { $t2$ } >>
        | t1 = ctyp; "="; OPT "|"; t2 = constructor_declarations ->
            <:ctyp< $t1$ == [ $t2$ ] >>
        | "{"; t = label_declaration; "}" ->
            <:ctyp< { $t$ } >> ] ]
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
    a_LIDENT_or_operator:
      [ [ x = a_LIDENT -> x
        | "("; x = operator_rparen -> x ] ]
    ;
    label_declaration:
      [ LEFTA
        [ t1 = SELF; ";"; t2 = SELF -> <:ctyp< $t1$; $t2$ >>
        | `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
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
      [ [ p = label_longident; "="; e = expr LEVEL "top" ->
            <:binding< $id:p$ = $e$ >> ] ]
    ;
  END;
end;
let module M = Register.OCamlSyntaxExtension Id Make in ();
