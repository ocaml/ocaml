open Camlp4;                                        (* -*- camlp4r -*- *)
(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

module Id = struct
  value name = "Camlp4OCamlRevisedParser";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  (* Camlp4_config.constructors_arity.val := True; *)
  Camlp4_config.constructors_arity.val := False;

  value help_sequences () =
    do {
      Printf.eprintf "\
New syntax:\
\n    (e1; e2; ... ; en) OR begin e1; e2; ... ; en end\
\n    while e do e1; e2; ... ; en done\
\n    for v = v1 to/downto v2 do e1; e2; ... ; en done\
\nOld syntax (still supported):\
\n    do {e1; e2; ... ; en}\
\n    while e do {e1; e2; ... ; en}\
\n    for v = v1 to/downto v2 do {e1; e2; ... ; en}\
\nVery old (no more supported) syntax:\
\n    do e1; e2; ... ; en-1; return en\
\n    while e do e1; e2; ... ; en; done\
\n    for v = v1 to/downto v2 do e1; e2; ... ; en; done\
\n";
      flush stderr;
      exit 1
    }
  ;
  Options.add "-help_seq" (Arg.Unit help_sequences)
    "Print explanations about new sequences and exit.";

  Gram.Entry.clear a_CHAR;
  Gram.Entry.clear a_FLOAT;
  Gram.Entry.clear a_INT;
  Gram.Entry.clear a_INT32;
  Gram.Entry.clear a_INT64;
  Gram.Entry.clear a_LABEL;
  Gram.Entry.clear a_LIDENT;
  Gram.Entry.clear a_NATIVEINT;
  Gram.Entry.clear a_OPTLABEL;
  Gram.Entry.clear a_STRING;
  Gram.Entry.clear a_UIDENT;
  Gram.Entry.clear a_ident;
  Gram.Entry.clear amp_ctyp;
  Gram.Entry.clear and_ctyp;
  Gram.Entry.clear match_case;
  Gram.Entry.clear match_case0;
  Gram.Entry.clear match_case_quot;
  Gram.Entry.clear binding;
  Gram.Entry.clear binding_quot;
  Gram.Entry.clear rec_binding_quot;
  Gram.Entry.clear class_declaration;
  Gram.Entry.clear class_description;
  Gram.Entry.clear class_expr;
  Gram.Entry.clear class_expr_quot;
  Gram.Entry.clear class_fun_binding;
  Gram.Entry.clear class_fun_def;
  Gram.Entry.clear class_info_for_class_expr;
  Gram.Entry.clear class_info_for_class_type;
  Gram.Entry.clear class_longident;
  Gram.Entry.clear class_longident_and_param;
  Gram.Entry.clear class_name_and_param;
  Gram.Entry.clear class_sig_item;
  Gram.Entry.clear class_sig_item_quot;
  Gram.Entry.clear class_signature;
  Gram.Entry.clear class_str_item;
  Gram.Entry.clear class_str_item_quot;
  Gram.Entry.clear class_structure;
  Gram.Entry.clear class_type;
  Gram.Entry.clear class_type_declaration;
  Gram.Entry.clear class_type_longident;
  Gram.Entry.clear class_type_longident_and_param;
  Gram.Entry.clear class_type_plus;
  Gram.Entry.clear class_type_quot;
  Gram.Entry.clear comma_ctyp;
  Gram.Entry.clear comma_expr;
  Gram.Entry.clear comma_ipatt;
  Gram.Entry.clear comma_patt;
  Gram.Entry.clear comma_type_parameter;
  Gram.Entry.clear constrain;
  Gram.Entry.clear constructor_arg_list;
  Gram.Entry.clear constructor_declaration;
  Gram.Entry.clear constructor_declarations;
  Gram.Entry.clear ctyp;
  Gram.Entry.clear ctyp_quot;
  Gram.Entry.clear cvalue_binding;
  Gram.Entry.clear direction_flag;
  Gram.Entry.clear dummy;
  Gram.Entry.clear eq_expr;
  Gram.Entry.clear expr;
  Gram.Entry.clear expr_eoi;
  Gram.Entry.clear expr_quot;
  Gram.Entry.clear field_expr;
  Gram.Entry.clear field_expr_list;
  Gram.Entry.clear fun_binding;
  Gram.Entry.clear fun_def;
  Gram.Entry.clear ident;
  Gram.Entry.clear ident_quot;
  Gram.Entry.clear implem;
  Gram.Entry.clear interf;
  Gram.Entry.clear ipatt;
  Gram.Entry.clear ipatt_tcon;
  Gram.Entry.clear label;
  Gram.Entry.clear label_declaration;
  Gram.Entry.clear label_declaration_list;
  Gram.Entry.clear label_expr_list;
  Gram.Entry.clear label_expr;
  Gram.Entry.clear label_ipatt;
  Gram.Entry.clear label_ipatt_list;
  Gram.Entry.clear label_longident;
  Gram.Entry.clear label_patt;
  Gram.Entry.clear label_patt_list;
  Gram.Entry.clear labeled_ipatt;
  Gram.Entry.clear let_binding;
  Gram.Entry.clear meth_list;
  Gram.Entry.clear meth_decl;
  Gram.Entry.clear module_binding;
  Gram.Entry.clear module_binding0;
  Gram.Entry.clear module_binding_quot;
  Gram.Entry.clear module_declaration;
  Gram.Entry.clear module_expr;
  Gram.Entry.clear module_expr_quot;
  Gram.Entry.clear module_longident;
  Gram.Entry.clear module_longident_with_app;
  Gram.Entry.clear module_rec_declaration;
  Gram.Entry.clear module_type;
  Gram.Entry.clear module_type_quot;
  Gram.Entry.clear more_ctyp;
  Gram.Entry.clear name_tags;
  Gram.Entry.clear opt_as_lident;
  Gram.Entry.clear opt_class_self_patt;
  Gram.Entry.clear opt_class_self_type;
  Gram.Entry.clear opt_comma_ctyp;
  Gram.Entry.clear opt_dot_dot;
  Gram.Entry.clear opt_eq_ctyp;
  Gram.Entry.clear opt_expr;
  Gram.Entry.clear opt_meth_list;
  Gram.Entry.clear opt_mutable;
  Gram.Entry.clear opt_polyt;
  Gram.Entry.clear opt_private;
  Gram.Entry.clear opt_rec;
  Gram.Entry.clear opt_virtual;
  Gram.Entry.clear opt_when_expr;
  Gram.Entry.clear patt;
  Gram.Entry.clear patt_as_patt_opt;
  Gram.Entry.clear patt_eoi;
  Gram.Entry.clear patt_quot;
  Gram.Entry.clear patt_tcon;
  Gram.Entry.clear phrase;
  Gram.Entry.clear poly_type;
  Gram.Entry.clear row_field;
  Gram.Entry.clear sem_expr;
  Gram.Entry.clear sem_expr_for_list;
  Gram.Entry.clear sem_patt;
  Gram.Entry.clear sem_patt_for_list;
  Gram.Entry.clear semi;
  Gram.Entry.clear sequence;
  Gram.Entry.clear sig_item;
  Gram.Entry.clear sig_item_quot;
  Gram.Entry.clear sig_items;
  Gram.Entry.clear star_ctyp;
  Gram.Entry.clear str_item;
  Gram.Entry.clear str_item_quot;
  Gram.Entry.clear str_items;
  Gram.Entry.clear top_phrase;
  Gram.Entry.clear type_constraint;
  Gram.Entry.clear type_declaration;
  Gram.Entry.clear type_ident_and_parameters;
  Gram.Entry.clear type_kind;
  Gram.Entry.clear type_longident;
  Gram.Entry.clear type_longident_and_parameters;
  Gram.Entry.clear type_parameter;
  Gram.Entry.clear type_parameters;
  Gram.Entry.clear typevars;
  Gram.Entry.clear use_file;
  Gram.Entry.clear val_longident;
  Gram.Entry.clear value_let;
  Gram.Entry.clear value_val;
  Gram.Entry.clear with_constr;
  Gram.Entry.clear with_constr_quot;

  value neg_string n =
    let len = String.length n in
    if len > 0 && n.[0] = '-' then String.sub n 1 (len - 1)
    else "-" ^ n
  ;

  value mkumin _loc f arg =
    match arg with
    [ <:expr< $int:n$ >> -> <:expr< $int:neg_string n$ >>
    | <:expr< $int32:n$ >> -> <:expr< $int32:neg_string n$ >>
    | <:expr< $int64:n$ >> -> <:expr< $int64:neg_string n$ >>
    | <:expr< $nativeint:n$ >> -> <:expr< $nativeint:neg_string n$ >>
    | <:expr< $flo:n$ >> -> <:expr< $flo:neg_string n$ >>
    | _ -> <:expr< $lid:"~" ^ f$ $arg$ >> ];

  value mklistexp _loc last =
    loop True where rec loop top =
      fun
      [ [] ->
          match last with
          [ Some e -> e
          | None -> <:expr< [] >> ]
      | [e1 :: el] ->
          let _loc =
            if top then _loc else Loc.merge (Ast.loc_of_expr e1) _loc
          in
          <:expr< [$e1$ :: $loop False el$] >> ]
  ;

  value mkassert _loc =
    fun
    [ <:expr< False >> ->
        <:expr< assert False >> (* this case takes care about
                                   the special assert false node *)
    | e -> <:expr< assert $e$ >> ]
  ;

  value append_eLem el e = el @ [e];
  value mk_anti ?(c = "") n s = "\\$"^n^c^":"^s;

  value mksequence _loc =
    fun
    [ <:expr< $_$; $_$ >> | <:expr< $anti:_$ >> as e -> <:expr< do { $e$ } >>
    | e -> e ]
  ;

  value mksequence' _loc =
    fun
    [ <:expr< $_$; $_$ >> as e -> <:expr< do { $e$ } >>
    | e -> e ]
  ;

  value rec lid_of_ident =
    fun
    [ <:ident< $_$ . $i$ >> -> lid_of_ident i
    | <:ident< $lid:lid$ >> -> lid
    | _                     -> assert False ];

  value module_type_app mt1 mt2 =
    match (mt1, mt2) with
    [ (<:module_type@_loc< $id:i1$ >>, <:module_type< $id:i2$ >>) ->
        <:module_type< $id:<:ident< $i1$ $i2$ >>$ >>
    | _ -> raise Stream.Failure ];

  value module_type_acc mt1 mt2 =
    match (mt1, mt2) with
    [ (<:module_type@_loc< $id:i1$ >>, <:module_type< $id:i2$ >>) ->
        <:module_type< $id:<:ident< $i1$.$i2$ >>$ >>
    | _ -> raise Stream.Failure ];

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
    (* | coords -> <:expr< Bigarray.Genarray.get $arr$ [| $list:coords$ |] >> ] *)
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

  value stopped_at _loc =
    Some (Loc.move_line 1 _loc) (* FIXME be more precise *);

  value rec generalized_type_of_type =
    fun
    [ <:ctyp< $t1$ -> $t2$ >> ->
        let (tl, rt) = generalized_type_of_type t2 in
        ([t1 :: tl], rt)
    | t ->
        ([], t) ]
  ;

  value symbolchar =
    let list =
      ['$'; '!'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?';
       '@'; '^'; '|'; '~'; '\\']
    in
    let rec loop s i =
      if i == String.length s then True
      else if List.mem s.[i] list then loop s (i + 1)
      else False
    in
    loop
  ;

  value setup_op_parser entry p =
    Gram.Entry.setup_parser entry
      (parser
        [: `(KEYWORD x | SYMBOL x, ti) when p x :] ->
          let _loc = Gram.token_location ti in
          <:expr< $lid:x$ >>);

  let list = ['!'; '?'; '~'] in
  let excl = ["!="; "??"] in
  setup_op_parser prefixop
    (fun x -> not (List.mem x excl) && String.length x >= 2 &&
              List.mem x.[0] list && symbolchar x 1);

  let list_ok = ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$"] in
  let list_first_char_ok = ['='; '<'; '>'; '|'; '&'; '$'; '!'] in
  let excl = ["<-"; "||"; "&&"] in
  setup_op_parser infixop0
    (fun x -> (List.mem x list_ok) ||
              (not (List.mem x excl) && String.length x >= 2 &&
              List.mem x.[0] list_first_char_ok && symbolchar x 1));

  let list = ['@'; '^'] in
  setup_op_parser infixop1
    (fun x -> String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1);

  let list = ['+'; '-'] in
  setup_op_parser infixop2
    (fun x -> x <> "->" && String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1);

  let list = ['*'; '/'; '%'; '\\'] in
  setup_op_parser infixop3
    (fun x -> String.length x >= 1 && List.mem x.[0] list &&
              (x.[0] <> '*' || String.length x < 2 || x.[1] <> '*') &&
              symbolchar x 1);

  setup_op_parser infixop4
    (fun x -> String.length x >= 2 && x.[0] == '*' && x.[1] == '*' &&
              symbolchar x 2);

  value rec infix_kwds_filter =
    parser
    [ [: `((KEYWORD "(", _) as tok); xs :] ->
        match xs with parser
        [ [: `(KEYWORD ("or"|"mod"|"land"|"lor"|"lxor"|"lsl"|"lsr"|"asr"|"*" as i)  , _loc);
             `(KEYWORD ")" , _); xs :] ->
               [: `(LIDENT i, _loc); infix_kwds_filter xs :]
        | [: xs :] ->
                [: `tok; infix_kwds_filter xs :] ]
    | [: `x; xs :] -> [: `x; infix_kwds_filter xs :] ];

  Token.Filter.define_filter (Gram.get_filter ())
    (fun f strm -> infix_kwds_filter (f strm));

  Gram.Entry.setup_parser sem_expr begin
    let symb1 = Gram.parse_tokens_after_filter expr in
    let symb =
      parser
      [ [: `(ANTIQUOT ("list" as n) s, ti) :] ->
        let _loc = Gram.token_location ti in
        <:expr< $anti:mk_anti ~c:"expr;" n s$ >>
      | [: a = symb1 :] -> a ]
    in
    let rec kont al =
      parser
      [ [: `(KEYWORD ";", _); a = symb; s :] ->
        let _loc = Loc.merge (Ast.loc_of_expr al)
                             (Ast.loc_of_expr a) in
        kont <:expr< $al$; $a$ >> s
      | [: :] -> al ]
    in
    parser [: a = symb; s :] -> kont a s
  end;

  let apply () = EXTEND Gram
    GLOBAL:
      a_CHAR a_FLOAT a_INT a_INT32 a_INT64 a_LABEL a_LIDENT rec_binding_quot
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
      dummy eq_expr expr expr_eoi expr_quot field_expr field_expr_list fun_binding
      fun_def ident ident_quot implem interf ipatt ipatt_tcon label
      label_declaration label_declaration_list label_expr label_expr_list
      label_ipatt label_ipatt_list label_longident label_patt label_patt_list
      labeled_ipatt let_binding meth_list meth_decl module_binding module_binding0
      module_binding_quot module_declaration module_expr module_expr_quot
      module_longident module_longident_with_app module_rec_declaration
      module_type module_type_quot more_ctyp name_tags opt_as_lident
      opt_class_self_patt opt_class_self_type opt_comma_ctyp opt_dot_dot opt_eq_ctyp opt_expr
      opt_meth_list opt_mutable opt_polyt opt_private opt_rec
      opt_virtual opt_when_expr patt patt_as_patt_opt patt_eoi
      patt_quot patt_tcon phrase poly_type row_field
      sem_expr sem_expr_for_list sem_patt sem_patt_for_list semi sequence
      sig_item sig_item_quot sig_items star_ctyp str_item str_item_quot
      str_items top_phrase type_constraint type_declaration
      type_ident_and_parameters type_kind type_longident
      type_longident_and_parameters type_parameter type_parameters typevars
      use_file val_longident value_let value_val with_constr with_constr_quot
      infixop0 infixop1 infixop2 infixop3 infixop4 do_sequence package_type
      rec_flag_quot direction_flag_quot mutable_flag_quot private_flag_quot
      virtual_flag_quot row_var_flag_quot override_flag_quot;
    module_expr:
      [ "top"
        [ "functor"; "("; i = a_UIDENT; ":"; t = module_type; ")"; "->";
          me = SELF ->
            <:module_expr< functor ( $i$ : $t$ ) -> $me$ >>
        | "struct"; st = str_items; "end" ->
            <:module_expr< struct $st$ end >> ]
      | "apply"
        [ me1 = SELF; me2 = SELF -> <:module_expr< $me1$ $me2$ >> ]
      | "simple"
        [ `ANTIQUOT (""|"mexp"|"anti"|"list" as n) s ->
            <:module_expr< $anti:mk_anti ~c:"module_expr" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.module_expr_tag
        | i = module_longident -> <:module_expr< $id:i$ >>
        | "("; me = SELF; ":"; mt = module_type; ")" ->
            <:module_expr< ( $me$ : $mt$ ) >>
        | "("; me = SELF; ")" -> <:module_expr< $me$ >>
        | "("; value_val; e = expr; ")" ->
            <:module_expr< (value $e$) >>
        | "("; value_val; e = expr; ":"; p = package_type; ")" ->
            <:module_expr< (value $e$ : $p$) >> ] ]
    ;
    str_item:
      [ "top"
        [ "exception"; t = constructor_declaration ->
            <:str_item< exception $t$ >>
        | "exception"; t = constructor_declaration; "="; i = type_longident ->
            <:str_item< exception $t$ = $i$ >>
        | "external"; i = a_LIDENT; ":"; t = ctyp; "="; sl = string_list ->
            <:str_item< external $i$ : $t$ = $sl$ >>
        | "include"; me = module_expr -> <:str_item< include $me$ >>
        | "module"; i = a_UIDENT; mb = module_binding0 ->
            <:str_item< module $i$ = $mb$ >>
        | "module"; "rec"; mb = module_binding ->
            <:str_item< module rec $mb$ >>
        | "module"; "type"; i = a_ident; "="; mt = module_type ->
            <:str_item< module type $i$ = $mt$ >>
        | "open"; "!"; i = module_longident -> Ast.StOpn _loc Ast.OvOverride i                 
        | "open"; i = module_longident ->
            Ast.StOpn _loc Ast.OvNil i 
            (* <:str_item< open $i$ >> *)

        | "type"; td = type_declaration ->
            <:str_item< type $td$ >>
        | value_let; r = opt_rec; bi = binding ->
            <:str_item< value $rec:r$ $bi$ >>
        | "class"; cd = class_declaration ->
            <:str_item< class $cd$ >>
        | "class"; "type"; ctd = class_type_declaration ->
            <:str_item< class type $ctd$ >>
        | `ANTIQUOT (""|"stri"|"anti"|"list" as n) s ->
            <:str_item< $anti:mk_anti ~c:"str_item" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.str_item_tag
        | e = expr -> <:str_item< $exp:e$ >> ] ]
    ;
    module_binding0:
      [ RIGHTA
        [ "("; m = a_UIDENT; ":"; mt = module_type; ")"; mb = SELF ->
            <:module_expr< functor ( $m$ : $mt$ ) -> $mb$ >>
        | ":"; mt = module_type; "="; me = module_expr ->
            <:module_expr< ( $me$ : $mt$ ) >>
        | "="; me = module_expr -> <:module_expr< $me$ >> ] ]
    ;
    module_binding:
      [ LEFTA
        [ b1 = SELF; "and"; b2 = SELF ->
            <:module_binding< $b1$ and $b2$ >>
        | `ANTIQUOT ("module_binding"|"anti"|"list" as n) s ->
            <:module_binding< $anti:mk_anti ~c:"module_binding" n s$ >>
        | `ANTIQUOT ("" as n) s ->
            <:module_binding< $anti:mk_anti ~c:"module_binding" n s$ >>
        | `ANTIQUOT ("" as n) m; ":"; mt = module_type; "="; me = module_expr ->
            <:module_binding< $mk_anti n m$ : $mt$ = $me$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.module_binding_tag
        | m = a_UIDENT; ":"; mt = module_type; "="; me = module_expr ->
            <:module_binding< $m$ : $mt$ = $me$ >> ] ]
    ;
    module_type:
      [ "top"
        [ "functor"; "("; i = a_UIDENT; ":"; t = SELF; ")"; "->"; mt = SELF ->
            <:module_type< functor ( $i$ : $t$ ) -> $mt$ >> ]
      | "with"
        [ mt = SELF; "with"; wc = with_constr ->
            <:module_type< $mt$ with $wc$ >> ]
      | "apply"
        [ mt1 = SELF; mt2 = SELF; dummy -> module_type_app mt1 mt2 ]
      | "."
        [ mt1 = SELF; "."; mt2 = SELF -> module_type_acc mt1 mt2 ]
      | "sig"
        [ "sig"; sg = sig_items; "end" ->
            <:module_type< sig $sg$ end >> ]
      | "simple"
        [ `ANTIQUOT (""|"mtyp"|"anti"|"list" as n) s ->
            <:module_type< $anti:mk_anti ~c:"module_type" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.module_type_tag
        | i = module_longident_with_app -> <:module_type< $id:i$ >>
        | "'"; i = a_ident -> <:module_type< ' $i$ >>
        | "("; mt = SELF; ")" -> <:module_type< $mt$ >>
        | "module"; "type"; "of"; me = module_expr ->
            <:module_type< module type of $me$ >> ] ]
    ;
    sig_item:
      [ "top"
        [ `ANTIQUOT (""|"sigi"|"anti"|"list" as n) s ->
            <:sig_item< $anti:mk_anti ~c:"sig_item" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.sig_item_tag
        | "exception"; t = constructor_declaration ->
            <:sig_item< exception $t$ >>
        | "external"; i = a_LIDENT; ":"; t = ctyp; "="; sl = string_list ->
            <:sig_item< external $i$ : $t$ = $sl$ >>
        | "include"; mt = module_type -> <:sig_item< include $mt$ >>
        | "module"; i = a_UIDENT; mt = module_declaration ->
            <:sig_item< module $i$ : $mt$ >>
        | "module"; "rec"; mb = module_rec_declaration ->
            <:sig_item< module rec $mb$ >>
        | "module"; "type"; i = a_ident; "="; mt = module_type ->
            <:sig_item< module type $i$ = $mt$ >>
        | "module"; "type"; i = a_ident ->
            <:sig_item< module type $i$ >>
        | "open"; i = module_longident ->
            <:sig_item< open $i$ >>
        | "type"; t = type_declaration ->
            <:sig_item< type $t$ >>
        | value_val; i = a_LIDENT; ":"; t = ctyp ->
            <:sig_item< value $i$ : $t$ >>
        | "class"; cd = class_description ->
            <:sig_item< class $cd$ >>
        | "class"; "type"; ctd = class_type_declaration ->
            <:sig_item< class type $ctd$ >> ] ]
    ;
    module_declaration:
      [ RIGHTA
        [ ":"; mt = module_type -> <:module_type< $mt$ >>
        | "("; i = a_UIDENT; ":"; t = module_type; ")"; mt = SELF ->
            <:module_type< functor ( $i$ : $t$ ) -> $mt$ >> ] ]
    ;
    module_rec_declaration:
      [ LEFTA
        [ m1 = SELF; "and"; m2 = SELF -> <:module_binding< $m1$ and $m2$ >>
        | `ANTIQUOT (""|"module_binding"|"anti"|"list" as n) s ->
            <:module_binding< $anti:mk_anti ~c:"module_binding" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.module_binding_tag
        | m = a_UIDENT; ":"; mt = module_type -> <:module_binding< $m$ : $mt$ >>
      ] ]
    ;
    with_constr:
      [ LEFTA
        [ wc1 = SELF; "and"; wc2 = SELF -> <:with_constr< $wc1$ and $wc2$ >>
        | `ANTIQUOT (""|"with_constr"|"anti"|"list" as n) s ->
            <:with_constr< $anti:mk_anti ~c:"with_constr" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.with_constr_tag
        | "type"; `ANTIQUOT (""|"typ"|"anti" as n) s; "="; t = ctyp ->
            <:with_constr< type $anti:mk_anti ~c:"ctyp" n s$ = $t$ >>
        | "type"; t1 = type_longident_and_parameters; "="; t2 = ctyp ->
            <:with_constr< type $t1$ = $t2$ >>
        | "module"; i1 = module_longident; "="; i2 = module_longident_with_app ->
            <:with_constr< module $i1$ = $i2$ >>
        | "type"; `ANTIQUOT (""|"typ"|"anti" as n) s; ":="; t = ctyp ->
            <:with_constr< type $anti:mk_anti ~c:"ctyp" n s$ := $t$ >>
        | "type"; t1 = type_longident_and_parameters; ":="; t2 = ctyp ->
            <:with_constr< type $t1$ := $t2$ >>
        | "module"; i1 = module_longident; ":="; i2 = module_longident_with_app ->
            <:with_constr< module $i1$ := $i2$ >> ] ]
    ;
    expr:
      [ "top" RIGHTA
        [ "let"; r = opt_rec; bi = binding; "in"; x = SELF ->
            <:expr< let $rec:r$ $bi$ in $x$ >>
        | "let"; "module"; m = a_UIDENT; mb = module_binding0; "in"; e = SELF ->
            <:expr< let module $m$ = $mb$ in $e$ >>

        | "let"; "open"; "!"; i = module_longident; "in"; e = SELF ->
            <:expr< let open! $id:i$ in $e$>>
        | "let"; "open"; i = module_longident; "in"; e = SELF ->
            <:expr< let open $id:i$ in $e$ >>
        | "fun"; "["; a = LIST0 match_case0 SEP "|"; "]" ->
            <:expr< fun [ $list:a$ ] >>
        | "fun"; e = fun_def -> e
        | "match"; e = sequence; "with"; a = match_case ->
            <:expr< match $mksequence' _loc e$ with [ $a$ ] >>
        | "try"; e = sequence; "with"; a = match_case ->
            <:expr< try $mksequence' _loc e$ with [ $a$ ] >>
        | "if"; e1 = SELF; "then"; e2 = SELF; "else"; e3 = SELF ->
            <:expr< if $e1$ then $e2$ else $e3$ >>
        | "do"; seq = do_sequence -> mksequence _loc seq
        | "for"; i = a_LIDENT; "="; e1 = sequence; df = direction_flag;
          e2 = sequence; "do"; seq = do_sequence ->
            <:expr< for $i$ = $mksequence' _loc e1$ $to:df$ $mksequence' _loc e2$ do { $seq$ } >>
        | "while"; e = sequence; "do"; seq = do_sequence ->
            <:expr< while $mksequence' _loc e$ do { $seq$ } >>
        | "object"; csp = opt_class_self_patt; cst = class_structure; "end" ->
            <:expr< object ($csp$) $cst$ end >>
        | e = SELF; "[@"; s = a_LIDENT; str = str_items; "]" ->
            Ast.ExAtt _loc s str e
             ]
      | "where"
        [ e = SELF; "where"; rf = opt_rec; lb = let_binding ->
            <:expr< let $rec:rf$ $lb$ in $e$ >> ]
      | ":=" NONA
        [ e1 = SELF; ":="; e2 = SELF; dummy ->
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
      | "+" LEFTA
        [ e1 = SELF; op = infixop2; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "*" LEFTA
        [ e1 = SELF; "land"; e2 = SELF -> <:expr< $e1$ land $e2$ >>
        | e1 = SELF; "lor"; e2 = SELF -> <:expr< $e1$ lor $e2$ >>
        | e1 = SELF; "lxor"; e2 = SELF -> <:expr< $e1$ lxor $e2$ >>
        | e1 = SELF; "mod"; e2 = SELF -> <:expr< $e1$ mod $e2$ >>
        | e1 = SELF; op = infixop3; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "**" RIGHTA
        [ e1 = SELF; "asr"; e2 = SELF -> <:expr< $e1$ asr $e2$ >>
        | e1 = SELF; "lsl"; e2 = SELF -> <:expr< $e1$ lsl $e2$ >>
        | e1 = SELF; "lsr"; e2 = SELF -> <:expr< $e1$ lsr $e2$ >>
        | e1 = SELF; op = infixop4; e2 = SELF -> <:expr< $op$ $e1$ $e2$ >> ]
      | "unary minus" NONA
        [ "-"; e = SELF -> mkumin _loc "-" e
        | "-."; e = SELF -> mkumin _loc "-." e ]
      | "apply" LEFTA
        [ e1 = SELF; e2 = SELF -> <:expr< $e1$ $e2$ >>
        | "assert"; e = SELF -> mkassert _loc e
        | "new"; i = class_longident -> <:expr< new $i$ >>
        | "lazy"; e = SELF -> <:expr< lazy $e$ >> ]
      | "label" NONA
        [ "~"; i = a_LIDENT; ":"; e = SELF -> <:expr< ~ $i$ : $e$ >>
        | "~"; i = a_LIDENT -> <:expr< ~ $i$ >>

        (* Here it's LABEL and not tilde_label since ~a:b is different than ~a : b *)
        | `LABEL i; e = SELF -> <:expr< ~ $i$ : $e$ >>

        (* Same remark for ?a:b *)
        | `OPTLABEL i; e = SELF -> <:expr< ? $i$ : $e$ >>

        | "?"; i = a_LIDENT; ":"; e = SELF -> <:expr< ? $i$ : $e$ >>
        | "?"; i = a_LIDENT -> <:expr< ? $i$ >> ]
      | "." LEFTA
        [ e1 = SELF; "."; "("; e2 = SELF; ")" -> <:expr< $e1$ .( $e2$ ) >>
        | e1 = SELF; "."; "["; e2 = SELF; "]" -> <:expr< $e1$ .[ $e2$ ] >>
        | e1 = SELF; "."; "{"; e2 = comma_expr; "}" -> bigarray_get _loc e1 e2
        | e1 = SELF; "."; e2 = SELF -> <:expr< $e1$ . $e2$ >>
        | e = SELF; "#"; lab = label -> <:expr< $e$ # $lab$ >> ]
      | "~-" NONA
        [ "!"; e = SELF -> <:expr< $e$.val >>
        | f = prefixop; e = SELF -> <:expr< $f$ $e$ >> ]
      | "simple"
        [ `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.expr_tag
        | `ANTIQUOT ("exp"|""|"anti" as n) s ->
            <:expr< $anti:mk_anti ~c:"expr" n s$ >>
        | `ANTIQUOT ("`bool" as n) s ->
            <:expr< $id:<:ident< $anti:mk_anti n s$ >>$ >>
        | `ANTIQUOT ("tup" as n) s ->
            <:expr< $tup: <:expr< $anti:mk_anti ~c:"expr" n s$ >>$ >>
        | `ANTIQUOT ("seq" as n) s ->
            <:expr< do $anti:mk_anti ~c:"expr" n s$ done >>
        | s = a_INT -> <:expr< $int:s$ >>
        | s = a_INT32 -> <:expr< $int32:s$ >>
        | s = a_INT64 -> <:expr< $int64:s$ >>
        | s = a_NATIVEINT -> <:expr< $nativeint:s$ >>
        | s = a_FLOAT -> <:expr< $flo:s$ >>
        | s = a_STRING -> <:expr< $str:s$ >>
        | s = a_CHAR -> <:expr< $chr:s$ >>
        | i = TRY module_longident_dot_lparen; e = sequence; ")" ->
            <:expr< let open $i$ in $e$ >> 
        | i = TRY val_longident -> <:expr< $id:i$ >>
        | "`"; s = a_ident -> <:expr< ` $s$ >>
        | "["; "]" -> <:expr< [] >>
        | "["; mk_list = sem_expr_for_list; "::"; last = expr; "]" ->
            mk_list last
        | "["; mk_list = sem_expr_for_list; "]" ->
            mk_list <:expr< [] >>
        | "[|"; "|]" -> <:expr< [| $<:expr<>>$ |] >>
        | "[|"; el = sem_expr; "|]" -> <:expr< [| $el$ |] >>
        | "{"; el = label_expr_list; "}" -> <:expr< { $el$ } >>
        | "{"; "("; e = SELF; ")"; "with"; el = label_expr_list; "}" ->
            <:expr< { ($e$) with $el$ } >>
        | "{<"; ">}" -> <:expr< {<>} >>
        | "{<"; fel = field_expr_list; ">}" -> <:expr< {< $fel$ >} >>
        | "("; ")" -> <:expr< () >>
        | "("; e = SELF; ":"; t = ctyp; ")" -> <:expr< ($e$ : $t$) >>
        | "("; e = SELF; ","; el = comma_expr; ")" -> <:expr< ( $e$, $el$ ) >>
        | "("; e = SELF; ";"; seq = sequence; ")" -> mksequence _loc <:expr< $e$; $seq$ >>
        | "("; e = SELF; ";"; ")" -> mksequence _loc e
        | "("; e = SELF; ":"; t = ctyp; ":>"; t2 = ctyp; ")" ->
            <:expr< ($e$ : $t$ :> $t2$ ) >>
        | "("; e = SELF; ":>"; t = ctyp; ")" -> <:expr< ($e$ :> $t$) >>
        | "("; e = SELF; ")" -> e
        | "begin"; seq = sequence; "end" -> mksequence _loc seq
        | "begin"; "end" -> <:expr< () >>
        | "("; "module"; me = module_expr; ")" ->
            <:expr< (module $me$) >>
        | "("; "module"; me = module_expr; ":"; pt = package_type; ")" ->
            <:expr< (module $me$ : $pt$) >>
        ] ]
    ;
    do_sequence:
      [ [ seq = TRY ["{"; seq = sequence; "}" -> seq] -> seq
        | TRY ["{"; "}"] -> <:expr< () >>
        | seq = TRY [seq = sequence; "done" -> seq] -> seq
        | "done" -> <:expr< () >>
      ] ]
    ;
    infixop5:
      [ [ x = [ "&" | "&&" ] -> <:expr< $lid:x$ >> ] ]
    ;
    infixop6:
      [ [ x = [ "or" | "||" ] -> <:expr< $lid:x$ >> ] ]
    ;
    sem_expr_for_list:
      [ [ e = expr; ";"; el = SELF -> fun acc -> <:expr< [ $e$ :: $el acc$ ] >>
        | e = expr; ";" -> fun acc -> <:expr< [ $e$ :: $acc$ ] >>
        | e = expr -> fun acc -> <:expr< [ $e$ :: $acc$ ] >>
      ] ]
    ;
    comma_expr:
      [ [ e1 = SELF; ","; e2 = SELF -> <:expr< $e1$, $e2$ >>
        | `ANTIQUOT ("list" as n) s -> <:expr< $anti:mk_anti ~c:"expr," n s$ >>
        | e = expr LEVEL "top" -> e ] ]
    ;
    dummy:
      [ [ -> () ] ]
    ;
    sequence':
      [ [ -> fun e -> e
        | ";" -> fun e -> e
        | ";"; el = sequence -> fun e -> <:expr< $e$; $el$ >> ] ]
    ;
    sequence:
      [ [ "let"; rf = opt_rec; bi = binding; "in"; e = expr; k = sequence' ->
            k <:expr< let $rec:rf$ $bi$ in $e$ >>
        | "let"; rf = opt_rec; bi = binding; ";"; el = SELF ->
            <:expr< let $rec:rf$ $bi$ in $mksequence _loc el$ >>
        | "let"; "module"; m = a_UIDENT; mb = module_binding0; "in"; e = expr; k = sequence' ->
            k <:expr< let module $m$ = $mb$ in $e$ >>
        | "let"; "module"; m = a_UIDENT; mb = module_binding0; ";"; el = SELF ->
            <:expr< let module $m$ = $mb$ in $mksequence _loc el$ >>

        | "let"; "open"; "!"; i = module_longident; "in"; e = SELF ->
            <:expr< let open! $id:i$ in $e$ >>
        | "let"; "open"; i = module_longident; "in"; e = SELF ->
            <:expr< let open $id:i$ in $e$ >>
        | `ANTIQUOT ("list" as n) s -> <:expr< $anti:mk_anti ~c:"expr;" n s$ >>
        | e = expr; k = sequence' -> k e ] ]
    ;
    binding:
      [ LEFTA
        [ `ANTIQUOT ("binding"|"list" as n) s ->
            <:binding< $anti:mk_anti ~c:"binding" n s$ >>
        | `ANTIQUOT (""|"anti" as n) s; "="; e = expr ->
            <:binding< $anti:mk_anti ~c:"patt" n s$ = $e$ >>
        | `ANTIQUOT (""|"anti" as n) s -> <:binding< $anti:mk_anti ~c:"binding" n s$ >>
        | b1 = SELF; "and"; b2 = SELF -> <:binding< $b1$ and $b2$ >>
        | b = let_binding -> b
      ] ]
    ;
    let_binding:
      [ [ p = ipatt; e = fun_binding -> <:binding< $p$ = $e$ >> ] ]
    ;
    fun_binding:
      [ RIGHTA
        [ TRY ["("; "type"]; i = a_LIDENT; ")"; e = SELF ->
            <:expr< fun (type $i$) -> $e$ >>
        | p = TRY labeled_ipatt; e = SELF ->
            <:expr< fun $p$ -> $e$ >>
        | bi = cvalue_binding -> bi
      ] ]
    ;
    match_case:
      [ [ "["; l = LIST0 match_case0 SEP "|"; "]" -> Ast.mcOr_of_list l
        | p = ipatt; "->"; e = expr -> <:match_case< $p$ -> $e$ >> ] ]
    ;
    match_case0:
      [ [ `ANTIQUOT ("match_case"|"list" as n) s ->
            <:match_case< $anti:mk_anti ~c:"match_case" n s$ >>
        | `ANTIQUOT (""|"anti" as n) s ->
            <:match_case< $anti:mk_anti ~c:"match_case" n s$ >>
        | `ANTIQUOT (""|"anti" as n) s; "->"; e = expr ->
            <:match_case< $anti:mk_anti ~c:"patt" n s$ -> $e$ >>
        | `ANTIQUOT (""|"anti" as n) s; "when"; w = expr; "->"; e = expr ->
            <:match_case< $anti:mk_anti ~c:"patt" n s$ when $w$ -> $e$ >>
        | p = patt_as_patt_opt; w = opt_when_expr; "->"; e = expr -> <:match_case< $p$ when $w$ -> $e$ >>
      ] ]
    ;
    opt_when_expr:
      [ [ "when"; w = expr -> w
        | -> <:expr<>>
      ] ]
    ;
    patt_as_patt_opt:
      [ [ p1 = patt; "as"; p2 = patt -> <:patt< ($p1$ as $p2$) >>
        | p = patt -> p
      ] ]
    ;
    label_expr_list:
      [ [ b1 = label_expr; ";"; b2 = SELF -> <:rec_binding< $b1$ ; $b2$ >>
        | b1 = label_expr; ";"            -> b1
        | b1 = label_expr                 -> b1
      ] ];
    label_expr:
      [ [ `ANTIQUOT ("rec_binding" as n) s ->
            <:rec_binding< $anti:mk_anti ~c:"rec_binding" n s$ >>
        | `ANTIQUOT (""|"anti" as n) s ->
            <:rec_binding< $anti:mk_anti ~c:"rec_binding" n s$ >>
        | `ANTIQUOT (""|"anti" as n) s; "="; e = expr ->
            <:rec_binding< $anti:mk_anti ~c:"ident" n s$ = $e$ >>
        | `ANTIQUOT ("list" as n) s ->
            <:rec_binding< $anti:mk_anti ~c:"rec_binding" n s$ >>
        | i = label_longident; e = fun_binding -> <:rec_binding< $i$ = $e$ >>
        | i = label_longident ->
            <:rec_binding< $i$ = $lid:lid_of_ident i$ >> ] ]
    ;
    fun_def:
      [ [ TRY ["("; "type"]; i = a_LIDENT; ")";
          e = fun_def_cont_no_when ->
            <:expr< fun (type $i$) -> $e$ >>
        | p = TRY labeled_ipatt; (w, e) = fun_def_cont ->
            <:expr< fun [ $p$ when $w$ -> $e$ ] >> ] ]
    ;
    fun_def_cont:
      [ RIGHTA
        [ TRY ["("; "type"]; i = a_LIDENT; ")";
          e = fun_def_cont_no_when ->
            (<:expr<>>, <:expr< fun (type $i$) -> $e$ >>)
        | p = TRY labeled_ipatt; (w,e) = SELF ->
            (<:expr<>>, <:expr< fun [ $p$ when $w$ -> $e$ ] >>)
        | "when"; w = expr; "->"; e = expr -> (w, e)
        | "->"; e = expr -> (<:expr<>>, e) ] ]
    ;
    fun_def_cont_no_when:
      [ RIGHTA
        [ TRY ["("; "type"]; i = a_LIDENT; ")";
          e = fun_def_cont_no_when -> <:expr< fun (type $i$) -> $e$ >>
        | p = TRY labeled_ipatt; (w,e) = fun_def_cont ->
            <:expr< fun [ $p$ when $w$ -> $e$ ] >>
        | "->"; e = expr -> e ] ]
    ;
    patt:
      [ "attribute"
        [ e = SELF; "[@"; s = a_LIDENT; str = str_items; "]" ->
            Ast.PaAtt _loc s str e ]
      | "|" LEFTA
        [ p1 = SELF; "|"; p2 = SELF -> <:patt< $p1$ | $p2$ >> ]
      | ".." NONA
        [ p1 = SELF; ".."; p2 = SELF -> <:patt< $p1$ .. $p2$ >> ]
      | "apply" LEFTA
        [ p1 = SELF; p2 = SELF -> <:patt< $p1$ $p2$ >>
        | "lazy"; p = SELF -> <:patt< lazy $p$ >>  ]
      | "simple"
        [ `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $anti:mk_anti ~c:"patt" n s$ >>
        | `ANTIQUOT ("tup" as n) s -> <:patt< ($tup:<:patt< $anti:mk_anti ~c:"patt" n s$ >>$) >>
        | `ANTIQUOT ("`bool" as n) s -> <:patt< $id:<:ident< $anti:mk_anti n s$ >>$ >>
        | i = ident -> <:patt< $id:i$ >>
        | s = a_INT -> <:patt< $int:s$ >>
        | s = a_INT32 -> <:patt< $int32:s$ >>
        | s = a_INT64 -> <:patt< $int64:s$ >>
        | s = a_NATIVEINT -> <:patt< $nativeint:s$ >>
        | s = a_FLOAT -> <:patt< $flo:s$ >>
        | s = a_STRING -> <:patt< $str:s$ >>
        | s = a_CHAR -> <:patt< $chr:s$ >>
        | "-"; s = a_INT -> <:patt< $int:neg_string s$ >>
        | "-"; s = a_INT32 -> <:patt< $int32:neg_string s$ >>
        | "-"; s = a_INT64 -> <:patt< $int64:neg_string s$ >>
        | "-"; s = a_NATIVEINT -> <:patt< $nativeint:neg_string s$ >>
        | "-"; s = a_FLOAT -> <:patt< $flo:neg_string s$ >>
        | "["; "]" -> <:patt< [] >>
        | "["; mk_list = sem_patt_for_list; "::"; last = patt; "]" ->
            mk_list last
        | "["; mk_list = sem_patt_for_list; "]" ->
            mk_list <:patt< [] >>
        | "[|"; "|]" -> <:patt< [| $<:patt<>>$ |] >>
        | "[|"; pl = sem_patt; "|]" -> <:patt< [| $pl$ |] >>
        | "{"; pl = label_patt_list; "}" -> <:patt< { $pl$ } >>
        | "("; ")" -> <:patt< () >>
        | "("; "module"; m = a_UIDENT; ")" -> <:patt< (module $m$) >>
        | "("; "module"; m = a_UIDENT; ":"; pt = package_type; ")" ->
            <:patt< ((module $m$) : (module $pt$)) >>
        | "("; p = SELF; ")" -> p
        | "("; p = SELF; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
        | "("; p = SELF; "as"; p2 = SELF; ")" -> <:patt< ($p$ as $p2$) >>
        | "("; p = SELF; ","; pl = comma_patt; ")" -> <:patt< ($p$, $pl$) >>
        | "_" -> <:patt< _ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.patt_tag
        | "`"; s = a_ident -> <:patt< ` $s$ >>
        | "#"; i = type_longident -> <:patt< # $i$ >>
        | `LABEL i; p = SELF -> <:patt< ~ $i$ : $p$ >>
        | "~"; `ANTIQUOT (""|"lid" as n) i; ":"; p = SELF ->
            <:patt< ~ $mk_anti n i$ : $p$ >>
        | "~"; `ANTIQUOT (""|"lid" as n) i -> <:patt< ~ $mk_anti n i$ >>
        | "~"; `LIDENT i -> <:patt< ~ $i$ >>
        (* | i = opt_label; "("; p = patt_tcon; ")" -> *)
            (* <:patt< ? $i$ : ($p$) >> *)
        | `OPTLABEL i; "("; p = patt_tcon; f = eq_expr; ")" -> f i p
        | "?"; `ANTIQUOT (""|"lid" as n) i; ":"; "("; p = patt_tcon; f = eq_expr; ")" ->
            f (mk_anti n i) p
        | "?"; `LIDENT i -> <:patt< ? $i$ >>
        | "?"; `ANTIQUOT (""|"lid" as n) i -> <:patt< ? $mk_anti n i$ >>
        | "?"; "("; p = patt_tcon; ")" ->
            <:patt< ? ($p$) >>
        | "?"; "("; p = patt_tcon; "="; e = expr; ")" ->
            <:patt< ? ($p$ = $e$) >> ] ]
    ;
    comma_patt:
      [ [ p1 = SELF; ","; p2 = SELF -> <:patt< $p1$, $p2$ >>
        | `ANTIQUOT ("list" as n) s -> <:patt< $anti:mk_anti ~c:"patt," n s$ >>
        | p = patt -> p ] ]
    ;
    sem_patt:
      [ LEFTA
        [ p1 = patt; ";"; p2 = SELF -> <:patt< $p1$; $p2$ >>
        | `ANTIQUOT ("list" as n) s -> <:patt< $anti:mk_anti ~c:"patt;" n s$ >>
        | p = patt; ";" -> p
        | p = patt -> p ] ]
    ;
    sem_patt_for_list:
      [ [ p = patt; ";"; pl = SELF -> fun acc -> <:patt< [ $p$ :: $pl acc$ ] >>
        | p = patt; ";" -> fun acc -> <:patt< [ $p$ :: $acc$ ] >>
        | p = patt -> fun acc -> <:patt< [ $p$ :: $acc$ ] >>
      ] ]
    ;
    label_patt_list:
      [ [ p1 = label_patt; ";"; p2 = SELF -> <:patt< $p1$ ; $p2$ >>
        | p1 = label_patt; ";"; "_"       -> <:patt< $p1$ ; _ >>
        | p1 = label_patt; ";"; "_"; ";"  -> <:patt< $p1$ ; _ >>
        | p1 = label_patt; ";"            -> p1
        | p1 = label_patt                 -> p1
      ] ];
    label_patt:
      [ [ `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $anti:mk_anti ~c:"patt" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.patt_tag
        | `ANTIQUOT ("list" as n) s ->
            <:patt< $anti:mk_anti ~c:"patt;" n s$ >>
        | i = label_longident; "="; p = patt -> <:patt< $i$ = $p$ >>
        | i = label_longident -> <:patt< $i$ = $lid:lid_of_ident i$ >>
      ] ]
    ;
    ipatt:
      [ [ "{"; pl = label_ipatt_list; "}" -> <:patt< { $pl$ } >>
        | `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $anti:mk_anti ~c:"patt" n s$ >>
        | `ANTIQUOT ("tup" as n) s ->
            <:patt< ($tup:<:patt< $anti:mk_anti ~c:"patt" n s$ >>$) >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.patt_tag
        | "("; ")" -> <:patt< () >>
        | "("; "module"; m = a_UIDENT; ")" -> <:patt< (module $m$) >>
        | "("; "module"; m = a_UIDENT; ":"; pt = package_type; ")" ->
            <:patt< ((module $m$) : (module $pt$)) >>
        | "("; p = SELF; ")" -> p
        | "("; p = SELF; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
        | "("; p = SELF; "as"; p2 = SELF; ")" -> <:patt< ($p$ as $p2$) >>
        | "("; p = SELF; ","; pl = comma_ipatt; ")" -> <:patt< ($p$, $pl$) >>
        | s = a_LIDENT -> <:patt< $lid:s$ >>
        | "_" -> <:patt< _ >> ] ]
    ;
    labeled_ipatt:
      [ [ p = ipatt -> p ] ]
    ;
    comma_ipatt:
      [ LEFTA
        [ p1 = SELF; ","; p2 = SELF -> <:patt< $p1$, $p2$ >>
        | `ANTIQUOT ("list" as n) s -> <:patt< $anti:mk_anti ~c:"patt," n s$ >>
        | p = ipatt -> p ] ]
    ;
    label_ipatt_list:
      [ [ p1 = label_ipatt; ";"; p2 = SELF -> <:patt< $p1$ ; $p2$ >>
        | p1 = label_ipatt; ";"; "_"       -> <:patt< $p1$ ; _ >>
        | p1 = label_ipatt; ";"; "_"; ";"  -> <:patt< $p1$ ; _ >>
        | p1 = label_ipatt; ";"            -> p1
        | p1 = label_ipatt                 -> p1
      ] ];
    label_ipatt:
      [ [ `ANTIQUOT (""|"pat"|"anti" as n) s ->
            <:patt< $anti:mk_anti ~c:"patt" n s$ >>
        | `ANTIQUOT ("list" as n) s -> <:patt< $anti:mk_anti ~c:"patt;" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.patt_tag
        | i = label_longident; "="; p = ipatt -> <:patt< $i$ = $p$ >>
      ] ]
    ;
    type_declaration:
      [ LEFTA
        [ `ANTIQUOT (""|"typ"|"anti" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctypand" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | t1 = SELF; "and"; t2 = SELF -> <:ctyp< $t1$ and $t2$ >>
        | (n, tpl) = type_ident_and_parameters; tk = opt_eq_ctyp;
          cl = LIST0 constrain -> Ast.TyDcl _loc n tpl tk cl ] ]
    ;
    constrain:
      [ [ "constraint"; t1 = ctyp; "="; t2 = ctyp -> (t1, t2) ] ]
    ;
    opt_eq_ctyp:
      [ [ "="; tk = type_kind -> tk
        | -> <:ctyp<>> ] ]
    ;
    type_kind:
      [ [ t = ctyp -> t ] ]
    ;
    type_ident_and_parameters:
      [ [ i = a_LIDENT; tpl = LIST0 optional_type_parameter -> (i, tpl) ] ]
    ;
    type_longident_and_parameters:
      [ [ i = type_longident; tpl = type_parameters -> tpl <:ctyp< $id:i$ >>
      ] ]
    ;
    type_parameters:
      [ [ t1 = type_parameter; t2 = SELF ->
            fun acc -> t2 <:ctyp< $acc$ $t1$ >>
        | t = type_parameter -> fun acc -> <:ctyp< $acc$ $t$ >>
        | -> fun t -> t
      ] ]
    ;

    type_parameter:
      [ [ `ANTIQUOT (""|"typ"|"anti" as n) s -> <:ctyp< $anti:mk_anti n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | "'"; i = a_ident -> <:ctyp< '$lid:i$ >>
        | "+"; "'"; i = a_ident -> <:ctyp< +'$lid:i$ >>
        | "-"; "'"; i = a_ident -> <:ctyp< -'$lid:i$ >> ] ]
    ;
    optional_type_parameter:
      [ [ `ANTIQUOT (""|"typ"|"anti" as n) s -> <:ctyp< $anti:mk_anti n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | "'"; i = a_ident -> <:ctyp< '$lid:i$ >>
        | "+"; "'"; i = a_ident -> <:ctyp< +'$lid:i$ >>
        | "-"; "'"; i = a_ident -> <:ctyp< -'$lid:i$ >>
        | "+"; "_" -> Ast.TyAnP _loc
        | "-"; "_" -> Ast.TyAnM _loc
        | "_" -> Ast.TyAny _loc

 ] ]
    ;


    ctyp:
      [ "==" LEFTA
        [ t1 = SELF; "=="; t2 = SELF -> <:ctyp< $t1$ == $t2$ >> ]
      | "private" NONA
        [ "private"; t = ctyp LEVEL "alias" -> <:ctyp< private $t$ >> ]
      | "alias" LEFTA
        [ t1 = SELF; "as"; t2 = SELF -> <:ctyp< $t1$ as $t2$ >> ]
      | "forall" LEFTA
        [ "!"; t1 = typevars; "."; t2 = ctyp -> <:ctyp< ! $t1$ . $t2$ >> ]
      | "arrow" RIGHTA
        [ t1 = SELF; "->"; t2 = SELF -> <:ctyp< $t1$ -> $t2$ >> ]
      | "label" NONA
        [ "~"; i = a_LIDENT; ":"; t = SELF -> <:ctyp< ~ $i$ : $t$ >>
        | i = a_LABEL; t =  SELF  -> <:ctyp< ~ $i$ : $t$ >>
        | "?"; i = a_LIDENT; ":"; t = SELF -> <:ctyp< ? $i$ : $t$ >>
        | i = a_OPTLABEL; t = SELF -> <:ctyp< ? $i$ : $t$ >> ]
      | "apply" LEFTA
        [ t1 = SELF; t2 = SELF ->
            let t = <:ctyp< $t1$ $t2$ >> in
            try <:ctyp< $id:Ast.ident_of_ctyp t$ >>
            with [ Invalid_argument _ -> t ] ]
      | "." LEFTA
        [ t1 = SELF; "."; t2 = SELF ->
            try <:ctyp< $id:Ast.ident_of_ctyp t1$.$id:Ast.ident_of_ctyp t2$ >>
            with [ Invalid_argument s -> raise (Stream.Error s) ] ]
      | "attribute"
        [ e = SELF; "[@"; s = a_LIDENT; str = str_items; "]" ->
            Ast.TyAtt _loc s str e ]
      | "simple"
        [ "'"; i = a_ident -> <:ctyp< '$i$ >>
        | "_" -> <:ctyp< _ >>
        | `ANTIQUOT (""|"typ"|"anti" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | `ANTIQUOT ("tup" as n) s ->
            <:ctyp< ($tup:<:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>$) >>
        | `ANTIQUOT ("id" as n) s ->
            <:ctyp< $id:<:ident< $anti:mk_anti ~c:"ident" n s$ >>$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | i = a_LIDENT -> <:ctyp< $lid:i$ >>
        | i = a_UIDENT -> <:ctyp< $uid:i$ >>
        | "("; t = SELF; "*"; tl = star_ctyp; ")" ->
            <:ctyp< ( $t$ * $tl$ ) >>
        | "("; t = SELF; ")" -> t
        | "["; "]" -> <:ctyp< [ ] >>
        | "["; t = constructor_declarations; "]" -> <:ctyp< [ $t$ ] >>
        | "["; "="; rfl = row_field; "]" ->
            <:ctyp< [ = $rfl$ ] >>
        | "["; ">"; "]" -> <:ctyp< [ > $<:ctyp<>>$ ] >>
        | "["; ">"; rfl = row_field; "]" ->
            <:ctyp< [ > $rfl$ ] >>
        | "["; "<"; rfl = row_field; "]" ->
            <:ctyp< [ < $rfl$ ] >>
        | "["; "<"; rfl = row_field; ">"; ntl = name_tags; "]" ->
            <:ctyp< [ < $rfl$ > $ntl$ ] >>
        | "[<"; rfl = row_field; "]" ->
            <:ctyp< [ < $rfl$ ] >>
        | "[<"; rfl = row_field; ">"; ntl = name_tags; "]" ->
            <:ctyp< [ < $rfl$ > $ntl$ ] >>
        | "{"; t = label_declaration_list; "}" -> <:ctyp< { $t$ } >>
        | "#"; i = class_longident -> <:ctyp< # $i$ >>
        | "<"; t = opt_meth_list; ">" -> t
        | "("; "module"; p = package_type; ")" -> <:ctyp< (module $p$) >>
      ] ]
    ;
    star_ctyp:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp*" n s$ >>
        | t1 = SELF; "*"; t2 = SELF ->
            <:ctyp< $t1$ * $t2$ >>
        | t = ctyp -> t
      ] ]
    ;
    constructor_declarations:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp|" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | t1 = SELF; "|"; t2 = SELF ->
            <:ctyp< $t1$ | $t2$ >>
        | s = a_UIDENT; "of"; t = constructor_arg_list ->
            <:ctyp< $uid:s$ of $t$ >>
        | s = a_UIDENT; ":"; t = ctyp ->
            let (tl, rt) = generalized_type_of_type t in
            <:ctyp< $uid:s$ : ($Ast.tyAnd_of_list tl$ -> $rt$) >>
        | s = a_UIDENT ->
          <:ctyp< $uid:s$ >>
      ] ]
    ;
    constructor_declaration:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | s = a_UIDENT; "of"; t = constructor_arg_list ->
            <:ctyp< $uid:s$ of $t$ >>
        | s = a_UIDENT ->
            <:ctyp< $uid:s$ >>
      ] ]
    ;
    constructor_arg_list:
      [ [ `ANTIQUOT ("list" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctypand" n s$ >>
        | t1 = SELF; "and"; t2 = SELF -> <:ctyp< $t1$ and $t2$ >>
        | t = ctyp -> t
      ] ]
    ;
    label_declaration_list:
      [ [ t1 = label_declaration; ";"; t2 = SELF -> <:ctyp< $t1$; $t2$ >>
        | t1 = label_declaration; ";"            -> t1
        | t1 = label_declaration                 -> t1
      ] ]
    ;
    label_declaration:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp;" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | s = a_LIDENT; ":"; t = poly_type ->  <:ctyp< $lid:s$ : $t$ >>
        | s = a_LIDENT; ":"; "mutable"; t = poly_type ->
            <:ctyp< $lid:s$ : mutable $t$ >>
      ] ]
    ;
    a_ident:
      [ [ i = a_LIDENT -> i
        | i = a_UIDENT -> i ] ]
    ;
    ident:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $anti:mk_anti ~c:"ident" n s$ >>
        | i = a_UIDENT -> <:ident< $uid:i$ >>
        | i = a_LIDENT -> <:ident< $lid:i$ >>
        | `ANTIQUOT (""|"id"|"anti"|"list" as n) s; "."; i = SELF ->
            <:ident< $anti:mk_anti ~c:"ident" n s$.$i$ >>
        | i = a_UIDENT; "."; j = SELF -> <:ident< $uid:i$.$j$ >> ] ]
    ;
    module_longident:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $anti:mk_anti ~c:"ident" n s$ >>
        | m = a_UIDENT; "."; l = SELF -> <:ident< $uid:m$.$l$ >>
        | i = a_UIDENT -> <:ident< $uid:i$ >> ] ]
    ;
    module_longident_with_app:
      [ "apply"
        [ i = SELF; j = SELF -> <:ident< $i$ $j$ >> ]
      | "."
        [ i = SELF; "."; j = SELF -> <:ident< $i$.$j$ >> ]
      | "simple"
        [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $anti:mk_anti ~c:"ident" n s$ >>
        | i = a_UIDENT -> <:ident< $uid:i$ >>
        | "("; i = SELF; ")" -> i ] ]
    ;
    module_longident_dot_lparen:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s; "."; "(" ->
            <:ident< $anti:mk_anti ~c:"ident" n s$ >>
        | m = a_UIDENT; "."; l = SELF -> <:ident< $uid:m$.$l$ >>
        | i = a_UIDENT; "."; "(" -> <:ident< $uid:i$ >> ] ]
    ;
    type_longident:
      [ "apply"
        [ i = SELF; j = SELF -> <:ident< $i$ $j$ >> ]
      | "."
        [ i = SELF; "."; j = SELF -> <:ident< $i$.$j$ >> ]
      | "simple"
        [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $anti:mk_anti ~c:"ident" n s$ >>
        | i = a_LIDENT -> <:ident< $lid:i$ >>
        | i = a_UIDENT -> <:ident< $uid:i$ >>
        | "("; i = SELF; ")" -> i ] ]
    ;
    label_longident:
      [ [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $anti:mk_anti ~c:"ident" n s$ >>
        | m = a_UIDENT; "."; l = SELF -> <:ident< $uid:m$.$l$ >>
        | i = a_LIDENT -> <:ident< $lid:i$ >> ] ]
    ;
    class_type_longident:
      [ [ x = type_longident -> x ] ]
    ;
    val_longident:
      [ [ x = ident -> x ] ]
    ;
    class_longident:
      [ [ x = label_longident -> x ] ]
    ;
    class_declaration:
      [ LEFTA
        [ c1 = SELF; "and"; c2 = SELF ->
            <:class_expr< $c1$ and $c2$ >>
        | `ANTIQUOT (""|"cdcl"|"anti"|"list" as n) s ->
            <:class_expr< $anti:mk_anti ~c:"class_expr" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.class_expr_tag
        | ci = class_info_for_class_expr; ce = class_fun_binding ->
            <:class_expr< $ci$ = $ce$ >>
      ] ]
    ;
    class_fun_binding:
      [ [ "="; ce = class_expr -> ce
        | ":"; ct = class_type_plus; "="; ce = class_expr ->
            <:class_expr< ($ce$ : $ct$) >>
        | p = labeled_ipatt; cfb = SELF ->
            <:class_expr< fun $p$ -> $cfb$ >>
      ] ]
    ;
    class_info_for_class_type:
      [ [ mv = opt_virtual; (i, ot) = class_name_and_param ->
            <:class_type< $virtual:mv$ $lid:i$ [ $ot$ ] >>
      ] ]
    ;
    class_info_for_class_expr:
      [ [ mv = opt_virtual; (i, ot) = class_name_and_param ->
            <:class_expr< $virtual:mv$ $lid:i$ [ $ot$ ] >>
      ] ]
    ;
    class_name_and_param:
      [ [ i = a_LIDENT; "["; x = comma_type_parameter; "]" -> (i, x)
        | i = a_LIDENT -> (i, <:ctyp<>>)
      ] ]
    ;
    comma_type_parameter:
      [ [ t1 = SELF; ","; t2 = SELF -> <:ctyp< $t1$, $t2$ >>
        | `ANTIQUOT ("list" as n) s -> <:ctyp< $anti:mk_anti ~c:"ctyp," n s$ >>
        | t = type_parameter -> t
      ] ]
    ;
    opt_comma_ctyp:
      [ [ "["; x = comma_ctyp; "]" -> x
        | -> <:ctyp<>>
      ] ]
    ;
    comma_ctyp:
      [ [ t1 = SELF; ","; t2 = SELF -> <:ctyp< $t1$, $t2$ >>
        | `ANTIQUOT ("list" as n) s -> <:ctyp< $anti:mk_anti ~c:"ctyp," n s$ >>
        | t = ctyp -> t
      ] ]
    ;
    class_fun_def:
      [ [ p = labeled_ipatt; ce = SELF -> <:class_expr< fun $p$ -> $ce$ >>
        | "->"; ce = class_expr -> ce ] ]
    ;
    class_expr:
      [ "top"
        [ "fun"; p = labeled_ipatt; ce = class_fun_def ->
            <:class_expr< fun $p$ -> $ce$ >>
        | "let"; rf = opt_rec; bi = binding; "in"; ce = SELF ->
            <:class_expr< let $rec:rf$ $bi$ in $ce$ >> ]
      | "apply" NONA
        [ ce = SELF; e = expr LEVEL "label" ->
            <:class_expr< $ce$ $e$ >> ]
      | "simple"
        [ `ANTIQUOT (""|"cexp"|"anti" as n) s ->
            <:class_expr< $anti:mk_anti ~c:"class_expr" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.class_expr_tag
        | ce = class_longident_and_param -> ce
        | "object"; csp = opt_class_self_patt; cst = class_structure; "end" ->
            <:class_expr< object ($csp$) $cst$ end >>
        | "("; ce = SELF; ":"; ct = class_type; ")" ->
            <:class_expr< ($ce$ : $ct$) >>
        | "("; ce = SELF; ")" -> ce ] ]
    ;
    class_longident_and_param:
      [ [ ci = class_longident; "["; t = comma_ctyp; "]" ->
          <:class_expr< $id:ci$ [ $t$ ] >>
        | ci = class_longident -> <:class_expr< $id:ci$ >>
      ] ]
    ;
    class_structure:
      [ [ `ANTIQUOT (""|"cst"|"anti"|"list" as n) s ->
            <:class_str_item< $anti:mk_anti ~c:"class_str_item" n s$ >>
        | `ANTIQUOT (""|"cst"|"anti"|"list" as n) s; semi; cst = SELF ->
            <:class_str_item< $anti:mk_anti ~c:"class_str_item" n s$; $cst$ >>
        | l = LIST0 [ cst = class_str_item; semi -> cst ] -> Ast.crSem_of_list l
      ] ]
    ;
    opt_class_self_patt:
      [ [ "("; p = patt; ")" -> p
        | "("; p = patt; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
        | -> <:patt<>> ] ]
    ;
    class_str_item:
      [ LEFTA
        [ `ANTIQUOT (""|"cst"|"anti"|"list" as n) s ->
            <:class_str_item< $anti:mk_anti ~c:"class_str_item" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.class_str_item_tag
        | "inherit"; o = opt_override; ce = class_expr; pb = opt_as_lident ->
            <:class_str_item< inherit $override:o$ $ce$ as $pb$ >>
        | o = value_val_opt_override; mf = opt_mutable; lab = label; e = cvalue_binding ->
            <:class_str_item< value $override:o$ $mutable:mf$ $lab$ = $e$ >>
        | o = value_val_opt_override; mf = opt_mutable; "virtual"; l = label; ":"; t = poly_type ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< value virtual $mutable:mf$ $l$ : $t$ >>
        | o = value_val_opt_override; "virtual"; mf = opt_mutable; l = label; ":"; t = poly_type ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< value virtual $mutable:mf$ $l$ : $t$ >>
        | o = method_opt_override; "virtual"; pf = opt_private; l = label; ":"; t = poly_type ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< method virtual $private:pf$ $l$ : $t$ >>
        | o = method_opt_override; pf = opt_private; l = label; topt = opt_polyt; e = fun_binding ->
            <:class_str_item< method $override:o$ $private:pf$ $l$ : $topt$ = $e$ >>
        | o = method_opt_override; pf = opt_private; "virtual"; l = label; ":"; t = poly_type ->
            if o <> <:override_flag<>> then
              raise (Stream.Error "override (!) is incompatible with virtual")
            else
              <:class_str_item< method virtual $private:pf$ $l$ : $t$ >>
        | type_constraint; t1 = ctyp; "="; t2 = ctyp ->
            <:class_str_item< type $t1$ = $t2$ >>
        | "initializer"; se = expr -> <:class_str_item< initializer $se$ >> ] ]
    ;
    method_opt_override:
      [ [ "method"; "!" -> <:override_flag< ! >>
        | "method"; `ANTIQUOT (("!"|"override"|"anti") as n) s -> Ast.OvAnt (mk_anti n s)
        | "method" -> <:override_flag<>>
      ] ]
    ;
    value_val_opt_override:
      [ [ value_val; "!" -> <:override_flag< ! >>
        | value_val; `ANTIQUOT (("!"|"override"|"anti") as n) s -> Ast.OvAnt (mk_anti n s)
        | value_val -> <:override_flag<>>
      ] ]
    ;
    opt_as_lident:
      [ [ "as"; i = a_LIDENT -> i
        | -> ""
      ] ]
    ;
    opt_polyt:
      [ [ ":"; t = poly_type -> t
        | -> <:ctyp<>> ] ]
    ;
    cvalue_binding:
      [ [ "="; e = expr -> e
        | ":"; "type"; t1 = unquoted_typevars; "." ; t2 = ctyp ; "="; e = expr ->
        let u = Ast.TyTypePol _loc t1 t2 in
        <:expr< ($e$ : $u$) >>
        | ":"; t = poly_type; "="; e = expr -> <:expr< ($e$ : $t$) >>
        | ":"; t = poly_type; ":>"; t2 = ctyp; "="; e = expr ->
            match t with
            [ <:ctyp< ! $_$ . $_$ >> -> raise (Stream.Error "unexpected polytype here")
            | _ -> <:expr< ($e$ : $t$ :> $t2$) >> ]
        | ":>"; t = ctyp; "="; e = expr -> <:expr< ($e$ :> $t$) >> ] ]
    ;
    label:
      [ [ i = a_LIDENT -> i ] ]
    ;
    class_type:
      [ [ `ANTIQUOT (""|"ctyp"|"anti" as n) s ->
            <:class_type< $anti:mk_anti ~c:"class_type" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.class_type_tag
        | ct = class_type_longident_and_param -> ct
        | "object"; cst = opt_class_self_type; csg = class_signature; "end" ->
            <:class_type< object ($cst$) $csg$ end >> ] ]
    ;
    class_type_longident_and_param:
      [ [ i = class_type_longident; "["; t = comma_ctyp; "]" ->
            <:class_type< $id:i$ [ $t$ ] >>
        | i = class_type_longident -> <:class_type< $id:i$ >> ] ]
    ;
    class_type_plus:
      [ [ "["; t = ctyp; "]"; "->"; ct = SELF -> <:class_type< [ $t$ ] -> $ct$ >>
        | ct = class_type -> ct ] ]
    ;
    opt_class_self_type:
      [ [ "("; t = ctyp; ")" -> t
        | -> <:ctyp<>> ] ]
    ;
    class_signature:
      [ [ `ANTIQUOT (""|"csg"|"anti"|"list" as n) s ->
            <:class_sig_item< $anti:mk_anti ~c:"class_sig_item" n s$ >>
        | `ANTIQUOT (""|"csg"|"anti"|"list" as n) s; semi; csg = SELF ->
            <:class_sig_item< $anti:mk_anti ~c:"class_sig_item" n s$; $csg$ >>
        | l = LIST0 [ csg = class_sig_item; semi -> csg ] -> Ast.cgSem_of_list l
      ] ]
    ;
    class_sig_item:
      [ [ `ANTIQUOT (""|"csg"|"anti"|"list" as n) s ->
            <:class_sig_item< $anti:mk_anti ~c:"class_sig_item" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.class_sig_item_tag
        | "inherit"; cs = class_type -> <:class_sig_item< inherit $cs$ >>
        | value_val; mf = opt_mutable; mv = opt_virtual;
          l = label; ":"; t = ctyp ->
            <:class_sig_item< value $mutable:mf$ $virtual:mv$ $l$ : $t$ >>
        | "method"; "virtual"; pf = opt_private; l = label; ":"; t = poly_type ->
            <:class_sig_item< method virtual $private:pf$ $l$ : $t$ >>
        | "method"; pf = opt_private; l = label; ":"; t = poly_type ->
            <:class_sig_item< method $private:pf$ $l$ : $t$ >>
        | "method"; pf = opt_private; "virtual"; l = label; ":"; t = poly_type ->
            <:class_sig_item< method virtual $private:pf$ $l$ : $t$ >>
        | type_constraint; t1 = ctyp; "="; t2 = ctyp ->
            <:class_sig_item< type $t1$ = $t2$ >> ] ]
    ;
    type_constraint:
      [ [ "type" | "constraint" -> () ] ]
    ;
    class_description:
      [ [ cd1 = SELF; "and"; cd2 = SELF -> <:class_type< $cd1$ and $cd2$ >>
        | `ANTIQUOT (""|"typ"|"anti"|"list" as n) s ->
            <:class_type< $anti:mk_anti ~c:"class_type" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.class_type_tag
        | ci = class_info_for_class_type; ":"; ct = class_type_plus -> <:class_type< $ci$ : $ct$ >>
      ] ]
    ;
    class_type_declaration:
      [ LEFTA
        [ cd1 = SELF; "and"; cd2 = SELF -> <:class_type< $cd1$ and $cd2$ >>
        | `ANTIQUOT (""|"typ"|"anti"|"list" as n) s ->
            <:class_type< $anti:mk_anti ~c:"class_type" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.class_type_tag
        | ci = class_info_for_class_type; "="; ct = class_type -> <:class_type< $ci$ = $ct$ >>
      ] ]
    ;
    field_expr_list:
      [ [ b1 = field_expr; ";"; b2 = SELF -> <:rec_binding< $b1$ ; $b2$ >>
        | b1 = field_expr; ";"            -> b1
        | b1 = field_expr                 -> b1
      ] ];
    field_expr:
      [ [ `ANTIQUOT (""|"bi"|"anti" as n) s ->
            <:rec_binding< $anti:mk_anti ~c:"rec_binding" n s$ >>
        | `ANTIQUOT ("list" as n) s ->
            <:rec_binding< $anti:mk_anti ~c:"rec_binding" n s$ >>
        | l = label; "="; e = expr LEVEL "top" -> <:rec_binding< $lid:l$ = $e$ >> ] ]
    ;
    meth_list:
      [ [ m = meth_decl; ";"; (ml, v) = SELF  -> (<:ctyp< $m$; $ml$ >>, v)
        | m = meth_decl; ";"; v = opt_dot_dot -> (m, v)
        | m = meth_decl; v = opt_dot_dot      -> (m, v)
      ] ]
    ;
    meth_decl:
      [ [ `ANTIQUOT (""|"typ" as n) s        -> <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | `ANTIQUOT ("list" as n) s          -> <:ctyp< $anti:mk_anti ~c:"ctyp;" n s$ >>
        | `QUOTATION x                       -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | lab = a_LIDENT; ":"; t = poly_type -> <:ctyp< $lid:lab$ : $t$ >> ] ]
    ;
    opt_meth_list:
      [ [ (ml, v) = meth_list -> <:ctyp< < $ml$ $..:v$ > >>
        | v = opt_dot_dot     -> <:ctyp< < $..:v$ > >>
      ] ]
    ;
    poly_type:
      [ [ t = ctyp -> t ] ]
    ;
    package_type:
      [ [ p = module_type -> p ] ]
    ;
    typevars:
      [ LEFTA
        [ t1 = SELF; t2 = SELF -> <:ctyp< $t1$ $t2$ >>
        | `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | "'"; i = a_ident -> <:ctyp< '$lid:i$ >>
      ] ]
    ;
    unquoted_typevars:
      [ LEFTA
        [ t1 = SELF; t2 = SELF -> <:ctyp< $t1$ $t2$ >>
        | `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | `QUOTATION x -> Quotation.expand _loc x Quotation.DynAst.ctyp_tag
        | i = a_ident -> <:ctyp< $lid:i$ >>
      ] ]
    ;

    row_field:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | `ANTIQUOT ("list" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp|" n s$ >>
        | t1 = SELF; "|"; t2 = SELF -> <:ctyp< $t1$ | $t2$ >>
        | "`"; i = a_ident -> <:ctyp< `$i$ >>
        | "`"; i = a_ident; "of"; "&"; t = amp_ctyp -> <:ctyp< `$i$ of & $t$ >>
        | "`"; i = a_ident; "of"; t = amp_ctyp -> <:ctyp< `$i$ of $t$ >>
        | t = ctyp -> t ] ]
    ;
    amp_ctyp:
      [ [ t1 = SELF; "&"; t2 = SELF -> <:ctyp< $t1$ & $t2$ >>
        | `ANTIQUOT ("list" as n) s -> <:ctyp< $anti:mk_anti ~c:"ctyp&" n s$ >>
        | t = ctyp -> t
      ] ]
    ;
    name_tags:
      [ [ `ANTIQUOT (""|"typ" as n) s ->
            <:ctyp< $anti:mk_anti ~c:"ctyp" n s$ >>
        | t1 = SELF; t2 = SELF -> <:ctyp< $t1$ $t2$ >>
        | "`"; i = a_ident -> <:ctyp< `$i$ >>
      ] ]
    ;
    eq_expr:
      [ [ "="; e = expr -> fun i p -> <:patt< ? $i$ : ($p$ = $e$) >>
        | -> fun i p -> <:patt< ? $i$ : ($p$) >> ] ]
    ;
    patt_tcon:
      [ [ p = patt; ":"; t = ctyp -> <:patt< ($p$ : $t$) >>
        | p = patt -> p ] ]
    ;
    ipatt:
      [ [ `LABEL i; p = SELF -> <:patt< ~ $i$ : $p$ >>
        | "~"; `ANTIQUOT (""|"lid" as n) i; ":"; p = SELF ->
            <:patt< ~ $mk_anti n i$ : $p$ >>
        | "~"; `ANTIQUOT (""|"lid" as n) i -> <:patt< ~ $mk_anti n i$ >>
        | "~"; `LIDENT i -> <:patt< ~ $i$ >>
        (* | i = opt_label; "("; p = ipatt_tcon; ")" ->
            <:patt< ? $i$ : ($p$) >>
        | i = opt_label; "("; p = ipatt_tcon; "="; e = expr; ")" ->
            <:patt< ? $i$ : ($p$ = $e$) >>                             *)
        | `OPTLABEL i; "("; p = ipatt_tcon; f = eq_expr; ")" -> f i p
        | "?"; `ANTIQUOT (""|"lid" as n) i; ":"; "("; p = ipatt_tcon;
          f = eq_expr; ")" -> f (mk_anti n i) p
        | "?"; `LIDENT i -> <:patt< ? $i$ >>
        | "?"; `ANTIQUOT (""|"lid" as n) i -> <:patt< ? $mk_anti n i$ >>
        | "?"; "("; p = ipatt_tcon; ")" ->
            <:patt< ? ($p$) >>
        | "?"; "("; p = ipatt_tcon; "="; e = expr; ")" ->
            <:patt< ? ($p$ = $e$) >> ] ]
    ;
    ipatt_tcon:
      [ [ p = ipatt; ":"; t = ctyp -> <:patt< ($p$ : $t$) >>
        | p = ipatt -> p ] ]
    ;
    direction_flag:
      [ [ "to" -> <:direction_flag< to >>
        | "downto" -> <:direction_flag< downto >>
        | `ANTIQUOT ("to"|"anti" as n) s -> Ast.DiAnt (mk_anti n s) ] ]
    ;
    opt_private:
      [ [ "private" -> <:private_flag< private >>
        | `ANTIQUOT ("private"|"anti" as n) s -> Ast.PrAnt (mk_anti n s)
        | -> <:private_flag<>>
      ] ]
    ;
    opt_mutable:
      [ [ "mutable" -> <:mutable_flag< mutable >>
        | `ANTIQUOT ("mutable"|"anti" as n) s -> Ast.MuAnt (mk_anti n s)
        | -> <:mutable_flag<>>
      ] ]
    ;
    opt_virtual:
      [ [ "virtual" -> <:virtual_flag< virtual >>
        | `ANTIQUOT ("virtual"|"anti" as n) s -> Ast.ViAnt (mk_anti n s)
        | -> <:virtual_flag<>>
      ] ]
    ;
    opt_dot_dot:
      [ [ ".." -> <:row_var_flag< .. >>
        | `ANTIQUOT (".."|"anti" as n) s -> Ast.RvAnt (mk_anti n s)
        | -> <:row_var_flag<>>
      ] ]
    ;
    opt_rec:
      [ [ "rec" -> <:rec_flag< rec >>
        | `ANTIQUOT ("rec"|"anti" as n) s -> Ast.ReAnt (mk_anti n s)
        | -> <:rec_flag<>>
      ] ]
    ;
    opt_override:
      [ [ "!" -> <:override_flag< ! >>
        | `ANTIQUOT (("!"|"override"|"anti") as n) s -> Ast.OvAnt (mk_anti n s)
        | -> <:override_flag<>>
      ] ]
    ;
    opt_expr:
      [ [ e = expr -> e
        | -> <:expr<>>
      ] ]
    ;
    interf:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ([ <:sig_item< # $n$ $dp$ >> ], stopped_at _loc)
        | si = sig_item; semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
        | `EOI -> ([], None) ] ]
    ;
    sig_items:
      [ [ `ANTIQUOT (""|"sigi"|"anti"|"list" as n) s ->
            <:sig_item< $anti:mk_anti n ~c:"sig_item" s$ >>
        | `ANTIQUOT (""|"sigi"|"anti"|"list" as n) s; semi; sg = SELF ->
            <:sig_item< $anti:mk_anti n ~c:"sig_item" s$; $sg$ >>
        | l = LIST0 [ sg = sig_item; semi -> sg ] -> Ast.sgSem_of_list l
      ] ]
    ;
    implem:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ([ <:str_item< # $n$ $dp$ >> ], stopped_at _loc)
        | si = str_item; semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
        | `EOI -> ([], None)
      ] ]
    ;
    str_items:
      [ [ `ANTIQUOT (""|"stri"|"anti"|"list" as n) s ->
            <:str_item< $anti:mk_anti n ~c:"str_item" s$ >>
        | `ANTIQUOT (""|"stri"|"anti"|"list" as n) s; semi; st = SELF ->
            <:str_item< $anti:mk_anti n ~c:"str_item" s$; $st$ >>
        | l = LIST0 [ st = str_item; semi -> st ] -> Ast.stSem_of_list l
      ] ]
    ;
    top_phrase:
      [ [ ph = phrase -> Some ph
        | `EOI -> None
      ] ]
    ;
    use_file:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            ([ <:str_item< # $n$ $dp$ >> ], stopped_at _loc)
        | si = str_item; semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
        | `EOI -> ([], None)
      ] ]
    ;
    phrase:
      [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
            <:str_item< # $n$ $dp$ >>
        | st = str_item; semi -> st
      ] ]
    ;
    a_INT:
      [ [ `ANTIQUOT (""|"int"|"`int" as n) s -> mk_anti n s
        | `INT _ s -> s ] ]
    ;
    a_INT32:
      [ [ `ANTIQUOT (""|"int32"|"`int32" as n) s -> mk_anti n s
        | `INT32 _ s -> s ] ]
    ;
    a_INT64:
      [ [ `ANTIQUOT (""|"int64"|"`int64" as n) s -> mk_anti n s
        | `INT64 _ s -> s ] ]
    ;
    a_NATIVEINT:
      [ [ `ANTIQUOT (""|"nativeint"|"`nativeint" as n) s -> mk_anti n s
        | `NATIVEINT _ s -> s ] ]
    ;
    a_FLOAT:
      [ [ `ANTIQUOT (""|"flo"|"`flo" as n) s -> mk_anti n s
        | `FLOAT _ s -> s ] ]
    ;
    a_CHAR:
      [ [ `ANTIQUOT (""|"chr"|"`chr" as n) s -> mk_anti n s
        | `CHAR _ s -> s ] ]
    ;
    a_UIDENT:
      [ [ `ANTIQUOT (""|"uid" as n) s -> mk_anti n s
        | `UIDENT s -> s ] ]
    ;
    a_LIDENT:
      [ [ `ANTIQUOT (""|"lid" as n) s -> mk_anti n s
        | `LIDENT s -> s ] ]
    ;
    a_LABEL:
      [ [ "~"; `ANTIQUOT ("" as n) s; ":" -> mk_anti n s
        | `LABEL s -> s ] ]
    ;
    a_OPTLABEL:
      [ [ "?"; `ANTIQUOT ("" as n) s; ":" -> mk_anti n s
        | `OPTLABEL s -> s ] ]
    ;
    a_STRING:
      [ [ `ANTIQUOT (""|"str"|"`str" as n) s -> mk_anti n s
        | `STRING _ s -> s ] ]
    ;
    string_list:
      [ [ `ANTIQUOT (""|"str_list") s -> Ast.LAnt (mk_anti "str_list" s)
        | `STRING _ x; xs = string_list -> Ast.LCons x xs
        | `STRING _ x -> Ast.LCons x Ast.LNil ] ]
    ;
    value_let:
      [ [ "value" -> () ] ]
    ;
    value_val:
      [ [ "value" -> () ] ]
    ;
    semi:
      [ [ ";" -> () ] ]
    ;
    expr_quot:
      [ [ e1 = expr; ","; e2 = comma_expr -> <:expr< $e1$, $e2$ >>
        | e1 = expr; ";"; e2 = sem_expr -> <:expr< $e1$; $e2$ >>
        | e = expr -> e
        | -> <:expr<>>
      ] ]
    ;
    patt_quot:
      [ [ x = patt; ","; y = comma_patt -> <:patt< $x$, $y$ >>
        | x = patt; ";"; y = sem_patt -> <:patt< $x$; $y$ >>
        | x = patt; "="; y = patt ->
            let i =
              match x with
              [ <:patt@loc< $anti:s$ >> -> <:ident@loc< $anti:s$ >>
              | p -> Ast.ident_of_patt p ]
            in
            <:patt< $i$ = $y$ >>
        | x = patt -> x
        | -> <:patt<>>
      ] ]
    ;
    ctyp_quot:
      [ [ x = more_ctyp; ","; y = comma_ctyp -> <:ctyp< $x$, $y$ >>
        | x = more_ctyp; ";"; y = label_declaration_list -> <:ctyp< $x$; $y$ >>
        | x = more_ctyp; "|"; y = constructor_declarations -> <:ctyp< $x$ | $y$ >>
        | x = more_ctyp; "of"; y = constructor_arg_list -> <:ctyp< $x$ of $y$ >>
        | x = more_ctyp; "of"; y = constructor_arg_list; "|"; z = constructor_declarations ->
            <:ctyp< $ <:ctyp< $x$ of $y$ >> $ | $z$ >>
        | x = more_ctyp; "of"; "&"; y = amp_ctyp -> <:ctyp< $x$ of & $y$ >>
        | x = more_ctyp; "of"; "&"; y = amp_ctyp; "|"; z = row_field ->
            <:ctyp< $ <:ctyp< $x$ of & $y$ >> $ | $z$ >>
        | x = more_ctyp; ":"; y = more_ctyp -> <:ctyp< $x$ : $y$ >>
        | x = more_ctyp; ":"; y = more_ctyp; ";"; z = label_declaration_list ->
            <:ctyp< $ <:ctyp< $x$ : $y$ >> $ ; $z$ >>
        | x = more_ctyp; "*"; y = star_ctyp -> <:ctyp< $x$ * $y$ >>
        | x = more_ctyp; "&"; y = amp_ctyp -> <:ctyp< $x$ & $y$ >>
        | x = more_ctyp; "and"; y = constructor_arg_list -> <:ctyp< $x$ and $y$ >>
        | x = more_ctyp -> x
        | -> <:ctyp<>>
      ] ]
    ;
    more_ctyp:
      [ [ "mutable"; x = SELF -> <:ctyp< mutable $x$ >>
        | "`"; x = a_ident -> <:ctyp< `$x$ >>
        | x = ctyp -> x
        | x = type_parameter -> x
      ] ]
    ;
    str_item_quot:
      [ [ "#"; n = a_LIDENT; dp = opt_expr -> <:str_item< # $n$ $dp$ >>
        | st1 = str_item; semi; st2 = SELF ->
            match st2 with
            [ <:str_item<>> -> st1
            | _ -> <:str_item< $st1$; $st2$ >> ]
        | st = str_item -> st
        | -> <:str_item<>> ] ]
    ;
    sig_item_quot:
      [ [ "#"; n = a_LIDENT; dp = opt_expr -> <:sig_item< # $n$ $dp$ >>
        | sg1 = sig_item; semi; sg2 = SELF ->
            match sg2 with
            [ <:sig_item<>> -> sg1
            | _ -> <:sig_item< $sg1$; $sg2$ >> ]
        | sg = sig_item -> sg
        | -> <:sig_item<>> ] ]
    ;
    module_type_quot:
      [ [ x = module_type -> x
        | -> <:module_type<>>
      ] ]
    ;
    module_expr_quot:
      [ [ x = module_expr -> x
        | -> <:module_expr<>>
      ] ]
    ;
    match_case_quot:
      [ [ x = LIST0 match_case0 SEP "|" -> <:match_case< $list:x$ >>
        | -> <:match_case<>> ] ]
    ;
    binding_quot:
      [ [ x = binding -> x
        | -> <:binding<>>
      ] ]
    ;
    rec_binding_quot:
      [ [ x = label_expr_list -> x
        | -> <:rec_binding<>> ] ]
    ;
    module_binding_quot:
      [ [ b1 = SELF; "and"; b2 = SELF ->
            <:module_binding< $b1$ and $b2$ >>
        | `ANTIQUOT ("module_binding"|"anti" as n) s ->
            <:module_binding< $anti:mk_anti ~c:"module_binding" n s$ >>
        | `ANTIQUOT ("" as n) s -> <:module_binding< $anti:mk_anti ~c:"module_binding" n s$ >>
        | `ANTIQUOT ("" as n) m; ":"; mt = module_type ->
            <:module_binding< $mk_anti n m$ : $mt$ >>
        | `ANTIQUOT ("" as n) m; ":"; mt = module_type; "="; me = module_expr ->
            <:module_binding< $mk_anti n m$ : $mt$ = $me$ >>
        | m = a_UIDENT; ":"; mt = module_type -> <:module_binding< $m$ : $mt$ >>
        | m = a_UIDENT; ":"; mt = module_type; "="; me = module_expr ->
            <:module_binding< $m$ : $mt$ = $me$ >>
        | -> <:module_binding<>>
      ] ]
    ;
    ident_quot:
      [ "apply"
        [ i = SELF; j = SELF -> <:ident< $i$ $j$ >> ]
      | "."
        [ i = SELF; "."; j = SELF -> <:ident< $i$.$j$ >> ]
      | "simple"
        [ `ANTIQUOT (""|"id"|"anti"|"list" as n) s ->
            <:ident< $anti:mk_anti ~c:"ident" n s$ >>
        | i = a_UIDENT -> <:ident< $uid:i$ >>
        | i = a_LIDENT -> <:ident< $lid:i$ >>
        | `ANTIQUOT (""|"id"|"anti"|"list" as n) s; "."; i = SELF ->
            <:ident< $anti:mk_anti ~c:"ident" n s$.$i$ >>
        | "("; i = SELF; ")" -> i
      ] ]
    ;
    class_expr_quot:
      [ [ ce1 = SELF; "and"; ce2 = SELF -> <:class_expr< $ce1$ and $ce2$ >>
        | ce1 = SELF; "="; ce2 = SELF -> <:class_expr< $ce1$ = $ce2$ >>
        | "virtual"; (i, ot) = class_name_and_param ->
            <:class_expr< virtual $lid:i$ [ $ot$ ] >>
        | `ANTIQUOT ("virtual" as n) s; i = ident; ot = opt_comma_ctyp ->
            let anti = Ast.ViAnt (mk_anti ~c:"class_expr" n s) in
            <:class_expr< $virtual:anti$ $id:i$ [ $ot$ ] >>
        | x = class_expr -> x
        | -> <:class_expr<>>
      ] ]
    ;
    class_type_quot:
      [ [ ct1 = SELF; "and"; ct2 = SELF -> <:class_type< $ct1$ and $ct2$ >>
        | ct1 = SELF; "="; ct2 = SELF -> <:class_type< $ct1$ = $ct2$ >>
        | ct1 = SELF; ":"; ct2 = SELF -> <:class_type< $ct1$ : $ct2$ >>
        | "virtual"; (i, ot) = class_name_and_param ->
            <:class_type< virtual $lid:i$ [ $ot$ ] >>
        | `ANTIQUOT ("virtual" as n) s; i = ident; ot = opt_comma_ctyp ->
            let anti = Ast.ViAnt (mk_anti ~c:"class_type" n s) in
            <:class_type< $virtual:anti$ $id:i$ [ $ot$ ] >>
        | x = class_type_plus -> x
        | -> <:class_type<>>
      ] ]
    ;
    class_str_item_quot:
      [ [ x1 = class_str_item; semi; x2 = SELF ->
          match x2 with
          [ <:class_str_item<>> -> x1
          | _ -> <:class_str_item< $x1$; $x2$ >> ]
        | x = class_str_item -> x
        | -> <:class_str_item<>> ] ]
    ;
    class_sig_item_quot:
      [ [ x1 = class_sig_item; semi; x2 = SELF ->
          match x2 with
          [ <:class_sig_item<>> -> x1
          | _ -> <:class_sig_item< $x1$; $x2$ >> ]
        | x = class_sig_item -> x
        | -> <:class_sig_item<>> ] ]
    ;
    with_constr_quot:
      [ [ x = with_constr -> x
        | -> <:with_constr<>> ] ]
    ;
    rec_flag_quot: [ [ x = opt_rec -> x ] ];
    direction_flag_quot: [ [ x = direction_flag -> x ] ];
    mutable_flag_quot: [ [ x = opt_mutable -> x ] ];
    private_flag_quot: [ [ x = opt_private -> x ] ];
    virtual_flag_quot: [ [ x = opt_virtual -> x ] ];
    row_var_flag_quot: [ [ x = opt_dot_dot -> x ] ];
    override_flag_quot: [ [ x = opt_override -> x ] ];
    patt_eoi:
      [ [ x = patt; `EOI -> x ] ]
    ;
    expr_eoi:
      [ [ x = expr; `EOI -> x ] ]
    ;
  END in apply ();

end;

let module M = Register.OCamlSyntaxExtension Id Make in ();
