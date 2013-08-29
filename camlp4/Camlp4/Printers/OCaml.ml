(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

open Format;

module Id = struct
  value name = "Camlp4.Printers.OCaml";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax;

  type sep = format unit formatter unit;
  type fun_binding = [= `patt of Ast.patt | `newtype of string ];

  value pp = fprintf;
  value cut f = fprintf f "@ ";

  value list' elt sep sep' f =
    let rec loop =
      fun
      [ [] -> ()
      | [x::xs] -> do { pp f sep ; elt f x; pp f sep'; loop xs } ] in
    fun
    [ [] -> ()
    | [x] -> do { elt f x; pp f sep' }
    | [x::xs] -> do { elt f x; pp f sep'; loop xs } ];

  value list elt sep f =
    let rec loop =
      fun
      [ [] -> ()
      | [x::xs] -> do { pp f sep ; elt f x; loop xs } ] in
    fun
    [ [] -> ()
    | [x] -> elt f x
    | [x::xs] -> do { elt f x; loop xs } ];

  value rec list_of_meta_list =
    fun
    [ Ast.LNil -> []
    | Ast.LCons x xs -> [x :: list_of_meta_list xs]
    | Ast.LAnt _ -> assert False ];

  value meta_list elt sep f mxs =
    let xs = list_of_meta_list mxs in
    list elt sep f xs;

  module CommentFilter = Struct.CommentFilter.Make Token;
  value comment_filter = CommentFilter.mk ();
  CommentFilter.define (Gram.get_filter ()) comment_filter;

  module StringSet = Set.Make String;

  value infix_lidents = ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"];

  value is_infix =
    let first_chars = ['='; '<'; '>'; '|'; '&'; '$'; '@'; '^'; '+'; '-'; '*'; '/'; '%'; '\\']
    and infixes =
      List.fold_right StringSet.add infix_lidents StringSet.empty
    in fun s -> (StringSet.mem s infixes
                 || (s <> "" && List.mem s.[0] first_chars));

  value is_keyword =
    let keywords = (* without infix_lidents *)
      List.fold_right StringSet.add
        ["and"; "as"; "assert"; "begin"; "class"; "constraint"; "do";
         "done"; "downto"; "else"; "end"; "exception"; "external"; "false";
         "for"; "fun"; "function"; "functor"; "if"; "in"; "include";
         "inherit"; "initializer"; "lazy"; "let"; "match"; "method"; "module";
         "mutable"; "new"; "object";  "of";  "open"; "parser";  "private";  "rec"; "sig";
         "struct";  "then";  "to";  "true";  "try";  "type";  "val"; "virtual";
         "when"; "while"; "with"] StringSet.empty
      in fun s -> StringSet.mem s keywords;

  module Lexer = Struct.Lexer.Make Token;
  let module M = ErrorHandler.Register Lexer.Error in ();
  open Sig;
  value lexer s =
    Lexer.from_string ~quotations:Camlp4_config.quotations.val Loc.ghost s;
  value lex_string str =
    try match lexer str with parser
        [: `(tok, _); `(EOI, _) :] -> tok
    with
    [ Stream.Failure | Stream.Error _ ->
        failwith (sprintf
          "Cannot print %S this string contains more than one token" str)
    | Lexer.Error.E exn ->
        failwith (sprintf
          "Cannot print %S this identifier does not respect OCaml lexing rules (%s)"
          str (Lexer.Error.to_string exn)) ];

  (* This is to be sure character literals are always escaped. *)
  value ocaml_char x = Char.escaped (Struct.Token.Eval.char x);

  value rec get_expr_args a al =
    match a with
    [ <:expr< $a1$ $a2$ >> -> get_expr_args a1 [a2 :: al]
    | _ -> (a, al) ];

  value rec get_patt_args a al =
    match a with
    [ <:patt< $a1$ $a2$ >> -> get_patt_args a1 [a2 :: al]
    | _ -> (a, al) ];

  value rec get_ctyp_args a al =
    match a with
    [ <:ctyp< $a1$ $a2$ >> -> get_ctyp_args a1 [a2 :: al]
    | _ -> (a, al) ];

  value is_irrefut_patt = Ast.is_irrefut_patt;

  value rec expr_fun_args =
    fun
    [ <:expr< fun $p$ -> $e$ >> as ge ->
        if is_irrefut_patt p then
          let (pl, e) = expr_fun_args e in
          ([`patt p :: pl], e)
        else ([], ge)
    | <:expr< fun (type $i$) -> $e$ >> ->
        let (pl, e) = expr_fun_args e in
        ([`newtype i :: pl], e)
    | ge -> ([], ge) ];

  value rec class_expr_fun_args =
    fun
    [ <:class_expr< fun $p$ -> $ce$ >> as ge ->
        if is_irrefut_patt p then
          let (pl, ce) = class_expr_fun_args ce in
          ([p :: pl], ce)
        else ([], ge)
    | ge -> ([], ge) ];

  value rec do_print_comments_before loc f =
    parser
    [ [: ` (comm, comm_loc) when Loc.strictly_before comm_loc loc; s :] ->
          let () = f comm comm_loc in
          do_print_comments_before loc f s
    | [: :] -> () ];

  class printer ?curry_constr:(init_curry_constr = False) ?(comments = True) () =
  object (o)

    (** pipe means we are under a match case (try, function) *)
    value pipe = False;
    value semi = False;

    method under_pipe = {< pipe = True >};
    method under_semi = {< semi = True >};
    method reset_semi = {< semi = False >};
    method reset =      {< pipe = False; semi = False >};

    value semisep : sep = ";;";
    value no_semisep : sep = ""; (* used to mark where ";;" should not occur *)
    value mode = if comments then `comments else `no_comments;
    value curry_constr = init_curry_constr;
    value var_conversion = False;

    method andsep : sep = "@]@ @[<2>and@ ";
    method value_val = "val";
    method value_let = "let";

    method semisep = semisep;
    method set_semisep s = {< semisep = s >};
    method set_comments b = {< mode = if b then `comments else `no_comments >};
    method set_loc_and_comments = {< mode = `loc_and_comments >};
    method set_curry_constr b = {< curry_constr = b >};

    method print_comments_before loc f =
      match mode with
      [ `comments ->
          do_print_comments_before loc (fun c _ -> pp f "%s@ " c)
            (CommentFilter.take_stream comment_filter)
      | `loc_and_comments ->
          let () = pp f "(*loc: %a*)@ " Loc.dump loc in
          do_print_comments_before loc
            (fun s -> pp f "%s(*comm_loc: %a*)@ " s Loc.dump)
            (CommentFilter.take_stream comment_filter)
      | _ -> () ];

    method var f =
      fun
      [ ""   -> pp f "$lid:\"\"$"
      | "[]" -> pp f "[]"
      | "()" -> pp f "()"
      | " True"  -> pp f "True"
      | " False" -> pp f "False"
      | v ->
          match (var_conversion, v) with
          [ (True, "val") -> pp f "contents"
          | (True, "True") -> pp f "true"
          | (True, "False") -> pp f "false"
          | _ ->
            match lex_string v with
            [ (LIDENT s | UIDENT s | ESCAPED_IDENT s) when is_keyword s ->
                pp f "%s__" s
            | (LIDENT s | ESCAPED_IDENT s) when List.mem s infix_lidents ->
                pp f "( %s )" s
            | SYMBOL s ->
                pp f "( %s )" s
            | LIDENT s | UIDENT s | ESCAPED_IDENT s ->
                pp_print_string f s
            | tok -> failwith (sprintf
                      "Bad token used as an identifier: %s"
                      (Token.to_string tok)) ] ] ];

    method type_params f =
      fun
      [ [] -> ()
      | [x] -> pp f "%a@ " o#ctyp x
      | l -> pp f "@[<1>(%a)@]@ " (list o#ctyp ",@ ") l ];

    method class_params f =
      fun
      [ <:ctyp< $t1$, $t2$ >> ->
          pp f "@[<1>%a,@ %a@]" o#class_params t1 o#class_params t2
      | x -> o#ctyp f x ];

    method override_flag f =
      fun
      [ Ast.OvOverride -> pp f "!"
      | Ast.OvNil -> ()
      | Ast.OvAnt s -> o#anti f s ];

    method mutable_flag f = fun
      [ Ast.MuMutable -> pp f "mutable@ "
      | Ast.MuNil -> ()
      | Ast.MuAnt s -> o#anti f s ];

    method rec_flag f = fun
      [ Ast.ReRecursive -> pp f "rec@ "
      | Ast.ReNil -> ()
      | Ast.ReAnt s -> o#anti f s ];

    method virtual_flag f = fun
      [ Ast.ViVirtual -> pp f "virtual@ "
      | Ast.ViNil -> ()
      | Ast.ViAnt s -> o#anti f s ];

    method private_flag f = fun
      [ Ast.PrPrivate -> pp f "private@ "
      | Ast.PrNil -> ()
      | Ast.PrAnt s -> o#anti f s ];

    method anti f s = pp f "$%s$" s;

    method seq f =
      fun
      [ <:expr< $e1$; $e2$ >> ->
          pp f "%a;@ %a" o#under_semi#seq e1 o#seq e2
      | <:expr< do { $e$ } >> ->
          o#seq f e
      | e -> o#expr f e ];

          (* FIXME when the Format module will fixed.
                  pp_print_if_newline f ();
                  pp_print_string f "| "; *)
    method match_case f =
      fun
      [ <:match_case@_loc<>> ->
          pp f "@[<2>@ _ ->@ %a@]" o#raise_match_failure _loc
      | a -> o#match_case_aux f a ];

    method match_case_aux f =
      fun
      [ <:match_case<>> -> ()
      | <:match_case< $anti:s$ >> -> o#anti f s
      | <:match_case< $a1$ | $a2$ >> ->
          pp f "%a%a" o#match_case_aux a1 o#match_case_aux a2
      | <:match_case< $p$ -> $e$ >> ->
          pp f "@ | @[<2>%a@ ->@ %a@]" o#patt p o#under_pipe#expr e
      | <:match_case< $p$ when $w$ -> $e$ >> ->
          pp f "@ | @[<2>%a@ when@ %a@ ->@ %a@]"
            o#patt p o#under_pipe#expr w o#under_pipe#expr e ];

    method fun_binding f =
      fun
      [ `patt p -> o#simple_patt f p
      | `newtype i -> pp f "(type %s)" i ];

    method binding f bi =
      let () = o#node f bi Ast.loc_of_binding in
      match bi with
      [ <:binding<>> -> ()
      | <:binding< $b1$ and $b2$ >> ->
          do { o#binding f b1; pp f o#andsep; o#binding f b2 }
      | <:binding< $p$ = $e$ >> ->
          let (pl, e') =
            match p with
            [ <:patt< ($_$ : $_$) >> -> ([], e)
            | _ -> expr_fun_args e ] in
          match (p, e') with
          [ (<:patt< $lid:_$ >>, <:expr< ($e'$ : $t$) >>) ->
              pp f "%a :@ %a =@ %a"
                (list o#fun_binding "@ ") [`patt p::pl] o#ctyp t o#expr e'
          | (<:patt< $lid:_$ >>, _) ->
              pp f "%a @[<0>%a=@]@ %a" o#simple_patt
                p (list' o#fun_binding "" "@ ") pl o#expr e'
          | _ ->
              pp f "%a =@ %a" o#simple_patt p o#expr e ]
      | <:binding< $anti:s$ >> -> o#anti f s ];

    method record_binding f bi =
      let () = o#node f bi Ast.loc_of_rec_binding in
      match bi with
      [ <:rec_binding<>> -> ()
      | <:rec_binding< $i$ = $e$ >> ->
          pp f "@ @[<2>%a =@ %a@];" o#var_ident i o#expr e
      | <:rec_binding< $b1$ ; $b2$ >> ->
          do { o#under_semi#record_binding f b1;
               o#under_semi#record_binding f b2 }
      | <:rec_binding< $anti:s$ >> -> o#anti f s ];

    method mk_patt_list =
      fun
      [ <:patt< [$p1$ :: $p2$] >> ->
          let (pl, c) = o#mk_patt_list p2 in
          ([p1 :: pl], c)
      | <:patt< [] >> -> ([], None)
      | p -> ([], Some p) ];

    method mk_expr_list =
      fun
      [ <:expr< [$e1$ :: $e2$] >> ->
          let (el, c) = o#mk_expr_list e2 in
          ([e1 :: el], c)
      | <:expr< [] >> -> ([], None)
      | e -> ([], Some e) ];

    method expr_list f =
      fun
      [ []  -> pp f "[]"
      | [e] -> pp f "[ %a ]" o#under_semi#expr e
      | el  -> pp f "@[<2>[ %a@] ]" (list o#under_semi#expr ";@ ") el ];

    method expr_list_cons simple f e =
      let (el, c) = o#mk_expr_list e in
      match c with
      [ None -> o#expr_list f el
      | Some x ->
          (if simple then pp f "@[<2>(%a)@]" else pp f "@[<2>%a@]")
            (list o#under_semi#dot_expr " ::@ ") (el @ [x]) ];

    method patt_expr_fun_args f (p, e) =
      let (pl, e) = expr_fun_args e
      in pp f "%a@ ->@ %a" (list o#fun_binding "@ ") [p::pl] o#expr e;

    method patt_class_expr_fun_args f (p, ce) =
      let (pl, ce) = class_expr_fun_args ce
      in pp f "%a =@]@ %a" (list o#simple_patt "@ ") [p::pl] o#class_expr ce;

    method constrain f (t1, t2) =
      pp f "@[<2>constraint@ %a =@ %a@]" o#ctyp t1 o#ctyp t2;

    method sum_type f t =
      match Ast.list_of_ctyp t [] with
      [ [] -> ()
      | ts ->
          pp f "@[<hv0>| %a@]" (list o#constructor_declaration "@ | ") ts ];

    method private constructor_declaration f t =
      match t with
      [ <:ctyp< $t1$ : $t2$ -> $t3$ >> -> pp f "@[<2>%a :@ @[<2>%a@ ->@ %a@]@]" o#ctyp t1 o#constructor_type t2 o#ctyp t3
      | t -> o#ctyp f t ];

    method string f = pp f "%s";
    method quoted_string f = pp f "%S";

    method numeric f num suff =
      if num.[0] = '-' then pp f "(%s%s)" num suff else pp f "%s%s" num suff;

    method module_expr_get_functor_args accu =
      fun
      [ <:module_expr< functor ($s$ : $mt$) -> $me$ >> ->
          o#module_expr_get_functor_args [(s, mt)::accu] me
      | <:module_expr< ($me$ : $mt$) >> -> (List.rev accu, me, Some mt)
      | me -> (List.rev accu, me, None) ];

    method functor_args f = list o#functor_arg "@ " f;

    method functor_arg f (s, mt) =
      pp f "@[<2>(%a :@ %a)@]" o#var s o#module_type mt;

    method module_rec_binding f =
      fun
      [ <:module_binding<>> -> ()
      | <:module_binding< $s$ : $mt$ = $me$ >> ->
           pp f "@[<2>%a :@ %a =@ %a@]"
             o#var s o#module_type mt o#module_expr me
      | <:module_binding< $s$ : $mt$ >> ->
           pp f "@[<2>%a :@ %a@]" o#var s o#module_type mt
      | <:module_binding< $mb1$ and $mb2$ >> ->
          do { o#module_rec_binding f mb1;
               pp f o#andsep;
               o#module_rec_binding f mb2 }
      | <:module_binding< $anti:s$ >> -> o#anti f s ];

    method class_declaration f =
      fun
      [ <:class_expr< ( $ce$ : $ct$ ) >> ->
          pp f "%a :@ %a" o#class_expr ce o#class_type ct
      | ce -> o#class_expr f ce ];

    method raise_match_failure f _loc =
      let n = Loc.file_name _loc in
      let l = Loc.start_line _loc in
      let c = Loc.start_off _loc - Loc.start_bol _loc in
      o#expr f <:expr< raise (Match_failure $`str:n$ $`int:l$ $`int:c$) >>;

    method node : ! 'a . formatter -> 'a -> ('a -> Loc.t) -> unit =
      fun f node loc_of_node ->
        o#print_comments_before (loc_of_node node) f;

    method ident f i =
    let () = o#node f i Ast.loc_of_ident in
    match i with
    [ <:ident< $i1$.$i2$ >> -> pp f "%a.@,%a" o#ident i1 o#ident i2
    | <:ident< $i1$ $i2$ >> -> pp f "%a@,(%a)" o#ident i1 o#ident i2
    | <:ident< $anti:s$ >> -> o#anti f s
    | <:ident< $lid:s$ >> | <:ident< $uid:s$ >> -> o#var f s ];

    method private var_ident = {< var_conversion = True >}#ident;

    method expr f e =
    let () = o#node f e Ast.loc_of_expr in
    match e with
    [ ((<:expr< let $rec:_$ $_$ in $_$ >> |
        <:expr< let module $_$ = $_$ in $_$ >>) as e) when semi ->
        pp f "(%a)" o#reset#expr e
    | ((<:expr< match $_$ with [ $_$ ] >> |
        <:expr< try $_$ with [ $_$ ] >> |
        <:expr< fun [ $_$ ] >>) as e) when pipe || semi ->
        pp f "(%a)" o#reset#expr e

    | <:expr< - $x$ >> ->
        (* If you want to remove the space take care of - !r *)
        pp f "@[<2>-@ %a@]" o#dot_expr x
    | <:expr< -. $x$ >> ->
        pp f "@[<2>-.@ %a@]" o#dot_expr x (* same note as above *)
    | <:expr< [$_$ :: $_$] >> -> o#expr_list_cons False f e
    | <:expr@_loc< $lid:n$ $x$ $y$ >> when is_infix n ->
        pp f "@[<2>%a@ %s@ %a@]" o#apply_expr x n o#apply_expr y
    | <:expr< $x$ $y$ >> ->
        let (a, al) = get_expr_args x [y] in
        if (not curry_constr) && Ast.is_expr_constructor a then
          match al with
          [ [ <:expr< ($tup:_$) >> ] ->
              pp f "@[<2>%a@ (%a)@]" o#apply_expr x o#expr y
          | [_] -> pp f "@[<2>%a@ %a@]" o#apply_expr x o#apply_expr y
          | al ->
              pp f "@[<2>%a@ (%a)@]" o#apply_expr a
                 (* The #apply_expr below may put too much parens.
                    However using #expr would be wrong: PR#5056. *)
                 (list o#under_pipe#apply_expr ",@ ") al ]
        else pp f "@[<2>%a@]" (list o#apply_expr "@ ") [a::al]
    | <:expr< $e1$.val := $e2$ >> ->
        pp f "@[<2>%a :=@ %a@]" o#dot_expr e1 o#expr e2
    | <:expr< $e1$ := $e2$ >> ->
        pp f "@[<2>%a@ <-@ %a@]" o#dot_expr e1 o#expr e2
    | <:expr@loc< fun [] >> ->
        pp f "@[<2>fun@ _@ ->@ %a@]" o#raise_match_failure loc
    | <:expr< fun $p$ -> $e$ >> when is_irrefut_patt p ->
        pp f "@[<2>fun@ %a@]" o#patt_expr_fun_args (`patt p, e)
    | <:expr< fun (type $i$) -> $e$ >> ->
        pp f "@[<2>fun@ %a@]" o#patt_expr_fun_args (`newtype i, e)
    | <:expr< fun [ $a$ ] >> ->
        pp f "@[<hv0>function%a@]" o#match_case a
    | <:expr< if $e1$ then $e2$ else $e3$ >> ->
        pp f "@[<hv0>@[<2>if@ %a@]@ @[<2>then@ %a@]@ @[<2>else@ %a@]@]"
           o#expr e1 o#under_semi#expr e2 o#under_semi#expr e3
    | <:expr< lazy $e$ >> -> pp f "@[<2>lazy@ %a@]" o#simple_expr e
    | <:expr< let $rec:r$ $bi$ in $e$ >> ->
        match e with
        [ <:expr< let $rec:_$ $_$ in $_$ >> ->
            pp f "@[<0>@[<2>let %a%a in@]@ %a@]"
              o#rec_flag r o#binding bi o#reset_semi#expr e
        | _ ->
            pp f "@[<hv0>@[<2>let %a%a@]@ @[<hv2>in@ %a@]@]"
              o#rec_flag r o#binding bi o#reset_semi#expr e ]
    | Ast.ExOpI _loc i ov e ->
    (* | <:expr< let open $i$ in $e$ >> -> *)
        pp f "@[<2>let open%a %a@]@ @[<2>in@ %a@]"
          o#override_flag ov o#ident i o#reset_semi#expr e
    | <:expr< match $e$ with [ $a$ ] >> ->
        pp f "@[<hv0>@[<hv0>@[<2>match %a@]@ with@]%a@]"
          o#expr e o#match_case a
    | <:expr< try $e$ with [ $a$ ] >> ->
        pp f "@[<0>@[<hv2>try@ %a@]@ @[<0>with%a@]@]"
          o#expr e o#match_case a
    | <:expr< assert False >> -> pp f "@[<2>assert@ false@]"
    | <:expr< assert $e$ >> -> pp f "@[<2>assert@ %a@]" o#dot_expr e
    | <:expr< let module $s$ = $me$ in $e$ >> ->
          pp f "@[<2>let module %a =@ %a@]@ @[<2>in@ %a@]" o#var s o#module_expr me o#reset_semi#expr e
    | <:expr< object $cst$ end >> ->
        pp f "@[<hv0>@[<hv2>object@ %a@]@ end@]" o#class_str_item cst
    | <:expr< object ($p$ : $t$) $cst$ end >> ->
        pp f "@[<hv0>@[<hv2>object @[<1>(%a :@ %a)@]@ %a@]@ end@]"
          o#patt p o#ctyp t o#class_str_item cst
    | <:expr< object ($p$) $cst$ end >> ->
        pp f "@[<hv0>@[<hv2>object @[<2>(%a)@]@ %a@]@ end@]"
          o#patt p o#class_str_item cst
    | e -> o#apply_expr f e ];

    method apply_expr f e =
    let () = o#node f e Ast.loc_of_expr in
    match e with
    [ <:expr< new $i$ >> -> pp f "@[<2>new@ %a@]" o#ident i
    | e -> o#dot_expr f e ];

    method dot_expr f e =
    let () = o#node f e Ast.loc_of_expr in
    match e with
    [ <:expr< $e$.val >> -> pp f "@[<2>!@,%a@]" o#simple_expr e
    | <:expr< $e1$ . $e2$ >> -> pp f "@[<2>%a.@,%a@]" o#dot_expr e1 o#dot_expr e2
    | <:expr< $e1$ .( $e2$ ) >> ->
        pp f "@[<2>%a.@,(%a)@]" o#dot_expr e1 o#expr e2
    | <:expr< $e1$ .[ $e2$ ] >> ->
        pp f "%a.@[<1>[@,%a@]@,]" o#dot_expr e1 o#expr e2
    | <:expr< $e$ # $s$ >> -> pp f "@[<2>%a#@,%s@]" o#dot_expr e s
    | e -> o#simple_expr f e ];

    method simple_expr f e =
    let () = o#node f e Ast.loc_of_expr in
    match e with
    [ <:expr<>> -> ()
    | <:expr< do { $e$ } >> ->
        pp f "@[<hv1>(%a)@]" o#seq e
    | <:expr< [$_$ :: $_$] >> -> o#expr_list_cons True f e
    | <:expr< ( $tup:e$ ) >> ->
        pp f "@[<1>(%a)@]" o#expr e
    | <:expr< [| $e$ |] >> ->
        pp f "@[<0>@[<2>[|@ %a@]@ |]@]" o#under_semi#expr e
    | <:expr< ($e$ :> $t$) >> ->
        pp f "@[<2>(%a :>@ %a)@]" o#expr e o#ctyp t
    | <:expr< ($e$ : $t1$ :> $t2$) >> ->
        pp f "@[<2>(%a :@ %a :>@ %a)@]" o#expr e o#ctyp t1 o#ctyp t2
    | <:expr< ($e$ : $t$) >> ->
        pp f "@[<2>(%a :@ %a)@]" o#expr e o#ctyp t
    | <:expr< $anti:s$ >> -> o#anti f s
    | <:expr< for $s$ = $e1$ $to:df$ $e2$ do { $e3$ } >> ->
        pp f "@[<hv0>@[<hv2>@[<2>for %a =@ %a@ %a@ %a@ do@]@ %a@]@ done@]"
          o#var s o#expr e1 o#direction_flag df o#expr e2 o#seq e3
    | <:expr< $int:s$ >> -> o#numeric f s ""
    | <:expr< $nativeint:s$ >> -> o#numeric f s "n"
    | <:expr< $int64:s$ >> -> o#numeric f s "L"
    | <:expr< $int32:s$ >> -> o#numeric f s "l"
    | <:expr< $flo:s$ >> -> o#numeric f s ""
    | <:expr< $chr:s$ >> -> pp f "'%s'" (ocaml_char s)
    | <:expr< $id:i$ >> -> o#var_ident f i
    | <:expr< { $b$ } >> ->
        pp f "@[<hv0>@[<hv2>{%a@]@ }@]" o#record_binding b
    | <:expr< { ($e$) with $b$ } >> ->
        pp f "@[<hv0>@[<hv2>{@ (%a)@ with%a@]@ }@]"
          o#expr e o#record_binding b
    | <:expr< $str:s$ >> -> pp f "\"%s\"" s
    | <:expr< while $e1$ do { $e2$ } >> ->
        pp f "@[<2>while@ %a@ do@ %a@ done@]" o#expr e1 o#seq e2
    | <:expr< ~ $s$ >> -> pp f "~%s" s
    | <:expr< ~ $s$ : $e$ >> -> pp f "@[<2>~%s:@ %a@]" s o#dot_expr e
    | <:expr< ? $s$ >> -> pp f "?%s" s
    | <:expr< ? $s$ : $e$ >> -> pp f "@[<2>?%s:@ %a@]" s o#dot_expr e
    | <:expr< ` $lid:s$ >> -> pp f "`%a" o#var s
    | <:expr< {< $b$ >} >> ->
        pp f "@[<hv0>@[<hv2>{<%a@]@ >}@]" o#record_binding b
    | <:expr< $e1$, $e2$ >> ->
        pp f "%a,@ %a" o#simple_expr e1 o#simple_expr e2
    | <:expr< $e1$; $e2$ >> ->
        pp f "%a;@ %a" o#under_semi#expr e1 o#expr e2
    | <:expr< (module $me$ : $mt$) >> ->
        pp f "@[<hv0>@[<hv2>(module %a : %a@])@]"
           o#module_expr me o#module_type mt
    | <:expr< (module $me$) >> ->
        pp f "@[<hv0>@[<hv2>(module %a@])@]" o#module_expr me
    | Ast.ExAtt _loc s str e ->
        pp f "((%a)[@@%s %a])" o#expr e s o#str_item str
    | <:expr< $_$ $_$ >> | <:expr< $_$ . $_$ >> | <:expr< $_$ . ( $_$ ) >> |
      <:expr< $_$ . [ $_$ ] >> | <:expr< $_$ := $_$ >> |
      <:expr< $_$ # $_$ >> |
      <:expr< fun [ $_$ ] >> | <:expr< fun (type $_$) -> $_$ >> | <:expr< match $_$ with [ $_$ ] >> |
      <:expr< try $_$ with [ $_$ ] >> |
      <:expr< if $_$ then $_$ else $_$ >> |
      <:expr< let $rec:_$ $_$ in $_$ >> |
      <:expr< let module $_$ = $_$ in $_$ >> |
      (* <:expr< let open $_$ in $_$ >> *)Ast.ExOpI _ _ _ _ |
      <:expr< assert $_$ >> | <:expr< assert False >> |
      <:expr< lazy $_$ >> | <:expr< new $_$ >> |
      <:expr< object ($_$) $_$ end >> ->
        pp f "(%a)" o#reset#expr e ];

    method direction_flag f b =
      match b with
      [ Ast.DiTo -> pp_print_string f "to"
      | Ast.DiDownto -> pp_print_string f "downto"
      | Ast.DiAnt s -> o#anti f s ];

    method patt f p =
    let () = o#node f p Ast.loc_of_patt in match p with
    [ <:patt< ( $p1$ as $p2$ ) >> -> pp f "@[<1>(%a@ as@ %a)@]" o#patt p1 o#patt p2
    | <:patt< $i$ = $p$ >> -> pp f "@[<2>%a =@ %a@]" o#var_ident i o#patt p
    | <:patt< $p1$; $p2$ >> -> pp f "%a;@ %a" o#patt p1 o#patt p2
    | p -> o#patt1 f p ];

    method patt1 f = fun
    [ <:patt< $p1$ | $p2$ >> -> pp f "@[<2>%a@ |@ %a@]" o#patt1 p1 o#patt2 p2
    | p -> o#patt2 f p ];

    method patt2 f = fun
    [ (* <:patt< ( $tup:p$ ) >> -> pp f "@[<1>(%a)@]" o#patt3 p
    | *) p -> o#patt3 f p ];

    method patt3 f = fun
    [ <:patt< $p1$ .. $p2$ >> -> pp f "@[<2>%a@ ..@ %a@]" o#patt3 p1 o#patt4 p2
    | <:patt< $p1$, $p2$ >> -> pp f "%a,@ %a" o#patt3 p1 o#patt3 p2
    | p -> o#patt4 f p ];

    method patt4 f = fun
    [ <:patt< [$_$ :: $_$] >> as p ->
        let (pl, c) = o#mk_patt_list p in
        match c with
        [ None -> pp f "@[<2>[@ %a@]@ ]" (list o#patt ";@ ") pl
        | Some x -> pp f "@[<2>%a@]" (list o#patt5 " ::@ ") (pl @ [x]) ]
    | p -> o#patt5 f p ];

    method patt5 f = fun
    [ <:patt< [$_$ :: $_$] >> as p -> o#simple_patt f p
    | <:patt< lazy $p$ >> ->
        pp f "@[<2>lazy %a@]" o#simple_patt p
    | <:patt< $x$ $y$ >> ->
        let (a, al) = get_patt_args x [y] in
        if not (Ast.is_patt_constructor a) then
          Format.eprintf "WARNING: strange pattern application of a non constructor@."
        else if curry_constr then
          pp f "@[<2>%a@]" (list o#simple_patt "@ ") [a::al]
        else
          match al with
          [ [ <:patt< ($tup:_$) >> ] ->
              pp f "@[<2>%a@ (%a)@]" o#simple_patt x o#patt y
          | [_] -> pp f "@[<2>%a@ %a@]" o#patt5 x o#simple_patt y
          | al -> pp f "@[<2>%a@ (%a)@]" o#patt5 a
                       (list o#simple_patt ",@ ") al ]
    | p -> o#simple_patt f p ];

    method simple_patt f p =
    let () = o#node f p Ast.loc_of_patt in
    match p with
    [ <:patt<>> -> ()
    | <:patt< $id:i$ >> -> o#var_ident f i
    | <:patt< $anti:s$ >> -> o#anti f s
    | <:patt< _ >> -> pp f "_"
    | <:patt< ( module $m$ ) >> -> pp f "(module %s)" m
    | <:patt< ( $tup:p$ ) >> -> pp f "@[<1>(%a)@]" o#patt3 p
    | <:patt< { $p$ } >> -> pp f "@[<hv2>{@ %a@]@ }" o#patt p
    | <:patt< $str:s$ >> -> pp f "\"%s\"" s
    | <:patt< ( $p$ : $t$ ) >> -> pp f "@[<1>(%a :@ %a)@]" o#patt p o#ctyp t
    | <:patt< $nativeint:s$ >> -> o#numeric f s "n"
    | <:patt< $int64:s$ >> -> o#numeric f s "L"
    | <:patt< $int32:s$ >> -> o#numeric f s "l"
    | <:patt< $int:s$ >> -> o#numeric f s ""
    | <:patt< $flo:s$ >> -> o#numeric f s ""
    | <:patt< $chr:s$ >> -> pp f "'%s'" (ocaml_char s)
    | <:patt< ~ $s$ >> -> pp f "~%s" s
    | <:patt< ` $uid:s$ >> -> pp f "`%a" o#var s
    | <:patt< # $i$ >> -> pp f "@[<2>#%a@]" o#ident i
    | <:patt< [| $p$ |] >> -> pp f "@[<2>[|@ %a@]@ |]" o#patt p
    | <:patt< ~ $s$ : ($p$) >> -> pp f "@[<2>~%s:@ (%a)@]" s o#patt p
    | <:patt< ? $s$ >> -> pp f "?%s" s
    | <:patt< ?($p$) >> ->
          pp f "@[<2>?(%a)@]" o#patt_tycon p
    | <:patt< ? $s$ : ($p$) >> ->
          pp f "@[<2>?%s:@,@[<1>(%a)@]@]" s o#patt_tycon p
    | <:patt< ?($p$ = $e$) >> ->
          pp f "@[<2>?(%a =@ %a)@]" o#patt_tycon p o#expr e
    | <:patt< ? $s$ : ($p$ = $e$) >> ->
          pp f "@[<2>?%s:@,@[<1>(%a =@ %a)@]@]" s o#patt_tycon p o#expr e
    | <:patt< $_$ $_$ >> | <:patt< ($_$ as $_$) >> | <:patt< $_$ | $_$ >> |
      <:patt< $_$ .. $_$ >> | <:patt< $_$, $_$ >> |
      <:patt< $_$; $_$ >> | <:patt< $_$ = $_$ >> | <:patt< lazy $_$ >> as p ->
          pp f "@[<1>(%a)@]" o#patt p
    | Ast.PaAtt _loc s str e ->
        pp f "((%a)[@@%s %a])" o#patt e s o#str_item str
    ];

    method patt_tycon f =
      fun
      [ <:patt< ( $p$ : $t$ ) >> -> pp f "%a :@ %a" o#patt p o#ctyp t
      | p -> o#patt f p ];

    method simple_ctyp f t =
    let () = o#node f t Ast.loc_of_ctyp in
    match t with
    [ <:ctyp< $id:i$ >> -> o#ident f i
    | <:ctyp< $anti:s$ >> -> o#anti f s
    | <:ctyp< _ >> -> pp f "_"
    | Ast.TyAnP _ -> pp f "+_"
    | Ast.TyAnM _ -> pp f "-_"
    | <:ctyp< ~ $s$ : $t$ >> -> pp f "@[<2>%s:@ %a@]" s o#simple_ctyp t
    | <:ctyp< ? $s$ : $t$ >> -> pp f "@[<2>?%s:@ %a@]" s o#simple_ctyp t
    | <:ctyp< < > >> -> pp f "< >"
    | <:ctyp< < .. > >> -> pp f "< .. >"
    | <:ctyp< < $t$ .. > >> -> pp f "@[<0>@[<2><@ %a;@ ..@]@ >@]" o#ctyp t
    | <:ctyp< < $t$ > >> -> pp f "@[<0>@[<2><@ %a@]@ >@]" o#ctyp t
    | <:ctyp< '$s$ >> -> pp f "'%a" o#var s
    | <:ctyp< { $t$ } >> -> pp f "@[<2>{@ %a@]@ }" o#ctyp t
    | <:ctyp< [ $t$ ] >> -> pp f "@[<0>%a@]" o#sum_type t
    | <:ctyp< ( $tup:t$ ) >> -> pp f "@[<1>(%a)@]" o#ctyp t
    | <:ctyp< (module $mt$) >> -> pp f "@[<2>(module@ %a@])" o#module_type mt
    | <:ctyp< [ = $t$ ] >> -> pp f "@[<2>[@ %a@]@ ]" o#sum_type t
    | <:ctyp< [ < $t$ ] >> -> pp f "@[<2>[<@ %a@]@,]" o#sum_type t
    | <:ctyp< [ < $t1$ > $t2$ ] >> ->
        let (a, al) = get_ctyp_args t2 [] in
        pp f "@[<2>[<@ %a@ >@ %a@]@ ]" o#sum_type t1
          (list o#simple_ctyp "@ ") [a::al]
    | <:ctyp< [ > $t$ ] >> -> pp f "@[<2>[>@ %a@]@,]" o#sum_type t
    | <:ctyp< # $i$ >> -> pp f "@[<2>#%a@]" o#ident i
    | <:ctyp< `$s$ >> -> pp f "`%a" o#var s
    | <:ctyp< $t1$ * $t2$ >> -> pp f "%a *@ %a" o#simple_ctyp t1 o#simple_ctyp t2
    | Ast.TyAtt _loc s str e ->
        pp f "((%a)[@@%s %a])" o#ctyp e s o#str_item str
    | <:ctyp<>> -> assert False
    | t -> pp f "@[<1>(%a)@]" o#ctyp t ];

    method ctyp f t =
    let () = o#node f t Ast.loc_of_ctyp in
    match t with
    [ <:ctyp< $t1$ as $t2$ >> -> pp f "@[<2>%a@ as@ %a@]" o#simple_ctyp t1 o#simple_ctyp t2
    | <:ctyp< $t1$ -> $t2$ >> -> pp f "@[<2>%a@ ->@ %a@]" o#ctyp1 t1 o#ctyp t2
    | <:ctyp< +'$s$ >> -> pp f "+'%a" o#var s
    | <:ctyp< -'$s$ >> -> pp f "-'%a" o#var s
    | <:ctyp< $t1$ | $t2$ >> -> pp f "%a@ | %a" o#ctyp t1 o#ctyp t2
    | <:ctyp< $t1$ : mutable $t2$ >> ->
        pp f "@[mutable@ %a :@ %a@]" o#ctyp t1 o#ctyp t2
    | <:ctyp< $t1$ : $t2$ >> -> pp f "@[<2>%a :@ %a@]" o#ctyp t1 o#ctyp t2
    | <:ctyp< $t1$; $t2$ >> -> pp f "%a;@ %a" o#ctyp t1 o#ctyp t2
    | <:ctyp< $t$ of $<:ctyp<>>$ >> -> o#ctyp f t
    | <:ctyp< $t1$ of $t2$ >> ->
        pp f "@[<h>%a@ @[<3>of@ %a@]@]" o#ctyp t1 o#constructor_type t2
    | <:ctyp< $t1$ of & $t2$ >> ->
        pp f "@[<h>%a@ @[<3>of &@ %a@]@]" o#ctyp t1 o#constructor_type t2
    | <:ctyp< $t1$ and $t2$ >> -> pp f "%a@ and %a" o#ctyp t1 o#ctyp t2
    | <:ctyp< mutable $t$ >> -> pp f "@[<2>mutable@ %a@]" o#ctyp t
    | <:ctyp< $t1$ & $t2$ >> -> pp f "%a@ &@ %a" o#ctyp t1 o#ctyp t2
    | <:ctyp< $t1$ == $t2$ >> ->
        pp f "@[<2>%a =@ %a@]" o#simple_ctyp t1 o#ctyp t2
    | Ast.TyDcl _ tn tp te cl -> do {
        pp f "@[<2>%a%a@]" o#type_params tp o#var tn;
        match te with
        [ <:ctyp<>> -> ()
        | _ -> pp f " =@ %a" o#ctyp te ];
        if cl <> [] then pp f "@ %a" (list o#constrain "@ ") cl else ();
      }
    | t -> o#ctyp1 f t ];

    method ctyp1 f = fun
    [ <:ctyp< $t1$ $t2$ >> ->
        match get_ctyp_args t1 [t2] with
        [ (_, [_]) -> pp f "@[<2>%a@ %a@]" o#simple_ctyp t2 o#simple_ctyp t1
        | (a, al) -> pp f "@[<2>(%a)@ %a@]" (list o#ctyp ",@ ") al o#simple_ctyp a ]
    | <:ctyp< ! $t1$ . $t2$ >> ->
        let (a, al) = get_ctyp_args t1 [] in
        pp f "@[<2>%a.@ %a@]" (list o#ctyp "@ ") [a::al] o#ctyp t2
    | Ast.TyTypePol (_,t1,t2) ->
        let (a, al) = get_ctyp_args t1 [] in
        pp f "@[<2>type %a.@ %a@]" (list o#ctyp "@ ") [a::al] o#ctyp t2
    | <:ctyp< private $t$ >> -> pp f "@[private@ %a@]" o#simple_ctyp t
    | t -> o#simple_ctyp f t ];

    method constructor_type f t =
    match t with
    [ <:ctyp@loc< $t1$ and $t2$ >> ->
        let () = o#node f t (fun _ -> loc) in
        pp f "%a@ * %a" o#constructor_type t1 o#constructor_type t2
    | <:ctyp< $_$ -> $_$ >> -> pp f "(%a)" o#ctyp t
    | t -> o#ctyp f t ];


    method sig_item f sg =
      let () = o#node f sg Ast.loc_of_sig_item in
      match sg with
      [ <:sig_item<>> -> ()
      | <:sig_item< $sg$; $<:sig_item<>>$ >> |
        <:sig_item< $<:sig_item<>>$; $sg$ >> ->
          o#sig_item f sg
      | <:sig_item< $sg1$; $sg2$ >> ->
          do { o#sig_item f sg1; cut f; o#sig_item f sg2 }
      | <:sig_item< exception $t$ >> ->
          pp f "@[<2>exception@ %a%(%)@]" o#ctyp t semisep
      | <:sig_item< external $s$ : $t$ = $sl$ >> ->
          pp f "@[<2>external@ %a :@ %a =@ %a%(%)@]"
            o#var s o#ctyp t (meta_list o#quoted_string "@ ") sl semisep
      | <:sig_item< module $s1$ ($s2$ : $mt1$) : $mt2$ >> ->
          let rec loop accu =
            fun
            [ <:module_type< functor ($s$ : $mt1$) -> $mt2$ >> ->
                loop [(s, mt1)::accu] mt2
            | mt -> (List.rev accu, mt) ] in
          let (al, mt) = loop [(s2, mt1)] mt2 in
          pp f "@[<2>module %a@ @[<0>%a@] :@ %a%(%)@]"
            o#var s1 o#functor_args al o#module_type mt semisep
      | <:sig_item< module $s$ : $mt$ >> ->
          pp f "@[<2>module %a :@ %a%(%)@]"
            o#var s o#module_type mt semisep
      | <:sig_item< module type $s$ = $ <:module_type<>> $ >> ->
          pp f "@[<2>module type %a%(%)@]" o#var s semisep
      | <:sig_item< module type $s$ = $mt$ >> ->
          pp f "@[<2>module type %a =@ %a%(%)@]"
            o#var s o#module_type mt semisep
      | <:sig_item< open $sl$ >> ->
          pp f "@[<2>open@ %a%(%)@]" o#ident sl semisep
      | <:sig_item< type $t$ >> ->
          pp f "@[<hv0>@[<hv2>type %a@]%(%)@]" o#ctyp t semisep
      | <:sig_item< value $s$ : $t$ >> ->
          pp f "@[<2>%s %a :@ %a%(%)@]"
            o#value_val o#var s o#ctyp t semisep
      | <:sig_item< include $mt$ >> ->
          pp f "@[<2>include@ %a%(%)@]" o#module_type mt semisep
      | <:sig_item< class type $ct$ >> ->
          pp f "@[<2>class type %a%(%)@]" o#class_type ct semisep
      | <:sig_item< class $ce$ >> ->
          pp f "@[<2>class %a%(%)@]" o#class_type ce semisep
      | <:sig_item< module rec $mb$ >> ->
          pp f "@[<2>module rec %a%(%)@]"
            o#module_rec_binding mb semisep
      | <:sig_item< # $_$ $_$ >> -> ()
      | <:sig_item< $anti:s$ >> ->
          pp f "%a%(%)" o#anti s semisep ];

    method str_item f st =
      let () = o#node f st Ast.loc_of_str_item in
      match st with
      [ <:str_item<>> -> ()
      | <:str_item< $st$; $<:str_item<>>$ >> |
        <:str_item< $<:str_item<>>$; $st$ >> ->
          o#str_item f st
      | <:str_item< $st1$; $st2$ >> ->
            do { o#str_item f st1; cut f; o#str_item f st2 }
      | <:str_item< exception $t$ >> ->
            pp f "@[<2>exception@ %a%(%)@]" o#ctyp t semisep
      | <:str_item< exception $t$ = $sl$ >> ->
            pp f "@[<2>exception@ %a =@ %a%(%)@]" o#ctyp t o#ident sl semisep
      | <:str_item< external $s$ : $t$ = $sl$ >> ->
            pp f "@[<2>external@ %a :@ %a =@ %a%(%)@]"
              o#var s o#ctyp t (meta_list o#quoted_string "@ ") sl semisep
      | <:str_item< module $s1$ ($s2$ : $mt1$) = $me$ >> ->
          match o#module_expr_get_functor_args [(s2, mt1)] me with
          [ (al, me, Some mt2) ->
              pp f "@[<2>module %a@ @[<0>%a@] :@ %a =@ %a%(%)@]"
                o#var s1 o#functor_args al o#module_type mt2
                o#module_expr me semisep
          | (al, me, _) ->
              pp f "@[<2>module %a@ @[<0>%a@] =@ %a%(%)@]"
                o#var s1 o#functor_args al o#module_expr me semisep ]
      | <:str_item< module $s$ : $mt$ = $me$ >> ->
            pp f "@[<2>module %a :@ %a =@ %a%(%)@]"
              o#var s o#module_type mt o#module_expr me semisep
      | <:str_item< module $s$ = $me$ >> ->
            pp f "@[<2>module %a =@ %a%(%)@]" o#var s o#module_expr me semisep
      | <:str_item< module type $s$ = $mt$ >> ->
            pp f "@[<2>module type %a =@ %a%(%)@]"
              o#var s o#module_type mt semisep
      | Ast.StOpn _loc ov sl ->
      (* | <:str_item< open $sl$ >> -> *)
            pp f "@[<2>open%a@ %a%(%)@]"
            o#override_flag ov
            o#ident sl semisep
      | <:str_item< type $t$ >> ->
            pp f "@[<hv0>@[<hv2>type %a@]%(%)@]" o#ctyp t semisep
      | <:str_item< value $rec:r$ $bi$ >> ->
            pp f "@[<2>%s %a%a%(%)@]" o#value_let o#rec_flag r o#binding bi semisep
      | <:str_item< $exp:e$ >> ->
            pp f "@[<2>let _ =@ %a%(%)@]" o#expr e semisep
      | <:str_item< include $me$ >> ->
            pp f "@[<2>include@ %a%(%)@]" o#simple_module_expr me semisep
      | <:str_item< class type $ct$ >> ->
            pp f "@[<2>class type %a%(%)@]" o#class_type ct semisep
      | <:str_item< class $ce$ >> ->
            pp f "@[<hv2>class %a%(%)@]" o#class_declaration ce semisep
      | <:str_item< module rec $mb$ >> ->
            pp f "@[<2>module rec %a%(%)@]" o#module_rec_binding mb semisep
      | <:str_item< # $_$ $_$ >> -> ()
      | <:str_item< $anti:s$ >> -> pp f "%a%(%)" o#anti s semisep
      | Ast.StExc _ _ (Ast.OAnt _) -> assert False ];

    method module_type f mt =
    let () = o#node f mt Ast.loc_of_module_type in
    match mt with
    [ <:module_type<>> -> assert False
    | <:module_type< module type of $me$ >> ->
        pp f "@[<2>module type of@ %a@]" o#module_expr me
    | <:module_type< $id:i$ >> -> o#ident f i
    | <:module_type< $anti:s$ >> -> o#anti f s
    | <:module_type< functor ( $s$ : $mt1$ ) -> $mt2$ >> ->
          pp f "@[<2>functor@ @[<1>(%a :@ %a)@]@ ->@ %a@]"
            o#var s o#module_type mt1 o#module_type mt2
    | <:module_type< '$s$ >> -> pp f "'%a" o#var s
    | <:module_type< sig $sg$ end >> ->
          pp f "@[<hv0>@[<hv2>sig@ %a@]@ end@]" o#sig_item sg
    | Ast.MtAtt _loc s str e ->
        pp f "((%a)[@@%s %a])" o#module_type e s o#str_item str
    | <:module_type< $mt$ with $wc$ >> ->
          pp f "@[<2>%a@ with@ %a@]" o#module_type mt o#with_constraint wc ];

    method with_constraint f wc =
    let () = o#node f wc Ast.loc_of_with_constr in
    match wc with
    [ <:with_constr<>> -> ()
    | <:with_constr< type $t1$ = $t2$ >> ->
          pp f "@[<2>type@ %a =@ %a@]" o#ctyp t1 o#ctyp t2
    | <:with_constr< module $i1$ = $i2$ >> ->
          pp f "@[<2>module@ %a =@ %a@]" o#ident i1 o#ident i2
    | <:with_constr< type $t1$ := $t2$ >> ->
          pp f "@[<2>type@ %a :=@ %a@]" o#ctyp t1 o#ctyp t2
    | <:with_constr< module $i1$ := $i2$ >> ->
          pp f "@[<2>module@ %a :=@ %a@]" o#ident i1 o#ident i2
    | <:with_constr< $wc1$ and $wc2$ >> ->
          do { o#with_constraint f wc1; pp f o#andsep; o#with_constraint f wc2 }
    | <:with_constr< $anti:s$ >> -> o#anti f s ];

    method module_expr f me =
    let () = o#node f me Ast.loc_of_module_expr in
    match me with
    [ <:module_expr<>> -> assert False
    | <:module_expr< ( struct $st$ end : sig $sg$ end ) >> ->
          pp f "@[<2>@[<hv2>struct@ %a@]@ end :@ @[<hv2>sig@ %a@]@ end@]"
            o#str_item st o#sig_item sg
    | _ -> o#simple_module_expr f me ];

    method simple_module_expr f me =
    let () = o#node f me Ast.loc_of_module_expr in
    match me with
    [ <:module_expr<>> -> assert False
    | <:module_expr< $id:i$ >> -> o#ident f i
    | <:module_expr< $anti:s$ >> -> o#anti f s
    | <:module_expr< $me1$ $me2$ >> ->
          pp f "@[<2>%a@,(%a)@]" o#module_expr me1 o#module_expr me2
    | <:module_expr< functor ( $s$ : $mt$ ) -> $me$ >> ->
          pp f "@[<2>functor@ @[<1>(%a :@ %a)@]@ ->@ %a@]" o#var s o#module_type mt o#module_expr me
    | <:module_expr< struct $st$ end >> ->
          pp f "@[<hv0>@[<hv2>struct@ %a@]@ end@]" o#str_item st
    | <:module_expr< ( $me$ : $mt$ ) >> ->
          pp f "@[<1>(%a :@ %a)@]" o#module_expr me o#module_type mt
    | <:module_expr< (value $e$ : $mt$ ) >> ->
          pp f "@[<1>(%s %a :@ %a)@]" o#value_val o#expr e o#module_type mt
    | <:module_expr< (value $e$ ) >> ->
          pp f "@[<1>(%s %a)@]" o#value_val o#expr e
    | Ast.MeAtt _loc s str e ->
        pp f "((%a)[@@%s %a])" o#module_expr e s o#str_item str
    ];

    method class_expr f ce =
    let () = o#node f ce Ast.loc_of_class_expr in
    match ce with
    [ <:class_expr< $ce$ $e$ >> ->
          pp f "@[<2>%a@ %a@]" o#class_expr ce o#apply_expr e
    | <:class_expr< $id:i$ >> ->
          pp f "@[<2>%a@]" o#ident i
    | <:class_expr< $id:i$ [ $t$ ] >> ->
          pp f "@[<2>@[<1>[%a]@]@ %a@]" o#class_params t o#ident i
    | <:class_expr< virtual $lid:i$ >> ->
          pp f "@[<2>virtual@ %a@]" o#var i
    | <:class_expr< virtual $lid:i$ [ $t$ ] >> ->
          pp f "@[<2>virtual@ @[<1>[%a]@]@ %a@]" o#class_params t o#var i
    | <:class_expr< fun $p$ -> $ce$ >> ->
          pp f "@[<2>fun@ %a@ ->@ %a@]" o#simple_patt p o#class_expr ce
    | <:class_expr< let $rec:r$ $bi$ in $ce$ >> ->
          pp f "@[<2>let %a%a@]@ @[<2>in@ %a@]"
            o#rec_flag r o#binding bi o#class_expr ce
    | <:class_expr< object $cst$ end >> ->
          pp f "@[<hv0>@[<hv2>object %a@]@ end@]" o#class_str_item cst
    | <:class_expr< object ($p$) $cst$ end >> ->
          pp f "@[<hv0>@[<hv2>object @[<1>(%a)@]@ %a@]@ end@]"
            o#patt p o#class_str_item cst
    | <:class_expr< ( $ce$ : $ct$ ) >> ->
          pp f "@[<1>(%a :@ %a)@]" o#class_expr ce o#class_type ct
    | <:class_expr< $anti:s$ >> -> o#anti f s
    | <:class_expr< $ce1$ and $ce2$ >> ->
          do { o#class_expr f ce1; pp f o#andsep; o#class_expr f ce2 }
    | <:class_expr< $ce1$ = fun $p$ -> $ce2$ >> when is_irrefut_patt p ->
          pp f "@[<2>%a@ %a" o#class_expr ce1
            o#patt_class_expr_fun_args (p, ce2)
    | <:class_expr< $ce1$ = $ce2$ >> ->
          pp f "@[<2>%a =@]@ %a" o#class_expr ce1 o#class_expr ce2
    | Ast.CeAtt _loc s str e ->
        pp f "((%a)[@@%s %a])" o#class_expr e s o#str_item str
    | _ -> assert False ];

    method class_type f ct =
    let () = o#node f ct Ast.loc_of_class_type in
    match ct with
    [ <:class_type< $id:i$ >> ->
          pp f "@[<2>%a@]" o#ident i
    | <:class_type< $id:i$ [ $t$ ] >> ->
          pp f "@[<2>[@,%a@]@,]@ %a" o#class_params t o#ident i
    | <:class_type< virtual $lid:i$ >> ->
          pp f "@[<2>virtual@ %a@]" o#var i
    | <:class_type< virtual $lid:i$ [ $t$ ] >> ->
          pp f "@[<2>virtual@ [@,%a@]@,]@ %a" o#class_params t o#var i
    | <:class_type< [ $t$ ] -> $ct$ >> ->
          pp f "@[<2>%a@ ->@ %a@]" o#simple_ctyp t o#class_type ct
    | <:class_type< object $csg$ end >> ->
          pp f "@[<hv0>@[<hv2>object@ %a@]@ end@]" o#class_sig_item csg
    | <:class_type< object ($t$) $csg$ end >> ->
          pp f "@[<hv0>@[<hv2>object @[<1>(%a)@]@ %a@]@ end@]"
            o#ctyp t o#class_sig_item csg
    | <:class_type< $anti:s$ >> -> o#anti f s
    | <:class_type< $ct1$ and $ct2$ >> ->
          do { o#class_type f ct1; pp f o#andsep; o#class_type f ct2 }
    | <:class_type< $ct1$ : $ct2$ >> ->
          pp f "%a :@ %a" o#class_type ct1 o#class_type ct2
    | <:class_type< $ct1$ = $ct2$ >> ->
          pp f "%a =@ %a" o#class_type ct1 o#class_type ct2
    | Ast.CtAtt _loc s str e ->
        pp f "((%a)[@@%s %a])" o#class_type e s o#str_item str
    | _ -> assert False ];

    method class_sig_item f csg =
      let () = o#node f csg Ast.loc_of_class_sig_item in
      match csg with
      [ <:class_sig_item<>> -> ()
      | <:class_sig_item< $csg$; $<:class_sig_item<>>$ >> |
        <:class_sig_item< $<:class_sig_item<>>$; $csg$ >> ->
          o#class_sig_item f csg
      | <:class_sig_item< $csg1$; $csg2$ >> ->
            do { o#class_sig_item f csg1; cut f; o#class_sig_item f csg2 }
      | <:class_sig_item< constraint $t1$ = $t2$ >> ->
            pp f "@[<2>constraint@ %a =@ %a%(%)@]" o#ctyp t1 o#ctyp t2 no_semisep
      | <:class_sig_item< inherit $ct$ >> ->
            pp f "@[<2>inherit@ %a%(%)@]" o#class_type ct no_semisep
      | <:class_sig_item< method $private:pr$ $s$ : $t$ >> ->
            pp f "@[<2>method %a%a :@ %a%(%)@]" o#private_flag pr o#var s
              o#ctyp t no_semisep
      | <:class_sig_item< method virtual $private:pr$ $s$ : $t$ >> ->
            pp f "@[<2>method virtual %a%a :@ %a%(%)@]"
              o#private_flag pr o#var s o#ctyp t no_semisep
      | <:class_sig_item< value $mutable:mu$ $virtual:vi$ $s$ : $t$ >> ->
            pp f "@[<2>%s %a%a%a :@ %a%(%)@]"
              o#value_val o#mutable_flag mu o#virtual_flag vi o#var s o#ctyp t
              no_semisep
      | <:class_sig_item< $anti:s$ >> ->
            pp f "%a%(%)" o#anti s no_semisep ];

    method class_str_item f cst =
      let () = o#node f cst Ast.loc_of_class_str_item in
      match cst with
      [ <:class_str_item<>> -> ()
      | <:class_str_item< $cst$; $<:class_str_item<>>$ >> |
        <:class_str_item< $<:class_str_item<>>$; $cst$ >> ->
          o#class_str_item f cst
      | <:class_str_item< $cst1$; $cst2$ >> ->
            do { o#class_str_item f cst1; cut f; o#class_str_item f cst2 }
      | <:class_str_item< constraint $t1$ = $t2$ >> ->
            pp f "@[<2>constraint %a =@ %a%(%)@]" o#ctyp t1 o#ctyp t2 no_semisep
      | <:class_str_item< inherit $override:ov$ $ce$ >> ->
            pp f "@[<2>inherit%a@ %a%(%)@]" o#override_flag ov o#class_expr ce no_semisep
      | <:class_str_item< inherit $override:ov$ $ce$ as $lid:s$ >> ->
            pp f "@[<2>inherit%a@ %a as@ %a%(%)@]" o#override_flag ov o#class_expr ce o#var s no_semisep
      | <:class_str_item< initializer $e$ >> ->
            pp f "@[<2>initializer@ %a%(%)@]" o#expr e no_semisep
      | <:class_str_item< method $override:ov$ $private:pr$ $s$ = $e$ >> ->
            pp f "@[<2>method%a %a%a =@ %a%(%)@]"
              o#override_flag ov o#private_flag pr o#var s o#expr e no_semisep
      | <:class_str_item< method $override:ov$ $private:pr$ $s$ : $t$ = $e$ >> ->
            pp f "@[<2>method%a %a%a :@ %a =@ %a%(%)@]"
              o#override_flag ov o#private_flag pr o#var s o#ctyp t o#expr e no_semisep
      | <:class_str_item< method virtual $private:pr$ $s$ : $t$ >> ->
            pp f "@[<2>method virtual@ %a%a :@ %a%(%)@]"
              o#private_flag pr o#var s o#ctyp t no_semisep
      | <:class_str_item< value virtual $mutable:mu$ $s$ : $t$ >> ->
            pp f "@[<2>%s virtual %a%a :@ %a%(%)@]"
              o#value_val o#mutable_flag mu o#var s o#ctyp t no_semisep
      | <:class_str_item< value $override:ov$ $mutable:mu$ $s$ = $e$ >> ->
            pp f "@[<2>%s%a %a%a =@ %a%(%)@]"
              o#value_val o#override_flag ov o#mutable_flag mu o#var s o#expr e no_semisep
      | <:class_str_item< $anti:s$ >> ->
            pp f "%a%(%)" o#anti s no_semisep ];

    method implem f st =
      match st with
      [ <:str_item< $exp:e$ >> -> pp f "@[<0>%a%(%)@]@." o#expr e semisep
      | st -> pp f "@[<v0>%a@]@." o#str_item st ];

    method interf f sg = pp f "@[<v0>%a@]@." o#sig_item sg;
  end;

  value with_outfile output_file fct arg =
    let call close f = do {
      try fct f arg with [ exn -> do { close (); raise exn } ];
      close ()
    } in
    match output_file with
    [ None -> call (fun () -> ()) std_formatter
    | Some s ->
        let oc = open_out s in
        let f = formatter_of_out_channel oc in
        call (fun () -> close_out oc) f ];

  value print output_file fct =
    let o = new printer () in
    with_outfile output_file (fct o);

  value print_interf ?input_file:(_) ?output_file sg =
    print output_file (fun o -> o#interf) sg;

  value print_implem ?input_file:(_) ?output_file st =
    print output_file (fun o -> o#implem) st;

end;

module MakeMore (Syntax : Sig.Camlp4Syntax)
: (Sig.Printer Syntax.Ast).S
= struct

  include Make Syntax;

  value semisep : ref sep = ref ("@\n" : sep);
  value margin = ref 78;
  value comments = ref True;
  value locations = ref False;
  value curry_constr = ref False;

  value print output_file fct =
    let o = new printer ~comments:comments.val
                        ~curry_constr:curry_constr.val () in
    let o = o#set_semisep semisep.val in
    let o = if locations.val then o#set_loc_and_comments else o in
    with_outfile output_file
      (fun f ->
        let () = Format.pp_set_margin f margin.val in
        Format.fprintf f "@[<v0>%a@]@." (fct o));

  value print_interf ?input_file:(_) ?output_file sg =
    print output_file (fun o -> o#interf) sg;

  value print_implem ?input_file:(_) ?output_file st =
    print output_file (fun o -> o#implem) st;

  value check_sep s =
    if String.contains s '%' then failwith "-sep Format error, % found in string"
    else (Obj.magic (Struct.Token.Eval.string s : string) : sep);

  Options.add "-l" (Arg.Int (fun i -> margin.val := i))
    "<length> line length for pretty printing.";

  Options.add "-ss" (Arg.Unit (fun () -> semisep.val := ";;"))
    " Print double semicolons.";

  Options.add "-no_ss" (Arg.Unit (fun () -> semisep.val := ""))
    " Do not print double semicolons (default).";

  Options.add "-sep" (Arg.String (fun s -> semisep.val := check_sep s))
    " Use this string between phrases.";

  Options.add "-curry-constr" (Arg.Set curry_constr) "Use currified constructors.";

  Options.add "-no_comments" (Arg.Clear comments) "Do not add comments.";

  Options.add "-add_locations" (Arg.Set locations) "Add locations as comment.";

end;
