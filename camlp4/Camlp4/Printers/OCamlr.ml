(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

open Format;

module Id = struct
  value name = "Camlp4.Printers.OCamlr";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Sig.Camlp4Syntax) = struct
  include Syntax;
  open Sig;

  module PP_o = OCaml.Make Syntax;

  open PP_o;

  value pp = fprintf;

  value is_keyword =
    let keywords = ["where"]
    and not_keywords = ["false"; "function"; "true"; "val"]
    in fun s -> not (List.mem s not_keywords)
             && (is_keyword s || List.mem s keywords);

  class printer ?curry_constr:(init_curry_constr = True) ?(comments = True) () =
  object (o)
    inherit PP_o.printer ~curry_constr:init_curry_constr ~comments () as super;

    value semisep : sep = ";";
    value andsep : sep = "@]@ @[<2>and@ ";
    value value_val = "value";
    value value_let = "value";
    value mode = if comments then `comments else `no_comments;
    value curry_constr = init_curry_constr;
    value first_match_case = True;

    method under_pipe = o;
    method under_semi = o;
    method reset_semi = o;
    method reset = o;
    method private unset_first_match_case = {< first_match_case = False >};
    method private set_first_match_case = {< first_match_case = True >};

    method seq f e =
      let rec self right f e =
        let go_right = self right and go_left = self False in
        match e with
        [ <:expr< let $rec:r$ $bi$ in $e1$ >> ->
            if right then
              pp f "@[<2>let %a%a@];@ %a"
                o#rec_flag r o#binding bi go_right e1
            else
              pp f "(%a)" o#expr e
        | <:expr< do { $e$ } >> -> go_right f e
        | <:expr< $e1$; $e2$ >> -> do {
            pp f "%a;@ " go_left e1;
            match (right, e2) with
            [ (True, <:expr< let $rec:r$ $bi$ in $e3$ >>) ->
                pp f "@[<2>let %a%a@];@ %a"
                  o#rec_flag r o#binding bi go_right e3
            | _ -> go_right f e2 ] }
        | e -> o#expr f e ]
      in self True f e;

    method var f =
      fun
      [ "" -> pp f "$lid:\"\"$"
      | "[]" -> pp f "[]"
      | "()" -> pp f "()"
      | " True"  -> pp f "True"
      | " False" -> pp f "False"
      | v ->
          match lex_string v with
          [ (LIDENT s | UIDENT s | ESCAPED_IDENT s) when is_keyword s ->
              pp f "%s__" s
          | SYMBOL s ->
              pp f "( %s )" s
          | LIDENT s | UIDENT s | ESCAPED_IDENT s ->
              pp_print_string f s
          | tok -> failwith (sprintf
                    "Bad token used as an identifier: %s"
                    (Token.to_string tok)) ] ];

    method type_params f =
      fun
      [ [] -> ()
      | [x] -> pp f "@ %a" o#ctyp x
      | l -> pp f "@ @[<1>%a@]" (list o#ctyp "@ ") l ];

    method match_case f =
      fun
      [ <:match_case<>> -> pp f "@ []"
      | m -> pp f "@ [ %a ]" o#set_first_match_case#match_case_aux m ];

    method match_case_aux f =
      fun
      [ <:match_case<>> -> ()
      | <:match_case< $anti:s$ >> -> o#anti f s
      | <:match_case< $a1$ | $a2$ >> ->
          pp f "%a%a" o#match_case_aux a1 o#unset_first_match_case#match_case_aux a2
      | <:match_case< $p$ -> $e$ >> ->
          let () = if first_match_case then () else pp f "@ | " in
          pp f "@[<2>%a@ ->@ %a@]" o#patt p o#under_pipe#expr e
      | <:match_case< $p$ when $w$ -> $e$ >> ->
          let () = if first_match_case then () else pp f "@ | " in
          pp f "@[<2>%a@ when@ %a@ ->@ %a@]"
            o#patt p o#under_pipe#expr w o#under_pipe#expr e ];

    method sum_type f =
      fun
      [ <:ctyp<>> -> pp f "[]"
      | t -> pp f "@[<hv0>[ %a ]@]" o#ctyp t
      ];

    method ident f i =
    let () = o#node f i Ast.loc_of_ident in
    match i with
    [ <:ident< $i1$ $i2$ >> -> pp f "%a@ %a" o#dot_ident i1 o#dot_ident i2
    | i -> o#dot_ident f i ];

    method private dot_ident f i =
    let () = o#node f i Ast.loc_of_ident in
    match i with
    [ <:ident< $i1$.$i2$ >> -> pp f "%a.@,%a" o#dot_ident i1 o#dot_ident i2
    | <:ident< $anti:s$ >> -> o#anti f s
    | <:ident< $lid:s$ >> | <:ident< $uid:s$ >> -> o#var f s
    | i -> pp f "(%a)" o#ident i ];

    method patt4 f = fun
    [ <:patt< [$_$ :: $_$] >> as p ->
        let (pl, c) = o#mk_patt_list p in
        match c with
        [ None -> pp f "@[<2>[@ %a@]@ ]" (list o#patt ";@ ") pl
        | Some x -> pp f "@[<2>[ %a ::@ %a ]@]" (list o#patt ";@ ") pl o#patt x ]
    | p -> super#patt4 f p ];

    method expr_list_cons _ f e =
      let (el, c) = o#mk_expr_list e in
      match c with
      [ None -> o#expr_list f el
      | Some x -> pp f "@[<2>[ %a ::@ %a ]@]" (list o#expr ";@ ") el o#expr x ];

    method expr f e =
    let () = o#node f e Ast.loc_of_expr in
    match e with
    [ <:expr< $e1$ := $e2$ >> ->
        pp f "@[<2>%a@ :=@ %a@]" o#dot_expr e1 o#expr e2
    | <:expr< fun $p$ -> $e$ >> when Ast.is_irrefut_patt p ->
        pp f "@[<2>fun@ %a@]" o#patt_expr_fun_args (p, e)
    | <:expr< fun [ $a$ ] >> ->
        pp f "@[<hv0>fun%a@]" o#match_case a
    | <:expr< assert False >> -> pp f "@[<2>assert@ False@]"
    | e -> super#expr f e ];

    method dot_expr f e =
    let () = o#node f e Ast.loc_of_expr in
    match e with
    [ <:expr< $e$.val >> -> pp f "@[<2>%a.@,val@]" o#simple_expr e
    | e -> super#dot_expr f e ];

    method ctyp f t =
    let () = o#node f t Ast.loc_of_ctyp in
    match t with
    [ Ast.TyDcl _ tn tp te cl -> do {
        pp f "@[<2>%a%a@]" o#var tn o#type_params tp;
        match te with
        [ <:ctyp<>> -> ()
        | _ -> pp f " =@ %a" o#ctyp te ];
        if cl <> [] then pp f "@ %a" (list o#constrain "@ ") cl else ();
      }
    | <:ctyp< $t1$ : mutable $t2$ >> ->
        pp f "@[%a :@ mutable %a@]" o#ctyp t1 o#ctyp t2
    | t -> super#ctyp f t ];

    method simple_ctyp f t =
    let () = o#node f t Ast.loc_of_ctyp in
    match t with
    [ <:ctyp< [ = $t$ ] >> -> pp f "@[<2>[ =@ %a@]@ ]" o#ctyp t
    | <:ctyp< [ < $t$ ] >> -> pp f "@[<2>[ <@ %a@]@,]" o#ctyp t
    | <:ctyp< [ < $t1$ > $t2$ ] >> ->
        pp f "@[<2>[ <@ %a@ >@ %a@]@ ]" o#ctyp t1 o#ctyp t2
    | <:ctyp< [ > $t$ ] >> -> pp f "@[<2>[ >@ %a@]@,]" o#ctyp t
    | <:ctyp< $t1$ == $t2$ >> ->
        pp f "@[<2>%a@ ==@ %a@]" o#simple_ctyp t1 o#simple_ctyp t2
    | <:ctyp< ~ $s$ : $t$ >> -> pp f "@[<2>~%s:@ %a@]" s o#simple_ctyp t
    | t -> super#simple_ctyp f t ];

    method ctyp1 f = fun
    [ <:ctyp< $t1$ $t2$ >> ->
        match get_ctyp_args t1 [t2] with
        [ (_, [_]) -> pp f "@[<2>%a@ %a@]" o#simple_ctyp t1 o#simple_ctyp t2
        | (a, al) -> pp f "@[<2>%a@]" (list o#simple_ctyp "@ ") [a::al] ]
    | <:ctyp< ! $t1$ . $t2$ >> ->
        let (a, al) = get_ctyp_args t1 [] in
        pp f "@[<2>! %a.@ %a@]" (list o#ctyp "@ ") [a::al] o#ctyp t2
    | t -> super#ctyp1 f t ];

    method constructor_type f t =
    match t with
    [ <:ctyp@loc< $t1$ and $t2$ >> ->
        let () = o#node f t (fun _ -> loc) in
        pp f "%a@ and %a" o#constructor_type t1 o#constructor_type t2
    | t -> o#ctyp f t ];

    method str_item f st =
    match st with
    [ <:str_item< $exp:e$ >> -> pp f "@[<2>%a%(%)@]" o#expr e semisep
    | st -> super#str_item f st ];

    method module_expr f me =
    let () = o#node f me Ast.loc_of_module_expr in
    match me with
    [ <:module_expr< $me1$ $me2$ >> ->
          pp f "@[<2>%a@ %a@]" o#module_expr me1 o#simple_module_expr me2
    | me -> super#module_expr f me ];

    method simple_module_expr f me =
    let () = o#node f me Ast.loc_of_module_expr in
    match me with
    [ <:module_expr< $_$ $_$ >> ->
          pp f "(%a)" o#module_expr me
    | _ -> super#simple_module_expr f me ];

    method implem f st = pp f "@[<v0>%a@]@." o#str_item st;

    method class_type f ct =
    let () = o#node f ct Ast.loc_of_class_type in
    match ct with
    [ <:class_type< [ $t$ ] -> $ct$ >> ->
          pp f "@[<2>[ %a ] ->@ %a@]" o#simple_ctyp t o#class_type ct
    | <:class_type< $id:i$ >> ->
          pp f "@[<2>%a@]" o#ident i
    | <:class_type< $id:i$ [ $t$ ] >> ->
          pp f "@[<2>%a [@,%a@]@,]" o#ident i o#class_params t
    | <:class_type< virtual $lid:i$ >> ->
          pp f "@[<2>virtual@ %a@]" o#var i
    | <:class_type< virtual $lid:i$ [ $t$ ] >> ->
          pp f "@[<2>virtual@ %a@ [@,%a@]@,]" o#var i o#class_params t
    | ct -> super#class_type f ct ];

    method class_expr f ce =
    let () = o#node f ce Ast.loc_of_class_expr in
    match ce with
    [ <:class_expr< $id:i$ >> ->
          pp f "@[<2>%a@]" o#ident i
    | <:class_expr< $id:i$ [ $t$ ] >> ->
          pp f "@[<2>%a@ @[<1>[%a]@]@]" o#ident i o#class_params t
    | <:class_expr< virtual $lid:i$ >> ->
          pp f "@[<2>virtual@ %a@]" o#var i
    | <:class_expr< virtual $lid:i$ [ $t$ ] >> ->
          pp f "@[<2>virtual@ %a@ @[<1>[%a]@]@]" o#var i o#ctyp t
    | ce -> super#class_expr f ce ];
  end;

  value with_outfile = with_outfile;
  value print = print;
  value print_interf = print_interf;
  value print_implem = print_implem;

end;

module MakeMore (Syntax : Sig.Camlp4Syntax)
: (Sig.Printer Syntax.Ast).S
= struct

  include Make Syntax;

  value margin = ref 78;
  value comments = ref True;
  value locations = ref False;
  value curry_constr = ref True;

  value print output_file fct =
    let o = new printer ~comments:comments.val
                        ~curry_constr:curry_constr.val () in
    let o = if locations.val then o#set_loc_and_comments else o in
    with_outfile output_file
      (fun f ->
        let () = Format.pp_set_margin f margin.val in
        Format.fprintf f "@[<v0>%a@]@." (fct o));

  value print_interf ?input_file:(_) ?output_file sg =
    print output_file (fun o -> o#interf) sg;

  value print_implem ?input_file:(_) ?output_file st =
    print output_file (fun o -> o#implem) st;

  Options.add "-l" (Arg.Int (fun i -> margin.val := i))
    "<length> line length for pretty printing.";

  Options.add "-no_comments" (Arg.Clear comments) "Do not add comments.";

  Options.add "-add_locations" (Arg.Set locations) "Add locations as comment.";

end;
