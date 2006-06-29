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
  value version = "$Id$";
end;

module Make (Syntax : Sig.Camlp4Syntax.S) = struct
  include Syntax;
  open Sig.Camlp4Token;

  module PP_o = OCaml.Make Syntax;

  open PP_o;

  value pp = fprintf;

  class printer ?(curry_constr = True) ?(comments = True) () =
  object (o)
    inherit PP_o.printer ~curry_constr ~comments () as super;

    value semisep = ";";
    value andsep : format unit formatter unit = "@]@ @[<2>and@ ";
    value value_val = "value";
    value value_let = "value";
    value mode = if comments then `comments else `no_comments;
    value curry_constr = curry_constr;
    value first_assoc = True;

    method under_pipe = o;
    method under_semi = o;
    method reset_semi = o;
    method reset = o;
    method private unset_first_assoc = {< first_assoc = False >};
    method private set_first_assoc = {< first_assoc = True >};

    method var f =
      fun
      [ "" -> pp f "$lid:\"\"$"
      | "[]" -> pp f "[]"
      | "()" -> pp f "()"
      | v ->
          match lex_string v with
          [ (LIDENT s | UIDENT s | ESCAPED_IDENT s) when is_keyword s ->
               pp f "\\%s" s
          | SYMBOL s ->
              pp f "\\%s" s
          | LIDENT s | UIDENT s | ESCAPED_IDENT s ->
              pp_print_string f s
          | tok -> failwith (sprintf
                    "Bad token used as an identifier: %s"
                    (Token.to_string tok)) ] ];

    method type_params f =
      fun
      [ [] -> ()
      | [x] -> pp f "%a@ " o#ctyp x
      | l -> pp f "@[<1>%a@]@ " (list o#ctyp "@ ") l ];

    method assoc f =
      fun
      [ <:assoc<>> -> pp f "@ []"
      | m -> pp f "@ [ %a ]" o#set_first_assoc#assoc_aux m ];

    method assoc_aux f =
      fun
      [ <:assoc<>> -> ()
      | <:assoc< $anti:s$ >> -> o#anti f s
      | <:assoc< $a1$ | $a2$ >> ->
          pp f "%a%a" o#assoc_aux a1 o#unset_first_assoc#assoc_aux a2
      | <:assoc< $p$ -> $e$ >> ->
          let () = if first_assoc then () else pp f "@ | " in
          pp f "@[<2>%a@ ->@ %a@]" o#patt p o#under_pipe#expr e
      | <:assoc< $p$ when $w$ -> $e$ >> ->
          let () = if first_assoc then () else pp f "@ | " in
          pp f "@[<2>%a@ when@ %a@ ->@ %a@]"
            o#patt p o#under_pipe#expr w o#under_pipe#expr e ];

    method sum_type f t = pp f "@[<hv0>[ %a ]@]" o#ctyp t;

    method ident f i =
    let () = o#node f i Ast.loc_of_ident in
    match i with
    [ <:ident< $i1$ $i2$ >> -> pp f "%a@ %a" o#ident i1 o#ident i2
    | i -> super#ident f i ];

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
    [ <:expr< do { $e1$; $e2$ } >> ->
        pp f "@[<hv0>@[<hv2>do {@ %a;@ %a@]@ }@]" o#expr e2 o#expr e1
    | <:expr< do { $e$ } >> ->
        o#expr f e
    | <:expr< $e1$ := $e2$ >> ->
        pp f "@[<2>%a@ :=@ %a@]" o#expr e1 o#expr e2
    | <:expr< fun $p$ -> $e$ >> when is_irrefut_patt p ->
        pp f "@[<2>fun@ %a@]" o#patt_expr_fun_args (p, e)
    | <:expr< fun [ $a$ ] >> ->
        pp f "@[<hv0>fun%a@]" o#assoc a
    | <:expr< assert False >> -> pp f "@[<2>assert@ False@]"
    | e -> super#expr f e ];

    method dot_expr f e =
    let () = o#node f e Ast.loc_of_expr in
    match e with
    [ <:expr< $e$.val >> -> pp f "@[<2>%a.@,val@]" o#simple_expr e
    | e -> super#dot_expr f e ];

    method simple_expr f e =
    let () = o#node f e Ast.loc_of_expr in
    match e with
    [ <:expr< for $s$ = $e1$ to $e2$ do { $e3$ } >> ->
        pp f "@[<hv0>@[<hv2>@[<2>for %a@ =@ %a@ to@ %a@ do {@]@ %a@]@ }@]"
          o#var s o#expr e1 o#expr e2 o#stms e3
    | <:expr< for $s$ = $e1$ downto $e2$ do { $e3$ } >> ->
        pp f "@[<hv0>@[<hv2>@[<2>for %a@ =@ %a@ downto@ %a@ do {@]@ %a@]@ }@]"
          o#var s o#expr e1 o#expr e2 o#stms e3
    | <:expr< while $e1$ do { $e2$ } >> ->
        pp f "@[<2>while@ %a@ do {@ %a@ }@]" o#expr e1 o#stms e2
    | <:expr< do { $e$ } >> ->
        pp f "@[<hv0>@[<hv2>do {@ %a@]@ }@]" o#stms e
    | e -> super#simple_expr f e ];

    method ctyp f t =
    let () = o#node f t Ast.loc_of_ctyp in
    match t with
    [ Ast.TyDcl _ tn tp te cl -> do {
        pp f "@[<2>%a%a@]" o#var tn o#type_params tp;
        match te with
        [ <:ctyp< '$s$ >>
            when not (List.exists (fun [ <:ctyp< '$s'$ >> -> s = s'
                                       | _ -> False ]) tp) -> ()
        | _ -> pp f " =@ %a" o#ctyp te ];
        if cl <> [] then pp f "@ %a" (list o#constrain "@ ") cl else ();
      }
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
    [ <:str_item< $exp:e$ >> -> pp f "@[<2>%a%s@]" o#expr e semisep
    | st -> super#str_item f st ];

    method module_expr f me =
    let () = o#node f me Ast.loc_of_module_expr in
    match me with
    [ <:module_expr< $me1$ $me2$ >> ->
          pp f "@[<2>%a@,(%a)@]" o#module_expr me1 o#module_expr me2
    | me -> super#module_expr f me ];

    method implem f st = pp f "@[<v0>%a@]@." o#str_item st;

    method class_type f ct =
    let () = o#node f ct Ast.loc_of_class_type in
    match ct with
    [ <:class_type< [ $t$ ] -> $ct$ >> ->
          pp f "@[<2>[ %a ] ->@ %a@]" o#simple_ctyp t o#class_type ct
    | ct -> super#class_type f ct ];
  end;

  value with_outfile = with_outfile;
  value print = print;
  value print_interf = print_interf;
  value print_implem = print_implem;

end;
