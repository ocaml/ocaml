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
 *)


module Id = struct
  value name = "Camlp4Parsers.Extfun";
  value version = "$Id$";
end;

module Make (Syntax : Sig.Camlp4Syntax.S) = struct
  include Syntax;

  value not_impl name x =
    let desc = ErrorHandler.ObjTools.desc (Obj.repr x) in
    let () = print_newline ()
    in failwith ("pa_extfun: not impl " ^ name ^ " " ^ desc);

  (* FIXME Replace it by a meta lift *)
  value rec mexpr p =
    let _loc = Ast.loc_of_patt p in
    match p with
    [ <:patt< $p1$ $p2$ >> ->
        loop <:expr< [$mexpr p2$] >> p1 where rec loop el =
          fun
          [ <:patt< $p1$ $p2$ >> -> loop <:expr< [$mexpr p2$ :: $el$] >> p1
          | p -> <:expr< Extfun.Eapp [$mexpr p$ :: $el$] >> ]
    | <:patt< $id:i$ >> ->
        let rec mident i acc =
          match i with
          [ <:ident< $id:i1$ . $i2$ >> -> mident i1 (mident i2 acc)
          (* <:expr< [$mident i2$ :: $el$] >> i1 *)
          (* [ <:ident< $uid:i1$ . $i2$ >> -> mident <:expr< [ Extfun.Econ $str:i1$ :: $el$] >> i2 *)
          | <:ident< $lid:_$ >> -> <:expr< [ Extfun.Evar () :: $acc$ ] >>
          | <:ident< $uid:id$ >> -> <:expr< [ Extfun.Econ $str:id$ :: $acc$ ] >>
          | <:ident< $anti:_$ >> -> not_impl "mident:anti" i
          | _ -> not_impl "mident" i ]
        in mident i <:expr< [] >>
    | <:patt< ($tup:p$) >> -> <:expr< Extfun.Etup $mexpr p$ >>
    | <:patt< $p1$, $p2$ >> -> <:expr< $mexpr p1$, $mexpr p2$ >>
    | <:patt< ` $id$ >> -> <:expr< Extfun.Econ $str:id$ >>
    | <:patt< $int:s$ >> -> <:expr< Extfun.Eint $str:s$ >>
    | <:patt< $str:s$ >> -> <:expr< Extfun.Estr $str:s$ >>
    | <:patt< ($p1$ as $_$) >> -> mexpr p1
    | <:patt< _ >> -> <:expr< Extfun.Evar () >>
    | <:patt< $_$ | $_$ >> ->
        Loc.raise _loc (Failure "or patterns not allowed in extfun")
    | p -> not_impl "mexpr" p ]
  ;

  value rec catch_any =
    fun
    [ <:patt< $uid:_$ >> -> False
    | <:patt< ` $_$ >> -> False
    | <:patt< $lid:_$ >> -> True
    | <:patt< _ >> -> True
    | <:patt< ($p1$, $p2$) >> | <:patt< $p1$, $p2$ >> -> catch_any p1 && catch_any p2
    | <:patt< $_$ $_$ >> -> False
    | <:patt< $_$ | $_$ >> -> False
    | <:patt< $int:_$ >> -> False
    | <:patt< $str:_$ >> -> False
    | <:patt< ($p1$ as $_$) >> -> catch_any p1
    | p -> not_impl "catch_any" p ]
  ;

  value conv (p, wo, e) =
    let tst = mexpr p in
    let _loc = Loc.merge (Ast.loc_of_patt p) (Ast.loc_of_expr e) in
    let e =
      if wo = None && catch_any p then <:expr< fun $p$ -> Some $e$ >>
      else match wo with
           [ None -> <:expr< fun [ $pat:p$ -> Some $e$ | _ -> None ] >>
           | Some w -> <:expr< fun [ $pat:p$ when $w$ -> Some $e$ | _ -> None ] >> ]
      (* else <:expr< fun [ $p$ $when:wo$ -> Some $e$ | _ -> None ] >> *)
    in
    let has_when =
      match wo with
      [ Some _ -> <:expr< True >>
      | None -> <:expr< False >> ]
    in
    <:expr< ($tst$, $has_when$, $e$) >>
  ;

  value rec conv_list tl =
    fun
    [ [pe :: pel] ->
        let _loc = Ast.loc_of_expr tl in
        <:expr< [$conv pe$ :: $conv_list tl pel$] >>
    | [] -> tl ]
  ;

  value rec split_or =
    fun
    [ [(<:patt< $p1$ | $p2$ >>, wo, e) :: pel] ->
        split_or [(p1, wo, e); (p2, wo, e) :: pel]
    | [(<:patt< ($p1$ | $p2$ as $p$) >>, wo, e) :: pel] ->
        let p1 =
          let _loc = Ast.loc_of_patt p1 in
          <:patt< ($p1$ as $p$) >>
        in
        let p2 =
          let _loc = Ast.loc_of_patt p2 in
          <:patt< ($p2$ as $p$) >>
        in
        split_or [(p1, wo, e); (p2, wo, e) :: pel]
    | [pe :: pel] -> [pe :: split_or pel]
    | [] -> [] ]
  ;

  EXTEND Gram
    GLOBAL: expr;
    expr: LEVEL "top"
      [ [ "extfun"; e = SELF; "with"; "["; list = match_case_list; "]" ->
            <:expr< Extfun.extend $e$ $list$ >> ] ]
    ;
    match_case_list:
      [ [ pel = LIST0 match_case SEP "|" ->
            conv_list <:expr< [] >> (split_or pel) ] ]
    ;
    match_case:
      [ [ p = patt; aso = OPT [ "as"; p = patt -> p ];
          w = OPT [ "when"; e = expr -> e ]; "->"; e = expr ->
            let p =
              match aso with
              [ Some p2 -> <:patt< ($p$ as $p2$) >>
              | _ -> p ]
            in
            (p, w, e) ] ]
    ;
  END;

end;

module M = Register.OCamlSyntaxExtension Id Make;
