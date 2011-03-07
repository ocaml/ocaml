(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright   2006    Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

open Camlp4;

module Id = struct
  value name    = "Camlp4Profiler";
  value version = Sys.ocaml_version;
end;

module Make (AstFilters : Camlp4.Sig.AstFilters) = struct
  open AstFilters;
  open Ast;

  value decorate_binding decorate_fun = object
    inherit Ast.map as super;
    method binding =
      fun
      [ <:binding@_loc< $lid:id$ = $(<:expr< fun [ $_$ ] >> as e)$ >> ->
          <:binding< $lid:id$ = $decorate_fun id e$ >>
      | b -> super#binding b ];
  end#binding;

  value decorate decorate_fun = object (o)
    inherit Ast.map as super;
    method str_item =
      fun
      [ <:str_item@_loc< value $rec:r$ $b$ >> ->
          <:str_item< value $rec:r$ $decorate_binding decorate_fun b$ >>
      | st -> super#str_item st ];
    method expr =
      fun
      [ <:expr@_loc< let $rec:r$ $b$ in $e$ >> ->
          <:expr< let $rec:r$ $decorate_binding decorate_fun b$ in $o#expr e$ >>
      | <:expr@_loc< fun [ $_$ ] >> as e -> decorate_fun "<fun>" e
      | e -> super#expr e ];
  end;

  value decorate_this_expr e id =
    let buf = Buffer.create 42 in
    let _loc = Ast.loc_of_expr e in
    let () = Format.bprintf buf "%s @@ %a@?" id Loc.dump _loc in
    let s = Buffer.contents buf in
    <:expr< let () = Camlp4prof.count $`str:s$ in $e$ >>;

  value rec decorate_fun id =
    let decorate = decorate decorate_fun in
    let decorate_expr = decorate#expr in
    let decorate_match_case = decorate#match_case in
    fun
    [ <:expr@_loc< fun $p$ -> $e$ >> ->
        <:expr< fun $p$ -> $decorate_fun id e$ >>
    | <:expr@_loc< fun [ $m$ ] >> ->
        decorate_this_expr <:expr< fun [ $decorate_match_case m$ ] >> id
    | e -> decorate_this_expr (decorate_expr e) id ];

  register_str_item_filter (decorate decorate_fun)#str_item;

end;

let module M = Camlp4.Register.AstFilter Id Make in ();
