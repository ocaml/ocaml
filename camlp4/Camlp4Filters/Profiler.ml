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
  value name    = "Camlp4Filters.Profiler";
  value version = "$Id$";
end;

module Make (AstFilters : Camlp4.Sig.AstFilters.S) = struct
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
      [ <:str_item@_loc< value $opt:r$ $b$ >> ->
          <:str_item< value $opt:r$ $decorate_binding decorate_fun b$ >>
      | st -> super#str_item st ];
    method expr =
      fun
      [ <:expr@_loc< let $opt:r$ $b$ in $e$ >> ->
          <:expr< let $opt:r$ $decorate_binding decorate_fun b$ in $o#expr e$ >>
      | <:expr@_loc< fun [ $_$ ] >> as e -> decorate_fun "<fun>" e
      | e -> super#expr e ];
  end;

  value rec decorate_fun id =
    let buf = Buffer.create 42 in
    let decorate_expr = (decorate decorate_fun)#expr in
    fun
    [ <:expr@_loc< fun $p$ -> $e$ >> ->
        <:expr< fun $p$ -> $decorate_fun id e$ >>
    | e ->
        let _loc = Ast.loc_of_expr e in
        let () = Format.bprintf buf "%s @@ %a@?" id Loc.dump _loc in
        let s = Buffer.contents buf in
        <:expr< let () = Camlp4Filters.Profiler.count $`str:s$
                in $decorate_expr e$ >> ];

  register_str_item_filter (decorate decorate_fun)#str_item;

end;

value count =
  let h = Hashtbl.create 1007 in
  let () = at_exit (fun () ->
    Hashtbl.iter (fun k v -> Format.eprintf "%s: %d@." k v.val) h) in
  fun s ->
    try incr (Hashtbl.find h s)
    with [ Not_found -> Hashtbl.add h s (ref 1) ];

let module M = Camlp4.Register.AstFilter Id Make in ();
