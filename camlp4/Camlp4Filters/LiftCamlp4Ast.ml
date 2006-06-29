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
  value name    = "Camlp4Filters.LiftCamlp4Ast";
  value version = "$Id$";
end;

module Make (AstFilters : Camlp4.Sig.AstFilters.S) = struct
  open AstFilters;

  module MetaLoc = struct
    module Ast = Ast;
    value meta_loc_patt _loc = <:patt< loc >>;
    value meta_loc_expr _loc = <:expr< loc >>;
  end;
  module MetaAst = Camlp4.Struct.MetaAst.Make MetaLoc;

  register_str_item_filter (fun ast ->
    let _loc = Ast.loc_of_str_item ast in
    <:str_item< let loc = Loc.ghost in $exp:MetaAst.Expr.str_item ast$ >>);

end;

let module M = Camlp4.Register.AstFilter Id Make in ();
