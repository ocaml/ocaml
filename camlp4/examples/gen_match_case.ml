(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2007  Institut  National  de  Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

open Camlp4.PreCast;;

let gen patts exprs =
  let cases =
    List.fold_right2 begin fun patt expr acc ->
      let _loc = Loc.merge (Ast.loc_of_patt patt) (Ast.loc_of_expr expr) in
      <:match_case< $patt$ -> $expr$ | $acc$ >>
    end patts exprs <:match_case@here<>>
  in
  let _loc = Ast.loc_of_match_case cases in
  <:expr< function $cases$ >>
;;
