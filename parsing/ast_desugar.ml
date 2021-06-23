(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Desugaring Parsetree fragments *)

open Location
open Parsetree

let ghost_loc loc = { loc with loc_ghost = true }
let ghexp ~loc d = Ast_helper.Exp.mk ~loc:(ghost_loc loc) d
let ghpat ~loc d = Ast_helper.Pat.mk ~loc:(ghost_loc loc) d
let ghloc ~loc d = { txt = d; loc = ghost_loc loc }
let ghrhs rhs loc = mkloc rhs (ghost_loc loc)

let ghexp_cons_desc consloc args =
  Pexp_construct(ghrhs (Longident.Lident "::") consloc, Some args)
let ghpat_cons_desc consloc args =
  Ppat_construct(ghrhs (Longident.Lident "::") consloc, Some ([], args))

let rec exp_list_ nilloc = function
  | [] ->
      let nil = ghloc ~loc:nilloc (Longident.Lident "[]") in
      Pexp_construct (nil, None), nilloc
  | e1 :: el ->
      let exp_el, el_loc = exp_list_ nilloc el in
      let loc = { e1.pexp_loc with loc_end= el_loc.loc_end } in
      let arg = ghexp ~loc (Pexp_tuple [e1; ghexp ~loc:el_loc exp_el]) in
      ghexp_cons_desc loc arg, loc

let exp_list l e = fst (exp_list_ l e)

let rec pat_list_ nilloc = function
  | [] ->
      let nil = ghloc ~loc:nilloc (Longident.Lident "[]") in
      Ppat_construct (nil, None), nilloc
  | p1 :: pl ->
      let pat_pl, el_loc = pat_list_ nilloc pl in
      let loc = { p1.ppat_loc with loc_end= el_loc.loc_end } in
      let arg = ghpat ~loc (Ppat_tuple [p1; ghpat ~loc:el_loc pat_pl]) in
      ghpat_cons_desc loc arg, loc

let pat_list l p = fst (pat_list_ l p)
