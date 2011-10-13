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

open Camlp4.PreCast;
AstFilters.register_str_item_filter
  (Ast.map_expr
    (fun
     [ <:expr@loc< $e1$ & $e2$ >> -> <:expr@loc< $e1$ $e2$ >>
     | e -> e ]))#str_item;
