(**************************************************************************)
(*        ^o3                                                             *)
(* ~/\_/\_|)                       OCaml                                  *)
(* |/=_=\|                                                                *)
(* "     "                                                                *)
(*               Jeremy Yallop, University of Cambridge                   *)
(*                                                                        *)
(*   Copyright 2017 Jeremy Yallop                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val is_valid_recursive_expression :
  Ident.t list ->
  Typedtree.expression ->
  Value_rec_types.recursive_binding_kind option

val is_valid_class_expr : Ident.t list -> Typedtree.class_expr -> bool
