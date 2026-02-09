(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Alistair O'Brien, University of Cambridge             *)
(*                                                                        *)
(*   Copyright 2026, Alistair O'Brien                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Approximate typing

    This module computes *best-effort* approximations of inferred types
    without performing full type inference. It is intended for typing
    recursive functions.

    All functions are conservative and never raise errors: always
    returning a fresh unknown type instead. *)

open Types

(** [type_expression ~env ?mono_lvl exp] computes a type approximation
    for [exp] in environment [env].

    The [mono_lvl] parameter specifies the level at which fresh type
    variables are generated when precise typing information is unavailable
    or deliberately approximated. If not provided, it defaults to the
    current level. *)
val type_expression
  :  env:Env.t
  -> ?mono_lvl:int
  -> Parsetree.expression
  -> type_expr
