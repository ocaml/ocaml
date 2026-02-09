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

(** [type_expression env exp] computes a type approximation for [exp]. *)
val type_expression : Env.t -> Parsetree.expression -> type_expr
