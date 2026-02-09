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

module Approx_env : sig
  (** An environment for approximate typing.

      It carries an environment together with a *monomorphic level*,
      used to generate fresh type variables when precise typing information
      is unabailable or deliberately approximated. *)
  type t

  (** [create ~env ?mono_lvl ()] creates a fresh environment.

      If [mono_lvl] is not provided, it defaults to the current level. *)
  val create : env:Env.t -> ?mono_lvl:int -> unit -> t
end

(** [type_expression aenv exp] computes a type approximation for [exp]. *)
val type_expression : Approx_env.t -> Parsetree.expression -> type_expr
