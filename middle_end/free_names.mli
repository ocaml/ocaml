(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Tracking of sets of free variables and symbols. *)

type t
type free_names = t

val is_free_variable : t -> Variable.t -> bool
val is_free_phantom_variable : t -> Variable.t -> bool

val free_variables : t -> Variable.Set.t
(* CR mshinwell: The name is misleading.  It should be "variables used
   in phantom context". *)
val free_phantom_variables : t -> Variable.Set.t

(** Both the normal and phantom free variables in the set. *)
val all_free_variables : t -> Variable.Set.t

val free_symbols : t -> Symbol.Set.t
(* CR mshinwell: clarify whether a symbol in free_phantom_symbols may occur
   in free_symbols *)
val free_phantom_symbols : t -> Symbol.Set.t

(** Both the normal and phantom free symbols in the set. *)
val all_free_symbols : t -> Symbol.Set.t

val subset : t -> t -> bool

val print : Format.formatter -> t -> unit

module Mutable : sig
  type t

  val create : unit -> t

  val free_variable : t -> Variable.t -> unit
  val free_phantom_variable : t -> Variable.t -> unit

  val free_symbol : t -> Symbol.t -> unit
  val free_symbols : t -> Symbol.Set.t -> unit
  val free_phantom_symbol : t -> Symbol.t -> unit

  val union : t -> free_names -> unit
  val union_free_symbols_only : t -> free_names -> unit

  val bound_variables : t -> Variable.Set.t -> unit

  val freeze : t -> free_names
end
