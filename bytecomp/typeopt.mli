(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

val is_function_type :
      Env.t -> Types.type_expr -> (Types.type_expr * Types.type_expr) option
val is_base_type : Env.t -> Types.type_expr -> Path.t -> bool

val maybe_pointer_type : Env.t -> Types.type_expr
  -> Lambda.immediate_or_pointer
val maybe_pointer : Typedtree.expression -> Lambda.immediate_or_pointer

val array_type_kind : Env.t -> Types.type_expr -> Lambda.array_kind
val array_kind : Typedtree.expression -> Lambda.array_kind
val array_pattern_kind : Typedtree.pattern -> Lambda.array_kind
val bigarray_type_kind_and_layout :
      Env.t -> Types.type_expr -> Lambda.bigarray_kind * Lambda.bigarray_layout
val value_kind : Env.t -> Types.type_expr -> Lambda.value_kind
val function_return_value_kind : Env.t -> Types.type_expr -> Lambda.value_kind

val lazy_val_requires_forward : Env.t -> Types.type_expr -> bool
  (** Whether a forward block is needed for a lazy thunk on a value, i.e.
      if the value can be represented as a float/forward/lazy *)
