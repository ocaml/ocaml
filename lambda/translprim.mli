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

(* Insertion of debugging events *)

val event_before : Lambda.scoped_location -> Typedtree.expression
                   -> Lambda.lambda -> Lambda.lambda

val event_after : Lambda.scoped_location -> Typedtree.expression
                  -> Lambda.lambda -> Lambda.lambda

(* Translation of primitives *)

val add_exception_ident : Ident.t -> unit
val remove_exception_ident : Ident.t -> unit

val clear_used_primitives : unit -> unit
val get_used_primitives: unit -> Path.t list

val check_primitive_arity :
  Location.t -> Primitive.description -> unit

val transl_primitive :
  Lambda.scoped_location -> Primitive.description -> Env.t ->
  Types.type_expr -> Path.t option -> Lambda.lambda

val transl_primitive_application :
  Lambda.scoped_location -> Primitive.description -> Env.t ->
  Types.type_expr -> Path.t -> Typedtree.expression option ->
  Lambda.lambda list -> Typedtree.expression list -> Lambda.lambda

(* Errors *)

type error =
  | Unknown_builtin_primitive of string
  | Wrong_arity_builtin_primitive of string

exception Error of Location.t * error

open Format

val report_error : formatter -> error -> unit
