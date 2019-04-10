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

(** Bytecode compilation for .ml and .mli files. *)

val interface:
  source_file:string -> output_prefix:string -> unit
val implementation:
  source_file:string -> output_prefix:string -> unit

(** {3 Internal functions} **)

val to_lambda :
  Compile_common.info ->
  Typedtree.structure * Typedtree.module_coercion ->
  Lambda.lambda * Ident.Set.t


val to_bytecode :
  Compile_common.info ->
  Lambda.lambda * Ident.Set.t ->
  Instruct.instruction list * Ident.Set.t
(** [to_bytecode info typed] takes a typechecked implementation
    and returns its bytecode.
*)

val emit_bytecode :
  Compile_common.info -> Instruct.instruction list * Ident.Set.t -> unit
(** [emit_bytecode bytecode] output the bytecode executable. *)
