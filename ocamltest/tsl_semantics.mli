(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris           *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Interpretation of TSL parse trees *)

val interprete_statement :
  Environments.t -> Tsl_ast.statement -> Environments.t
val interprete_statements :
  Environments.t -> Tsl_ast.statement list -> Environments.t
val compile_test_spec :
  Format.formatter ->
  Environments.t -> Tsl_ast.test_spec -> Tests.t * Environments.t

val compile_test_specs :
  Format.formatter ->
  Environments.t -> Tsl_ast.test_spec list -> (Tests.t * Environments.t) list
