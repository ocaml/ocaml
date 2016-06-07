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
val tests_of_ast :
  Format.formatter ->
  Tsl_ast.test_spec list ->
  (Tests.t * Tsl_ast.environment_description) list

val actions_of_tests :
  (* Format.formatter -> *)
  Tests.t list -> Actions.t list
