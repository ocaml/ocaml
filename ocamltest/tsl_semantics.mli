(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Interpretation of TSL blocks and operations on test trees *)

exception No_such_test_or_action of string
val lookup_test : string Tsl_ast.located -> Tests.t

type behavior =
  | Skip_all
  | Run

type summary = Test_result.status = Pass | Skip | Fail
val string_of_summary : summary -> string

val run_environment_statement :
  add_msg:(string -> unit) ->
  report_error:(Location.t -> exn -> string -> string) ->
  Environments.t ->
  Tsl_ast.environment_statement Tsl_ast.located ->
  (Environments.t, unit) result

val run :
  log:out_channel ->
  add_msg:(string -> unit) ->
  report_error:(Location.t -> exn -> string -> string) ->
  behavior -> Environments.t -> summary -> Tsl_ast.t -> summary
