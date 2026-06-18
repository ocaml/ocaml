(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Descriptions of the OCaml tools *)

module type Tool = sig
  val name : string
  val family : string
  val flags : string
  val directory : string
  val exit_status_variable : Variables.t
  val reference_variable : Variables.t
  val output_variable : Variables.t
  val reference_filename_suffix : Environments.t -> string
  val reference_file : Environments.t -> string -> string
end

type tool = (module Tool)

val tool :
  name : string ->
  family : string ->
  flags : string ->
  directory : string ->
  exit_status_variable : Variables.t ->
  reference_variable : Variables.t ->
  output_variable : Variables.t ->
  ?reference_filename_suffix : (Environments.t -> string) ->
  unit ->
  tool

val expected_exit_status : Environments.t -> tool -> int

val ocamldoc: tool
