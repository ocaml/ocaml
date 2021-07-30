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

class tool :
  name : string ->
  family : string ->
  flags : string ->
  directory : string ->
  exit_status_variable : Variables.t ->
  reference_variable : Variables.t ->
  output_variable : Variables.t ->
object
  method name : string
  method family : string
  method flags : string
  method directory : string
  method exit_status_variable : Variables.t
  method reference_variable : Variables.t
  method output_variable : Variables.t
  method reference_filename_suffix : Environments.t -> string
  method reference_file : Environments.t -> string -> string
end

val expected_exit_status : Environments.t -> tool -> int

val ocamldoc: tool
