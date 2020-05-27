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

(* Descriptions of the OCaml compilers *)

class compiler :
  name : string ->
  flags : string ->
  directory : string ->
  exit_status_variable : Variables.t ->
  reference_variable : Variables.t ->
  output_variable : Variables.t ->
  host : Ocaml_backends.t ->
  target : Ocaml_backends.t ->
object inherit Ocaml_tools.tool
  method host : Ocaml_backends.t
  method target : Ocaml_backends.t
  method program_variable : Variables.t
  method program_output_variable : Variables.t option
end

val ocamlc_byte : compiler

val ocamlc_opt : compiler

val ocamlopt_byte : compiler

val ocamlopt_opt : compiler
