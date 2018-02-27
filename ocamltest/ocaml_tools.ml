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

open Ocamltest_stdlib

class tool
  ~(name : string -> string)
  ~(flags : string)
  ~(directory : string)
  ~(exit_status_variable : Variables.t)
  ~(reference_variable : Variables.t)
  ~(output_variable : Variables.t)
= object (self)
  method name = name
  method flags = flags
  method directory = directory
  method exit_status_variable = exit_status_variable
  method reference_variable = reference_variable
  method output_variable = output_variable

  method reference_filename_suffix env =
    let tool_reference_suffix =
      Environments.safe_lookup Ocaml_variables.compiler_reference_suffix env
    in
    if tool_reference_suffix<>""
    then tool_reference_suffix ^ ".reference"
    else ".reference"

  method reference_file env prefix =
    let suffix = self#reference_filename_suffix env in
    (Filename.make_filename prefix directory) ^ suffix
end

let expected_exit_status env tool =
  try int_of_string
    (Environments.safe_lookup tool#exit_status_variable env)
  with _ -> 0
