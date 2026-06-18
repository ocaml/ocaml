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

let reference_filename_suffix_default env =
  let tool_reference_suffix =
    Environments.safe_lookup Ocaml_variables.compiler_reference_suffix env
  in
  if tool_reference_suffix<>""
  then tool_reference_suffix ^ ".reference"
  else ".reference"

let tool
  ~(name : string)
  ~(family : string)
  ~(flags : string)
  ~(directory : string)
  ~(exit_status_variable : Variables.t)
  ~(reference_variable : Variables.t)
  ~(output_variable : Variables.t)
  ?(reference_filename_suffix = reference_filename_suffix_default)
  ()
  : tool
= (module struct
  let name = name
  let family = family
  let flags = flags
  let directory = directory
  let exit_status_variable = exit_status_variable
  let reference_variable = reference_variable
  let output_variable = output_variable
  let reference_filename_suffix = reference_filename_suffix

  let reference_file env prefix =
    let suffix = reference_filename_suffix env in
    (Filename.make_filename prefix directory) ^ suffix
end)

let expected_exit_status env (module Tool : Tool) =
  Actions_helpers.exit_status_of_variable env Tool.exit_status_variable

let ocamldoc =
  let reference_filename_suffix env =
    let backend =
      Environments.safe_lookup Ocaml_variables.ocamldoc_backend env in
    if backend = "" then
      ".reference"
    else "." ^ backend ^ ".reference"
  in
  tool
    ~name:Ocaml_files.ocamldoc
    ~family:"doc"
    ~flags:""
    ~directory:"ocamldoc"
    ~exit_status_variable:Ocaml_variables.ocamldoc_exit_status
    ~reference_variable:Ocaml_variables.ocamldoc_reference
    ~output_variable:Ocaml_variables.ocamldoc_output
    ~reference_filename_suffix ()
