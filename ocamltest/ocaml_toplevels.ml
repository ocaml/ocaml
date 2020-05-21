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

(* Description of the OCaml toplevels *)

open Ocamltest_stdlib
open Ocaml_backends

type t =
  Ocaml_backends.t

let ocaml = Bytecode
let ocamlnat = Native

let name = function
  | Bytecode -> Ocaml_commands.ocamlrun_ocaml
  | Native -> Ocaml_files.ocamlnat

let exit_status_variable = function
  | Bytecode -> Ocaml_variables.ocaml_exit_status
  | Native -> Ocaml_variables.ocamlnat_exit_status

let reference_variable = function
  | Bytecode -> Ocaml_variables.compiler_reference
  | Native -> Ocaml_variables.compiler_reference2

let output_variable = function
  | Bytecode -> Ocaml_variables.compiler_output
  | Native -> Ocaml_variables.compiler_output2

let directory = function
  | Bytecode -> "ocaml"
  | Native -> "ocamlnat"

let backend t =
  t

let compiler = function
  | Bytecode -> Ocaml_compilers.ocamlc_byte
  | Native -> Ocaml_compilers.ocamlc_opt

let reference_file t env prefix =
  let default =
    let suffix = Ocaml_compilers.reference_file_suffix env in
    Filename.make_filename prefix (directory t) ^ suffix
  in
  if Sys.file_exists default then default
  else begin
    let suffix = Ocaml_compilers.reference_file_suffix env in
    let mk s = Filename.make_filename prefix s ^ suffix in
    let filename = mk (Ocaml_backends.string_of_backend t) in
    if Sys.file_exists filename then filename
    else mk "compilers"
  end
