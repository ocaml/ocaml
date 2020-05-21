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

(* Description of the OCaml compilers *)

open Ocamltest_stdlib

class compiler
  ~(name : string)
  ~(flags : string)
  ~(directory : string)
  ~(exit_status_variable : Variables.t)
  ~(reference_variable : Variables.t)
  ~(output_variable : Variables.t)
  ~(host : Ocaml_backends.t)
  ~(target : Ocaml_backends.t)
= object (self) inherit Ocaml_tools.tool
  ~name:name
  ~family:"compiler"
  ~flags:flags
  ~directory:directory
  ~exit_status_variable:exit_status_variable
  ~reference_variable:reference_variable
  ~output_variable:output_variable
  as tool

  method host = host
  method target = target

  method program_variable =
    if Ocaml_backends.is_native host
    then Builtin_variables.program2
    else Builtin_variables.program

  method program_output_variable =
    if Ocaml_backends.is_native host
    then None
    else Some Builtin_variables.output

  method ! reference_file env prefix =
    let default = tool#reference_file env prefix in
    if Sys.file_exists default then default else
    let suffix = self#reference_filename_suffix env in
    let mk s = (Filename.make_filename prefix s) ^ suffix in
    let filename = mk
      (Ocaml_backends.string_of_backend target) in
    if Sys.file_exists filename then filename else
    mk "compilers"
end

let ocamlc_byte = new compiler
  ~name: Ocaml_commands.ocamlrun_ocamlc
  ~flags: ""
  ~directory: "ocamlc.byte"
  ~exit_status_variable: Ocaml_variables.ocamlc_byte_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference
  ~output_variable: Ocaml_variables.compiler_output
  ~host: Ocaml_backends.Bytecode
  ~target: Ocaml_backends.Bytecode

let ocamlc_opt = new compiler
  ~name: Ocaml_files.ocamlc_dot_opt
  ~flags: ""
  ~directory: "ocamlc.opt"
  ~exit_status_variable: Ocaml_variables.ocamlc_opt_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference2
  ~output_variable: Ocaml_variables.compiler_output2
  ~host: Ocaml_backends.Native
  ~target: Ocaml_backends.Bytecode

let ocamlopt_byte = new compiler
  ~name: Ocaml_commands.ocamlrun_ocamlopt
  ~flags: ""
  ~directory: "ocamlopt.byte"
  ~exit_status_variable: Ocaml_variables.ocamlopt_byte_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference
  ~output_variable: Ocaml_variables.compiler_output
  ~host: Ocaml_backends.Bytecode
  ~target: Ocaml_backends.Native

let ocamlopt_opt = new compiler
  ~name: Ocaml_files.ocamlopt_dot_opt
  ~flags: ""
  ~directory: "ocamlopt.opt"
  ~exit_status_variable: Ocaml_variables.ocamlopt_opt_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference2
  ~output_variable: Ocaml_variables.compiler_output2
  ~host: Ocaml_backends.Native
  ~target: Ocaml_backends.Native
