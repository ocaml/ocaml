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
  ~(name : string -> string)
  ~(flags : string)
  ~(directory : string)
  ~(exit_status_variable : Variables.t)
  ~(reference_variable : Variables.t)
  ~(output_variable : Variables.t)
  ~(backend : Ocaml_backends.t)
  ~(is_native : bool)
= object (self) inherit Ocaml_tools.tool
  ~name:name
  ~family:"compiler"
  ~flags:flags
  ~directory:directory
  ~exit_status_variable:exit_status_variable
  ~reference_variable:reference_variable
  ~output_variable:output_variable
  as tool

  method backend = backend
  method is_native = is_native
  
  method program_variable =
    if is_native
    then Builtin_variables.program2
    else Builtin_variables.program
  
  method program_output_variable =
    if is_native then None else Some Builtin_variables.output

  method ! reference_file env prefix =
    let default = tool#reference_file env prefix in
    if Sys.file_exists default then default else
    let suffix = self#reference_filename_suffix env in
    let mk s = (Filename.make_filename prefix s) ^ suffix in
    let filename = mk
      (Ocaml_backends.string_of_backend self#backend) in
    if Sys.file_exists filename then filename else
    mk "compilers"
end

(* Compilers compiling byte-code programs *)

let ocamlc_byte = new compiler
  ~name: Ocaml_commands.ocamlrun_ocamlc
  ~flags: ""
  ~directory: "ocamlc.byte"
  ~exit_status_variable: Ocaml_variables.ocamlc_byte_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference
  ~output_variable: Ocaml_variables.compiler_output
  ~backend: Ocaml_backends.Bytecode
  ~is_native: false

let ocamlc_opt = new compiler
  ~name: Ocaml_files.ocamlc_dot_opt
  ~flags: ""
  ~directory: "ocamlc.opt"
  ~exit_status_variable: Ocaml_variables.ocamlc_opt_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference2
  ~output_variable: Ocaml_variables.compiler_output2
  ~backend: Ocaml_backends.Bytecode
  ~is_native: true

(* Compilers compiling native-code programs *)

let ocamlopt_byte = new compiler
  ~name: Ocaml_commands.ocamlrun_ocamlopt
  ~flags: ""
  ~directory: "ocamlopt.byte"
  ~exit_status_variable: Ocaml_variables.ocamlopt_byte_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference
  ~output_variable: Ocaml_variables.compiler_output
  ~backend: Ocaml_backends.Native
  ~is_native: false

let ocamlopt_opt = new compiler
  ~name: Ocaml_files.ocamlopt_dot_opt
  ~flags: ""
  ~directory: "ocamlopt.opt"
  ~exit_status_variable: Ocaml_variables.ocamlopt_opt_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference2
  ~output_variable: Ocaml_variables.compiler_output2
  ~backend: Ocaml_backends.Native
  ~is_native: true
