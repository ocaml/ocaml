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

(* Description of the OCaml compilers and toplevels *)

open Ocamltest_stdlib

class compiler
  ~(name : string -> string)
  ~(flags : string)
  ~(directory : string)
  ~(exit_status_variable : Variables.t)
  ~(reference_variable : Variables.t)
  ~(output_variable : Variables.t)
  ~(backend : Ocaml_backends.t)
  ~(is_toplevel : bool)
  ~(is_native : bool)
= object inherit Ocaml_tools.tool
  ~name:name
  ~flags:flags
  ~directory:directory
  ~exit_status_variable:exit_status_variable
  ~reference_variable:reference_variable
  ~output_variable:output_variable
  method backend = backend
  method is_toplevel = is_toplevel
  method is_native = is_native
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
  ~is_toplevel: false
  ~is_native: false

let ocamlc_opt = new compiler
  ~name: Ocaml_files.ocamlc_dot_opt
  ~flags: ""
  ~directory: "ocamlc.opt"
  ~exit_status_variable: Ocaml_variables.ocamlc_opt_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference2
  ~output_variable: Ocaml_variables.compiler_output2
  ~backend: Ocaml_backends.Bytecode
  ~is_toplevel: false
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
  ~is_toplevel: false
  ~is_native: false

let ocamlopt_opt = new compiler
  ~name: Ocaml_files.ocamlopt_dot_opt
  ~flags: ""
  ~directory: "ocamlopt.opt"
  ~exit_status_variable: Ocaml_variables.ocamlopt_opt_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference2
  ~output_variable: Ocaml_variables.compiler_output2
  ~backend: Ocaml_backends.Native
  ~is_toplevel: false
  ~is_native: true

(* Top-levels *)

let ocaml = new compiler
  ~name: Ocaml_commands.ocamlrun_ocaml
  ~flags: ""
  ~directory: "ocaml"
  ~exit_status_variable: Ocaml_variables.ocaml_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference
  ~output_variable: Ocaml_variables.compiler_output
  ~backend: Ocaml_backends.Bytecode
  ~is_toplevel: true
  ~is_native: false

let ocamlnat = new compiler
  ~name: Ocaml_files.ocamlnat
  ~flags: "-S" (* Keep intermediate assembly files *)
  ~directory: "ocamlnat"
  ~exit_status_variable: Ocaml_variables.ocamlnat_exit_status
  ~reference_variable: Ocaml_variables.compiler_reference2
  ~output_variable: Ocaml_variables.compiler_output2
  ~backend: Ocaml_backends.Native
  ~is_toplevel: true
  ~is_native: true

let expected_exit_status env compiler =
  try int_of_string
    (Environments.safe_lookup compiler#exit_status_variable env)
  with _ -> 0

let reference_filename env prefix compiler =
  let compiler_reference_suffix =
    Environments.safe_lookup Ocaml_variables.compiler_reference_suffix env in
  let suffix =
    if compiler_reference_suffix<>""
    then compiler_reference_suffix ^ ".reference"
    else ".reference" in
  let mk s = (Filename.make_filename prefix s) ^ suffix in
  let filename = mk compiler#directory in
  if Sys.file_exists filename then filename else
  let filename = mk
    (Ocaml_backends.string_of_backend compiler#backend) in
  if Sys.file_exists filename then filename else
  mk "compilers"
