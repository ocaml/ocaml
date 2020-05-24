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

module Compiler = struct
  type t =
    {
      host: Ocaml_backends.t;
      target: Ocaml_backends.t;
    }

  let name = function
    | { host = Bytecode; target = Bytecode } ->
        Ocaml_commands.ocamlrun_ocamlc
    | { host = Native; target = Bytecode } ->
        Ocaml_files.ocamlc_dot_opt
    | { host = Bytecode; target = Native } ->
        Ocaml_commands.ocamlrun_ocamlopt
    | { host = Native; target = Native } ->
        Ocaml_files.ocamlopt_dot_opt

  let reference_variable = function
    | { host = Bytecode; _ } ->
        Ocaml_variables.compiler_reference
    | { host = Native; _ } ->
        Ocaml_variables.compiler_reference2

  let output_variable = function
    | { host = Bytecode; _ } ->
        Ocaml_variables.compiler_output
    | { host = Native; _ } ->
        Ocaml_variables.compiler_output2

  let directory = function
    | { host = Bytecode; target = Bytecode } ->
        "ocamlc.byte"
    | { host = Bytecode; target = Native } ->
        "ocamlopt.byte"
    | { host = Native; target = Bytecode } ->
        "ocamlc.opt"
    | { host = Native; target = Native } ->
        "ocamlopt.opt"

  let exit_status_variable = function
    | { host = Bytecode; target = Bytecode } ->
        Ocaml_variables.ocamlc_byte_exit_status
    | { host = Bytecode; target = Native } ->
        Ocaml_variables.ocamlopt_byte_exit_status
    | { host = Native; target = Bytecode } ->
        Ocaml_variables.ocamlc_opt_exit_status
    | { host = Native; target = Native } ->
        Ocaml_variables.ocamlopt_opt_exit_status

  let reference_file_suffix env =
    Environments.safe_lookup
      Ocaml_variables.compiler_reference_suffix env ^ ".reference"

  let reference_file t env prefix =
    let default =
      let suffix = reference_file_suffix env in
      Filename.make_filename prefix (directory t) ^ suffix
    in
    if Sys.file_exists default then
      default
    else begin
      let suffix = reference_file_suffix env in
      let mk s = Filename.make_filename prefix s ^ suffix in
      let filename =
        mk (Ocaml_backends.string_of_backend t.target) in
      if Sys.file_exists filename then filename
      else mk "compilers"
    end

  let host { host; _ } = host
  let target { target; _ } = target

  let  program_variable = function
    | { host = Native; _ } ->
        Builtin_variables.program2
    | { host = Bytecode; _ } ->
        Builtin_variables.program

  let program_output_variable = function
    | { host = Native; _ } ->
        None
    | { host = Bytecode; _ } ->
        Some Builtin_variables.output

  let ocamlc_byte =
    { host = Bytecode; target = Bytecode }

  let ocamlc_opt =
    { host = Native; target = Bytecode }

  let ocamlopt_byte =
    { host = Bytecode; target = Native }

  let ocamlopt_opt =
    { host = Native; target = Native }
end

module Toplevel = struct
  open Ocaml_backends

  type nonrec t = t

  let name = function
    | Bytecode ->
        Ocaml_commands.ocamlrun_ocaml
    | Native ->
        Ocaml_files.ocamlnat

  let reference_variable = function
    | Bytecode ->
        Ocaml_variables.compiler_reference
    | Native ->
        Ocaml_variables.compiler_reference2

  let output_variable = function
    | Bytecode ->
        Ocaml_variables.compiler_output
    | Native ->
        Ocaml_variables.compiler_output2

  let exit_status_variable = function
    | Bytecode ->
        Ocaml_variables.ocaml_exit_status
    | Native ->
        Ocaml_variables.ocamlnat_exit_status

  let directory = function
    | Bytecode -> "ocaml"
    | Native -> "ocamlnat"

  let compiler = function
    | Bytecode -> Compiler.ocamlc_byte
    | Native -> Compiler.ocamlc_opt

  let reference_file t env prefix =
    let default =
      let suffix = Compiler.reference_file_suffix env in
      Filename.make_filename prefix (directory t) ^ suffix
    in
    if Sys.file_exists default then
      default
    else begin
      let suffix = Compiler.reference_file_suffix env in
      let mk s = Filename.make_filename prefix s ^ suffix in
      let filename = mk (Ocaml_backends.string_of_backend t) in
      if Sys.file_exists filename then filename
      else mk "compilers"
    end

  let backend t = t

  let flags = function
    | Native -> "-S" (* Keep intermediate assembly files *)
    | Bytecode -> ""

  let ocaml = Bytecode
  let ocamlnat = Native
end

module Ocamldoc = struct
  type t = unit

  let name () =
    Ocaml_files.ocamldoc

  let reference_variable () =
    Ocaml_variables.ocamldoc_reference

  let output_variable () =
    Ocaml_variables.ocamldoc_output

  let exit_status_variable () =
    Ocaml_variables.ocamldoc_exit_status

  let directory () =
    "ocamldoc"

  let reference_file_suffix env =
    let backend =
      Environments.safe_lookup Ocaml_variables.ocamldoc_backend env in
    if backend = "" then
      ".reference"
    else
      "." ^ backend ^ ".reference"

  let reference_file () env prefix =
    let suffix = reference_file_suffix env in
    Filename.make_filename prefix (directory ()) ^ suffix

  let output_file () env prefix =
    let backend =
      Environments.safe_lookup Ocaml_variables.ocamldoc_backend env in
    let suffix = match backend with
      | "latex" -> ".tex"
      | "html" -> ".html"
      | "man" -> ".3o"
      | _ -> ".result" in
    prefix ^ suffix

  let ocamldoc = ()
end

type t =
  | Compiler of Compiler.t
  | Toplevel of Toplevel.t
  | Ocamldoc of Ocamldoc.t

let exit_status_variable = function
  | Compiler t -> Compiler.exit_status_variable t
  | Toplevel t -> Toplevel.exit_status_variable t
  | Ocamldoc t -> Ocamldoc.exit_status_variable t

let expected_exit_status env t =
  Actions_helpers.exit_status_of_variable env
    (exit_status_variable t)

let family = function
  | Compiler _ -> "compiler"
  | Ocamldoc _ -> "doc"
  | Toplevel _ -> "toplevel"

let output_variable = function
  | Compiler t -> Compiler.output_variable t
  | Toplevel t -> Toplevel.output_variable t
  | Ocamldoc t -> Ocamldoc.output_variable t

let reference_variable = function
  | Compiler t -> Compiler.reference_variable t
  | Toplevel t -> Toplevel.reference_variable t
  | Ocamldoc t -> Ocamldoc.reference_variable t

let reference_file = function
  | Compiler t -> Compiler.reference_file t
  | Toplevel t -> Toplevel.reference_file t
  | Ocamldoc t -> Ocamldoc.reference_file t

let directory = function
  | Compiler t -> Compiler.directory t
  | Toplevel t -> Toplevel.directory t
  | Ocamldoc t -> Ocamldoc.directory t
