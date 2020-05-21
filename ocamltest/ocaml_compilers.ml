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
open Ocaml_backends
open Actions

let (let+) = A.(let+)
let (and+) = A.(and+)

type t =
  {
    host: Ocaml_backends.t;
    target: Ocaml_backends.t;
  }

let host {host; _} = host
let target {target; _} = target

let  program_variable = function
  | { host = Native; _ } -> Builtin_variables.program2
  | { host = Bytecode; _ } -> Builtin_variables.program

let program_output_variable = function
  | { host = Native; _ } -> None
  | { host = Bytecode; _ } -> Some Builtin_variables.output

let ocamlc_byte =
  { host = Bytecode; target = Bytecode }

let ocamlc_opt =
  { host = Native; target = Bytecode }

let ocamlopt_byte =
  { host = Bytecode; target = Native }

let ocamlopt_opt =
  { host = Native; target = Native }

let name = function
  | { host = Bytecode; target = Bytecode } -> Ocaml_commands.ocamlrun_ocamlc
  | { host = Native; target = Bytecode } -> Ocaml_files.ocamlc_dot_opt
  | { host = Bytecode; target = Native } -> Ocaml_commands.ocamlrun_ocamlopt
  | { host = Native; target = Native } -> Ocaml_files.ocamlopt_dot_opt

let exit_status_variable = function
  | { host = Bytecode; target = Bytecode } -> Ocaml_variables.ocamlc_byte_exit_status
  | { host = Bytecode; target = Native } -> Ocaml_variables.ocamlopt_byte_exit_status
  | { host = Native; target = Bytecode } -> Ocaml_variables.ocamlc_opt_exit_status
  | { host = Native; target = Native } -> Ocaml_variables.ocamlopt_opt_exit_status

let reference_variable = function
  | { host = Bytecode; _ } -> Ocaml_variables.compiler_reference
  | { host = Native; _ } -> Ocaml_variables.compiler_reference2

let output_variable = function
  | { host = Bytecode; _ } -> Ocaml_variables.compiler_output
  | { host = Native; _ } -> Ocaml_variables.compiler_output2

let directory = function
  | { host = Bytecode; target = Bytecode } -> "ocamlc.byte"
  | { host = Bytecode; target = Native } -> "ocamlopt.byte"
  | { host = Native; target = Bytecode } -> "ocamlc.opt"
  | { host = Native; target = Native } -> "ocamlopt.opt"

let reference_file_suffix =
  let+ tool_reference_suffix =
    A.safe_lookup Ocaml_variables.compiler_reference_suffix
  in
  if tool_reference_suffix<>""
  then tool_reference_suffix ^ ".reference"
  else ".reference"

let reference_file t prefix =
  let default =
    let+ prefix = prefix
    and+ suffix = reference_file_suffix in
    Filename.make_filename prefix (directory t) ^ suffix
  in
  A.if_ (A.file_exists default) default
    (let+ prefix = prefix
     and+ suffix = reference_file_suffix in
     let mk s = Filename.make_filename prefix s ^ suffix in
     let filename =
       mk (Ocaml_backends.string_of_backend t.target) in
     if Sys.file_exists filename then filename
     else mk "compilers")
