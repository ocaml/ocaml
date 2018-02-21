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

(* Descriptions of the OCaml compilers and toplevels *)

open Ocamltest_stdlib

type t = {
  name : string -> string;
  flags : string;
  directory : string;
  backend : Ocaml_backends.t;
  is_toplevel : bool;
  is_native : bool;
  exit_status_variabe : Variables.t;
  reference_variable : Variables.t;
  output_variable : Variables.t
}

(* Compilers compiling byte-code programs *)

let ocamlc_byte =
{
  name = Ocaml_commands.ocamlrun_ocamlc;
  flags = "";
  directory = "ocamlc.byte";
  backend = Ocaml_backends.Bytecode;
  is_toplevel = false;
  is_native = false;
  exit_status_variabe = Ocaml_variables.ocamlc_byte_exit_status;
  reference_variable = Ocaml_variables.compiler_reference;
  output_variable = Ocaml_variables.compiler_output;
}

let ocamlc_opt =
{
  name = Ocaml_files.ocamlc_dot_opt;
  flags = "";
  directory = "ocamlc.opt";
  backend = Ocaml_backends.Bytecode;
  is_toplevel = false;
  is_native = true;
  exit_status_variabe = Ocaml_variables.ocamlc_opt_exit_status;
  reference_variable = Ocaml_variables.compiler_reference2;
  output_variable = Ocaml_variables.compiler_output2;
}

(* Compilers compiling native-code programs *)

let ocamlopt_byte =
{
  name = Ocaml_commands.ocamlrun_ocamlopt;
  flags = "";
  directory = "ocamlopt.byte";
  backend = Ocaml_backends.Native;
  is_toplevel = false;
  is_native = false;
  exit_status_variabe = Ocaml_variables.ocamlopt_byte_exit_status;
  reference_variable = Ocaml_variables.compiler_reference;
  output_variable = Ocaml_variables.compiler_output;
}

let ocamlopt_opt =
{
  name = Ocaml_files.ocamlopt_dot_opt;
  flags = "";
  directory = "ocamlopt.opt";
  backend = Ocaml_backends.Native;
  is_toplevel = false;
  is_native = true;
  exit_status_variabe = Ocaml_variables.ocamlopt_opt_exit_status;
  reference_variable = Ocaml_variables.compiler_reference2;
  output_variable = Ocaml_variables.compiler_output2;
}

(* Top-levels *)

let ocaml = {
  name = Ocaml_commands.ocamlrun_ocaml;
  flags = "";
  directory = "ocaml";
  backend = Ocaml_backends.Bytecode;
  is_toplevel = true;
  is_native = false;
  exit_status_variabe = Ocaml_variables.ocaml_exit_status;
  reference_variable = Ocaml_variables.compiler_reference;
  output_variable = Ocaml_variables.compiler_output;
}

let ocamlnat = {
  name = Ocaml_files.ocamlnat;
  flags = "-S"; (* Keep intermediate assembly files *)
  directory = "ocamlnat";
  backend = Ocaml_backends.Native;
  is_toplevel = true;
  is_native = true;
  exit_status_variabe = Ocaml_variables.ocamlnat_exit_status;
  reference_variable = Ocaml_variables.compiler_reference2;
  output_variable = Ocaml_variables.compiler_output2;
}

let expected_exit_status env compiler =
  try int_of_string
    (Environments.safe_lookup compiler.exit_status_variabe env)
  with _ -> 0

let reference_filename env prefix compiler =
  let compiler_reference_suffix =
    Environments.safe_lookup Ocaml_variables.compiler_reference_suffix env in
  let suffix =
    if compiler_reference_suffix<>""
    then compiler_reference_suffix ^ ".reference"
    else ".reference" in
  let mk s = (Filename.make_filename prefix s) ^ suffix in
  let filename = mk compiler.directory in
  if Sys.file_exists filename then filename else
  let filename = mk
    (Ocaml_backends.string_of_backend compiler.backend) in
  if Sys.file_exists filename then filename else
  mk "compilers"
