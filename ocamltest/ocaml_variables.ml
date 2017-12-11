(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of variables used by built-in actions *)

(* The variables are listed in alphabetical order *)

(*
  The name of the identifier representing a variable and its string name
  should be similar. Is there a way to enforce this?
*)

open Variables (* Should not be necessary with a ppx *)

let c_preprocessor = make ("c_preprocessor",
  "Command to use to invoke the C preprocessor")

let compare_programs = make ("compare_programs",
  "Set to \"false\" to disable program comparison")

let compiler_directory_suffix = make ("compiler_directory_suffix",
  "Suffix to add to the directory where the test will be compiled")

let compiler_reference = make ("compiler_reference",
  "Reference file for compiler output for ocamlc.byte and ocamlopt.byte")

let compiler_reference2 = make ("compiler_reference2",
  "Reference file for compiler output for ocamlc.opt and ocamlopt.opt")

let compiler_reference_suffix = make ("compiler_reference_suffix",
  "Suffix to add to the file name containing the reference for compiler output")

let compiler_output = make ("compiler_output",
  "Where to log output of bytecode compilers")

let compiler_output2 = make ("compiler_output2",
  "Where to log output of native compilers")

let ocamlc_flags = make ("ocamlc_flags",
  "Flags passed to ocamlc.byte and ocamlc.opt")

let ocamlc_default_flags = make ("ocamlc_default_flags",
  "Flags passed by default to ocamlc.byte and ocamlc.opt")

let directories = make ("directories",
  "Directories to include by all the compilers")

let flags = make ("flags",
  "Flags passed to all the compilers")

let libraries = make ("libraries",
  "Libraries the program should be linked with")

let modules = make ("modules",
  "Other modules of the test")

let ocamlopt_flags = make ("ocamlopt_flags",
  "Flags passed to ocamlopt.byte and ocamlopt.opt")

let ocamlopt_default_flags = make ("ocamlopt_default_flags",
  "Flags passed by default to ocamlopt.byte and ocamlopt.opt")

let ocaml_exit_status = make ("ocaml_exit_status",
  "Expected exit status of ocaml")

let ocamlc_byte_exit_status = make ("ocamlc_byte_exit_status",
  "Expected exit status of ocac.byte")

let ocamlopt_byte_exit_status = make ("ocamlopt_byte_exit_status",
  "Expected exit status of ocamlopt.byte")

let ocamlnat_exit_status = make ("ocamlnat_exit_status",
  "Expected exit status of ocamlnat")

let ocamlc_opt_exit_status = make ("ocamlc_opt_exit_status",
  "Expected exit status of ocac.opt")

let ocamlopt_opt_exit_status = make ("ocamlopt_opt_exit_status",
  "Expected exit status of ocamlopt.opt")

let ocamlsrcdir = make ("ocamlsrcdir",
  "Where OCaml sources are")

let os_type = make ("os_type",
  "The OS we are running on")

let source_modules = make ("source_modules",
  "Complete list of modules (private)")

let _ = List.iter register_variable
  [
    c_preprocessor;
    compare_programs;
    compiler_directory_suffix;
    compiler_reference;
    compiler_reference2;
    compiler_reference_suffix;
    compiler_output;
    compiler_output2;
    directories;
    flags;
    libraries;
    modules;
    ocamlc_flags;
    ocamlc_default_flags;
    ocamlopt_flags;
    ocamlopt_default_flags;
    ocaml_exit_status;
    ocamlc_byte_exit_status;
    ocamlopt_byte_exit_status;
    ocamlnat_exit_status;
    ocamlc_opt_exit_status;
    ocamlopt_opt_exit_status;
    os_type;
    (* source_modules is intentionally not registered *)
  ]
