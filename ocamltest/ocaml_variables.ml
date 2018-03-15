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

open Ocamltest_stdlib

open Variables (* Should not be necessary with a ppx *)

let all_modules = make ("all_modules",
  "All the modules to compile and link")

let c_preprocessor = make ("c_preprocessor",
  "Command to use to invoke the C preprocessor")

let caml_ld_library_path_name = "CAML_LD_LIBRARY_PATH"

let export_caml_ld_library_path value =
  let current_value = Sys.safe_getenv caml_ld_library_path_name in
  let local_value =
    (String.concat Filename.path_sep (String.words value)) in
  let new_value =
    if local_value="" then current_value else
    if current_value="" then local_value else
    String.concat Filename.path_sep [local_value; current_value] in
  Printf.sprintf "%s=%s" caml_ld_library_path_name new_value

let caml_ld_library_path =
  make_with_exporter
    export_caml_ld_library_path
    ("ld_library_path",
      "List of paths to lookup for loading dynamic libraries")

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

let compile_only = make ("compile_only",
  "Compile only (do not link)")

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

let module_ = make ("module",
  "Compile one module at once")

let modules = make ("modules",
  "Other modules of the test")

let ocamllex_flags = make ("ocamllex_flags",
  "Flags passed to ocamllex")

let ocamlopt_flags = make ("ocamlopt_flags",
  "Flags passed to ocamlopt.byte and ocamlopt.opt")

let ocamlopt_default_flags = make ("ocamlopt_default_flags",
  "Flags passed by default to ocamlopt.byte and ocamlopt.opt")

let ocamlyacc_flags = make ("ocamlyacc_flags",
  "Flags passed to ocamlyacc")

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

let export_ocamlrunparam value =
  Printf.sprintf "%s=%s" "OCAMLRUNPARAM" value

let ocamlrunparam =
  make_with_exporter
    export_ocamlrunparam
    ("ocamlrunparam",
      "Equivalent of OCAMLRUNPARAM")

let ocamlsrcdir = make ("ocamlsrcdir",
  "Where OCaml sources are")

let os_type = make ("os_type",
  "The OS we are running on")

let ocamldoc_flags = Variables.make ("ocamldoc_flags",
  "ocamldoc flags")

let ocamldoc_backend = Variables.make ("ocamldoc_backend",
  "ocamldoc backend (html, latex, man, ... )")

let ocamldoc_exit_status =
  Variables.make ( "ocamldoc_exit_status", "expected ocamldoc exit status")

let ocamldoc_output =
  Variables.make ( "ocamldoc_output", "Where to log ocamldoc output")

let ocamldoc_reference =
  Variables.make ( "ocamldoc_reference",
                   "Where to find expected ocamldoc output")


let plugins =
  Variables.make ( "plugins", "plugins for ocamlc,ocamlopt or ocamldoc" )


let _ = List.iter register_variable
  [
    all_modules;
    c_preprocessor;
    compare_programs;
    compiler_directory_suffix;
    compiler_reference;
    compiler_reference2;
    compiler_reference_suffix;
    compiler_output;
    compiler_output2;
    compile_only;
    directories;
    flags;
    libraries;
    module_;
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
    ocamlrunparam;
    os_type;
    ocamllex_flags;
    ocamlyacc_flags;
    ocamldoc_flags;
    ocamldoc_backend;
    ocamldoc_output;
    ocamldoc_reference;
    ocamldoc_exit_status;
    plugins
  ]
