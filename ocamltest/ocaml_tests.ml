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

(* Tests specific to the OCaml compiler *)

open Tests
open Builtin_actions
open Ocaml_actions

let bytecode =
  let byte_build =
  [
    setup_ocamlc_byte_build_env;
    ocamlc_byte;
    check_ocamlc_byte_output
  ] in
  let opt_build =
  [
    setup_ocamlc_opt_build_env;
    ocamlc_opt;
    check_ocamlc_opt_output
  ] in
{
  test_name = "bytecode";
  test_run_by_default = true;
  test_description = "Build bytecode program, run it and check its output";
  test_actions =
  (if Sys.win32 && Ocamltest_config.native_compiler then
    opt_build
  else
    byte_build) @
  [
    run;
    check_program_output;
  ] @
  (if not Sys.win32 && Ocamltest_config.native_compiler then
    opt_build @ [compare_bytecode_programs]
  else
    []
  )
}

let native =
  let byte_build =
  [
    setup_ocamlopt_byte_build_env;
    ocamlopt_byte;
    check_ocamlopt_byte_output;
  ] in
  let opt_build =
  [
    setup_ocamlopt_opt_build_env;
    ocamlopt_opt;
    check_ocamlopt_opt_output;
  ] in
  let opt_actions =
  (if Sys.win32 then
    opt_build
  else
    byte_build
  ) @
  [
    run;
    check_program_output;
  ] @
  (if not Sys.win32 then
    opt_build
  else
    []
  ) in
  {
    test_name = "native";
    test_run_by_default = true;
  test_description = "Build native program, run it and check its output";
    test_actions =
      (if Ocamltest_config.native_compiler then opt_actions else [skip])
  }

let toplevel = {
  test_name = "toplevel";
  test_run_by_default = false;
  test_description =
    "Run the program in the OCaml toplevel and check its output";
  test_actions =
  [
    setup_ocaml_build_env;
    ocaml;
    check_ocaml_output;
  ]
}

let nattoplevel = {
  test_name = "toplevel.opt";
  test_run_by_default = false;
  test_description =
    "Run the program in the native OCaml toplevel (ocamlnat) and check its \
     output";
  test_actions =
  [
    shared_libraries;
    setup_ocamlnat_build_env;
    ocamlnat;
    check_ocamlnat_output;
  ]
}

let expect =
{
  test_name = "expect";
  test_run_by_default = false;
  test_description =
    "Run expect tests in the program in the OCaml toplevel and check their \
     output";
  test_actions =
  [
    setup_simple_build_env;
    run_expect;
    check_program_output
  ]
}

let ocamldoc =
{
  test_name = "ocamldoc";
  test_run_by_default = false;
  test_description = "Run ocamldoc on the test and compare with the reference";
  test_actions =
  if  Ocamltest_config.ocamldoc then
  [
    shared_libraries;
    setup_ocamldoc_build_env;
    run_ocamldoc;
    check_program_output;
    check_ocamldoc_output
  ]
  else
  [ skip ]
}

let asmgen_skip_on_bytecode_only =
  Actions_helpers.skip_with_reason "native compiler disabled"

let msvc64 =
  Ocamltest_config.ccomptype = "msvc" && Ocamltest_config.arch="amd64"

let asmgen_skip_on_msvc64 =
  Actions_helpers.skip_with_reason "not ported to MSVC64 yet"

let asmgen_actions =
  if not Ocamltest_config.native_compiler then [asmgen_skip_on_bytecode_only]
  else if msvc64 then [asmgen_skip_on_msvc64]
  else [
    setup_simple_build_env;
    codegen;
    cc;
  ]

let asmgen =
{
  test_name = "asmgen";
  test_run_by_default = false;
  test_description =
    "Generate the assembly for the test program; and also use the C compiler \
     to make the executable";
  test_actions = asmgen_actions
}

let _ =
  List.iter register
  [
    bytecode;
    native;
    toplevel;
    nattoplevel;
    expect;
    ocamldoc;
    asmgen;
  ]
