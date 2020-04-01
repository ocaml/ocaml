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
  let opt_actions =
  [
    setup_ocamlc_opt_build_env;
    ocamlc_opt;
    check_ocamlc_opt_output;
    compare_bytecode_programs
  ] in
{
  test_name = "bytecode";
  test_run_by_default = true;
  test_actions =
  [
    setup_ocamlc_byte_build_env;
    ocamlc_byte;
    check_ocamlc_byte_output;
    run;
    check_program_output;
  ] @ (if Ocamltest_config.arch<>"none" then opt_actions else [])
}

let native =
  let opt_actions =
  [
    setup_ocamlopt_byte_build_env;
    ocamlopt_byte;
    check_ocamlopt_byte_output;
    run;
    check_program_output;
    setup_ocamlopt_opt_build_env;
    ocamlopt_opt;
    check_ocamlopt_opt_output;
    compare_native_programs;
  ] in
  {
    test_name = "native";
    test_run_by_default = true;
    test_actions =
      (if Ocamltest_config.arch<>"none" then opt_actions else [skip])
  }

let toplevel = {
  test_name = "toplevel";
  test_run_by_default = false;
  test_actions =
  [
    setup_ocaml_build_env;
    ocaml;
    check_ocaml_output;
(*
    setup_ocamlnat_build_env;
    ocamlnat;
    check_ocamlnat_output;
*)
  ]
}

let expect =
{
  test_name = "expect";
  test_run_by_default = false;
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

let asmgen_skip_on_spacetime =
  Actions_helpers.skip_with_reason "not ported to Spacetime yet"

let msvc64 =
  Ocamltest_config.ccomptype = "msvc" && Ocamltest_config.arch="amd64"

let asmgen_skip_on_msvc64 =
  Actions_helpers.skip_with_reason "not ported to MSVC64 yet"

let asmgen_actions =
  if Ocamltest_config.arch="none" then [asmgen_skip_on_bytecode_only]
  else if Ocamltest_config.spacetime then [asmgen_skip_on_spacetime]
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
  test_actions = asmgen_actions
}

let _ =
  List.iter register
  [
    bytecode;
    native;
    toplevel;
    expect;
    ocamldoc;
    asmgen;
  ]
