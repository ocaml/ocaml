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

let native = {
  test_name = "native";
  test_run_by_default = true;
  test_actions =
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
  ]
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

let _ =
  List.iter register
  [
    bytecode;
    toplevel;
    expect;
  ];
  if (Ocamltest_config.arch <> "none") then register native
