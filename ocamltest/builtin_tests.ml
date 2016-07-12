(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definitions of builtin tests *)

open Tests
open Builtin_actions

let bytecode = {
  test_name = "bytecode";
  test_run_by_default = true;
  test_actions =
  [
    compile_bytecode_with_bytecode_compiler;
    check_ocamlc_dot_byte_output;
    execute;
    check_program_output;
    compile_bytecode_with_nativecode_compiler;
    check_ocamlc_dot_opt_output;
    compare_bytecode_programs;
  ]
}

let nativecode = {
  test_name = "nativecode";
  test_run_by_default = true;
  test_actions =
  [
    compile_nativecode_with_bytecode_compiler;
    check_ocamlopt_dot_byte_output;
    execute;
    check_program_output;
    compile_nativecode_with_nativecode_compiler;
    check_ocamlopt_dot_opt_output;
    compare_nativecode_programs;
  ]
}

let script = {
  test_name = "script";
  test_run_by_default = false;
  test_actions = [script];
}

let toplevel = {
  test_name = "toplevel";
  test_run_by_default = false;
  test_actions =
  [
    run_in_bytecode_toplevel;
    check_bytecode_toplevel_output;
    run_in_nativecode_toplevel;
    check_nativecode_toplevel_output;
  ]
}

let _ =
  List.iter register
  [
    bytecode;
    nativecode;
    script;
    toplevel;
  ]
