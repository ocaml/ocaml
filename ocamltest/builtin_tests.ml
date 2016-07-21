(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris           *)
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
    compile_bytecode_with_nativecode_compiler;
    execute;
    check_program_output
  ]
}

let nativecode = {
  test_name = "nativecode";
  test_run_by_default = true;
  test_actions =
  [
    compile_nativecode_with_bytecode_compiler;
    compile_nativecode_with_nativecode_compiler;
    execute;
    check_program_output
  ]

}

let _ =
  register bytecode;
  register nativecode
