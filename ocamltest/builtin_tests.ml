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

let bytecode = {
  test_name = "bytecode";
  test_run_by_default = true;
  test_actions = [Builtin_actions.bytecode_compile; Builtin_actions.execute]
}

let nativecode = {
  test_name = "nativecode";
  test_run_by_default = true;
  test_actions = [Builtin_actions.nativecode_compile; Builtin_actions.execute]
}

let _ =
  register bytecode;
  register nativecode
