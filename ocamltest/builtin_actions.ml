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

(* Definition of a few builtin actions *)

open Actions

let noop env = Pass env

let bytecode_compile = {
  action_name = "bytecode_compile";
  action_body = noop
}

let nativecode_compile = {
  action_name = "nativecode_compile";
  action_body = noop
}

let execute = {
  action_name = "execute";
  action_body = noop
}

let reg act = register act.action_name act.action_body

let _ =
  reg bytecode_compile;
  reg nativecode_compile;
  reg execute
