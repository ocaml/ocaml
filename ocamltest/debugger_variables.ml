(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                           Tim McGilchrist, Tarides                     *)
(*                                                                        *)
(*   Copyright 2024 Tarides.                                              *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let debugger_script = Variables.make ("debugger_script",
  "Where the debugger should read its commands")

let _ = List.iter Variables.register_variable
  [
    debugger_script;
  ]
