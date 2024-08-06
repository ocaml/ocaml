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

(** Run LLDB debugger *)
val lldb : Actions.t

(** Run GDB debugger *)
val gdb : Actions.t

(** Run ocamldebug (bytecode) debugger *)
val ocamldebug : Actions.t
