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

(* Flags used in LLDB commands *)
val lldb_default_flags : string

(* Flags used in GDB commands *)
val gdb_default_flags : string

(* Flags used in Ocamldebug (bytecode debugger) commands *)
val ocamldebug_default_flags : string
