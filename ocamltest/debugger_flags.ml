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

let ocamldebug_default_flags =
  "-no-version -no-prompt -no-time -no-breakpoint-message " ^
    ("-I " ^ Ocaml_directories.stdlib ^ " ")

let lldb_default_flags = "--no-use-colors"

let gdb_default_flags = "--quiet --batch"
