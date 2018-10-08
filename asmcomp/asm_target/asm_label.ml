(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Int of int
  | String of string

let create_int label = Int label
let create_string label = String label

let label_prefix =
  match Target_system.architecture () with
  | IA32 | X86_64 ->
    begin match Target_system.system () with
    | Linux
    | Windows Cygwin
    | Windows MinGW
    | FreeBSD
    | NetBSD
    | OpenBSD
    | Generic_BSD
    | Solaris
    | BeOS
    | GNU
    | Dragonfly
    | Unknown -> ".L"
    | MacOS_like
    | Windows Native -> "L"
    end
  | ARM
  | AArch64
  | POWER
  | Z -> ".L"

let encode (t : t) =
  match t with
  | Int label -> label_prefix ^ (string_of_int label)
  | String label -> label_prefix ^ label

let new_label_ref = ref None

let not_initialized () =
  Misc.fatal_error "[Asm_label.initialize] has not been called"

let initialize ~new_label =
  new_label_ref := Some new_label

let create () =
  match !new_label_ref with
  | None -> not_initialized ()
  | Some new_label -> create_int (new_label ())
