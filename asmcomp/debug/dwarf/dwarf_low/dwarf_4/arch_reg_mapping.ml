(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Architecture-specific register mapping indirection.

    This module provides a callback-based interface for mapping backend
    register numbers to DWARF register numbers. The actual mapping is
    provided by architecture-specific code at initialization time.

    This indirection avoids compilation order issues where DWARF modules
    are compiled before architecture-specific modules. *)

type register_mapper = int -> int

let mapper : register_mapper ref =
  ref (fun backend_reg ->
    (* Default implementation: identity mapping *)
    backend_reg)

let set_mapper f =
  mapper := f

let to_dwarf_register backend_reg =
  !mapper backend_reg

(** Frame pointer DWARF register number.
    Set by architecture-specific initialization code. *)
let frame_pointer_register : int ref = ref 6  (* Default to AMD64 rbp *)

let set_frame_pointer_register reg =
  frame_pointer_register := reg

let get_frame_pointer_register () =
  !frame_pointer_register
