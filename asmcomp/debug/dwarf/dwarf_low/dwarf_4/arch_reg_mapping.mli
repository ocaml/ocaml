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

(** Architecture-specific register mapping indirection. *)

type register_mapper = int -> int

(** Set the register mapping function.
    Should be called during initialization by architecture-specific code. *)
val set_mapper : register_mapper -> unit

(** Convert backend register number to DWARF register number.
    Uses the mapper set by set_mapper, or identity mapping if not set. *)
val to_dwarf_register : int -> int

(** Set the frame pointer DWARF register number for this architecture.
    Should be called during initialization by architecture-specific code. *)
val set_frame_pointer_register : int -> unit

(** Get the frame pointer DWARF register number.
    Returns the value set by set_frame_pointer_register, or a default if not set. *)
val get_frame_pointer_register : unit -> int
