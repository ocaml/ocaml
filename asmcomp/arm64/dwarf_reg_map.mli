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

(** ARM64 DWARF register number mappings.

    Maps OCaml backend register numbers to DWARF register numbers
    as defined by the AARCH64 DWARF specification. *)

(** Convert OCaml backend register number to DWARF register number.

    @param backend_reg OCaml backend register number (0-27 for int, 100-131 for float)
    @return DWARF register number according to AARCH64 ABI
    @raise Invalid_argument if the register number is out of range *)
val to_dwarf_register : int -> int

(** Get the register name for debugging purposes.

    @param backend_reg OCaml backend register number
    @return Human-readable register name (e.g., "x0", "d15") *)
val register_name : int -> string

(** DWARF register number for the frame pointer (x29 on ARM64).

    This is used to emit DW_AT_frame_base attributes in subprogram DIEs,
    allowing DW_OP_fbreg expressions to work correctly for stack-based
    parameters and local variables. *)
val frame_pointer_dwarf_register : int
