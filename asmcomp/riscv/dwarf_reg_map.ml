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

(** DWARF register number mapping for RISC-V architecture.

    This file is a stub that needs to be implemented with proper
    RISC-V DWARF register mappings according to the RISC-V ELF psABI.

    See: https://github.com/riscv-non-isa/riscv-elf-psabi-doc
*)

let to_dwarf_register backend_reg =
  failwith (Printf.sprintf
    "DWARF register mapping not implemented for RISC-V architecture.\n\
     Backend register %d cannot be mapped to DWARF register number.\n\
     \n\
     To fix this:\n\
     1. Consult the RISC-V ELF psABI DWARF register mapping specification\n\
     2. Implement the mapping in asmcomp/riscv/dwarf_reg_map.ml\n\
     3. Define frame_pointer_dwarf_register for RISC-V\n\
     \n\
     For reference implementations, see:\n\
     - asmcomp/amd64/dwarf_reg_map.ml\n\
     - asmcomp/arm64/dwarf_reg_map.ml"
    backend_reg)

let frame_pointer_dwarf_register =
  (* RISC-V typically uses s0/fp (x8) as frame pointer, which is DWARF register 8,
     but this needs to be verified against the actual psABI specification. *)
  failwith
    "DWARF frame pointer register not defined for RISC-V architecture.\n\
     Please implement proper RISC-V DWARF register mapping in asmcomp/riscv/dwarf_reg_map.ml"
