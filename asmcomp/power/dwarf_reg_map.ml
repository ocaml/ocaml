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

(** DWARF register number mapping for PowerPC architecture.

    This file is a stub that needs to be implemented with proper
    PowerPC DWARF register mappings according to the PowerPC ELF ABI.

    See: https://github.com/PowerPC-DWARF/powerpc-dwarf/blob/master/powerpc-dwarf-5-ABIv2.pdf
*)

let to_dwarf_register backend_reg =
  failwith (Printf.sprintf
    "DWARF register mapping not implemented for PowerPC architecture.\n\
     Backend register %d cannot be mapped to DWARF register number.\n\
     \n\
     To fix this:\n\
     1. Consult the PowerPC ELF ABI DWARF register mapping specification\n\
     2. Implement the mapping in asmcomp/power/dwarf_reg_map.ml\n\
     3. Define frame_pointer_dwarf_register for PowerPC\n\
     \n\
     For reference implementations, see:\n\
     - asmcomp/amd64/dwarf_reg_map.ml\n\
     - asmcomp/arm64/dwarf_reg_map.ml"
    backend_reg)

let frame_pointer_dwarf_register =
  (* PowerPC typically uses r31 as frame pointer, which is DWARF register 31,
     but this needs to be verified against the actual ABI specification. *)
  failwith
    "DWARF frame pointer register not defined for PowerPC architecture.\n\
     Please implement proper PowerPC DWARF register mapping in asmcomp/power/dwarf_reg_map.ml"
