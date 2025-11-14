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

(** DWARF 4 line number program opcodes.

    The line number program is a sequence of opcodes that drive a
    state machine to generate the mapping between addresses and
    source positions. *)

(** Standard opcodes *)
type standard_opcode =
  | DW_LNS_copy
  | DW_LNS_advance_pc
  | DW_LNS_advance_line
  | DW_LNS_set_file
  | DW_LNS_set_column
  | DW_LNS_negate_stmt
  | DW_LNS_set_basic_block
  | DW_LNS_const_add_pc
  | DW_LNS_fixed_advance_pc
  | DW_LNS_set_prologue_end
  | DW_LNS_set_epilogue_begin
  | DW_LNS_set_isa

(** Extended opcodes *)
type extended_opcode =
  | DW_LNE_end_sequence
  | DW_LNE_set_address of Code_address.t
  | DW_LNE_define_file of {
      name: string;
      dir_index: int;
      mtime: int;
      size: int;
    }
  | DW_LNE_set_discriminator of int

(** Special opcodes (computed, not enumerated) *)
type special_opcode = int

(** All opcode types *)
type t =
  | Standard of standard_opcode * int option  (* opcode, optional operand *)
  | Extended of extended_opcode
  | Special of special_opcode

(** Get the byte code for a standard opcode *)
val standard_opcode_to_code : standard_opcode -> int

(** Encode an opcode to bytes.
    @param address_size Size of addresses in bytes (4 for 32-bit, 8 for 64-bit)
    @param opcode The opcode to encode *)
val encode : int -> t -> bytes

(** Pretty-printer *)
val print : Format.formatter -> t -> unit
