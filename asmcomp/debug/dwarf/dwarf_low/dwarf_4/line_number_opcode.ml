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

type special_opcode = int

type t =
  | Standard of standard_opcode * int option
  | Extended of extended_opcode
  | Special of special_opcode

let standard_opcode_to_code = function
  | DW_LNS_copy -> 0x01
  | DW_LNS_advance_pc -> 0x02
  | DW_LNS_advance_line -> 0x03
  | DW_LNS_set_file -> 0x04
  | DW_LNS_set_column -> 0x05
  | DW_LNS_negate_stmt -> 0x06
  | DW_LNS_set_basic_block -> 0x07
  | DW_LNS_const_add_pc -> 0x08
  | DW_LNS_fixed_advance_pc -> 0x09
  | DW_LNS_set_prologue_end -> 0x0a
  | DW_LNS_set_epilogue_begin -> 0x0b
  | DW_LNS_set_isa -> 0x0c

let encode_extended buf address_size ext_op =
  match ext_op with
  | DW_LNE_end_sequence ->
      (* Length: 1, Opcode: 0x01 - encoded as ULEB128 *)
      let len_bytes = Leb128.encode_uleb128 1 in
      Buffer.add_bytes buf len_bytes;
      Buffer.add_char buf '\001'
  | DW_LNE_set_address addr ->
      (* Length: 1 (opcode) + address_size (from target architecture) *)
      let len_bytes = Leb128.encode_uleb128 (1 + address_size) in
      Buffer.add_bytes buf len_bytes;
      Buffer.add_char buf '\002'; (* DW_LNE_set_address *)
      begin match Code_address.absolute addr with
      | Some abs_addr ->
          let bytes = Bytes.create address_size in
          Bytes.set_int64_le bytes 0 abs_addr;
          Buffer.add_bytes buf bytes
      | None ->
          (* Label-based address - emit placeholder of correct size *)
          let placeholder = Bytes.create address_size in
          Buffer.add_bytes buf placeholder
      end
  | DW_LNE_define_file { name; dir_index; mtime; size } ->
      (* Calculate length first *)
      let dir_index_bytes = Leb128.encode_uleb128 dir_index in
      let mtime_bytes = Leb128.encode_uleb128 mtime in
      let size_bytes = Leb128.encode_uleb128 size in
      let data_len = 1 + (* opcode *)
                     String.length name + 1 + (* name + null *)
                     Bytes.length dir_index_bytes +
                     Bytes.length mtime_bytes +
                     Bytes.length size_bytes in
      (* Encode length as ULEB128 (can be > 255 for long filenames) *)
      let len_bytes = Leb128.encode_uleb128 data_len in
      Buffer.add_bytes buf len_bytes;
      Buffer.add_char buf '\003'; (* DW_LNE_define_file *)
      Buffer.add_string buf name;
      Buffer.add_char buf '\000';
      Buffer.add_bytes buf dir_index_bytes;
      Buffer.add_bytes buf mtime_bytes;
      Buffer.add_bytes buf size_bytes
  | DW_LNE_set_discriminator disc ->
      let disc_bytes = Leb128.encode_uleb128 disc in
      let len = 1 + Bytes.length disc_bytes in
      (* Encode length as ULEB128 *)
      let len_bytes = Leb128.encode_uleb128 len in
      Buffer.add_bytes buf len_bytes;
      Buffer.add_char buf '\004'; (* DW_LNE_set_discriminator *)
      Buffer.add_bytes buf disc_bytes

let encode address_size opcode =
  let buf = Buffer.create 16 in
  (match opcode with
  | Standard (std_op, operand_opt) ->
      Buffer.add_char buf (Char.chr (standard_opcode_to_code std_op));
      begin match operand_opt with
      | None -> ()
      | Some operand ->
          let operand_bytes =
            if std_op = DW_LNS_advance_line then
              Leb128.encode_sleb128 operand
            else
              Leb128.encode_uleb128 operand
          in
          Buffer.add_bytes buf operand_bytes
      end
  | Extended ext_op ->
      Buffer.add_char buf '\000'; (* Extended opcode prefix *)
      encode_extended buf address_size ext_op
  | Special opcode ->
      if opcode < 1 || opcode > 255 then
        invalid_arg "Special opcode out of range";
      Buffer.add_char buf (Char.chr opcode));
  Bytes.of_string (Buffer.contents buf)

let print ppf = function
  | Standard (op, operand) ->
      let op_name = match op with
        | DW_LNS_copy -> "DW_LNS_copy"
        | DW_LNS_advance_pc -> "DW_LNS_advance_pc"
        | DW_LNS_advance_line -> "DW_LNS_advance_line"
        | DW_LNS_set_file -> "DW_LNS_set_file"
        | DW_LNS_set_column -> "DW_LNS_set_column"
        | DW_LNS_negate_stmt -> "DW_LNS_negate_stmt"
        | DW_LNS_set_basic_block -> "DW_LNS_set_basic_block"
        | DW_LNS_const_add_pc -> "DW_LNS_const_add_pc"
        | DW_LNS_fixed_advance_pc -> "DW_LNS_fixed_advance_pc"
        | DW_LNS_set_prologue_end -> "DW_LNS_set_prologue_end"
        | DW_LNS_set_epilogue_begin -> "DW_LNS_set_epilogue_begin"
        | DW_LNS_set_isa -> "DW_LNS_set_isa"
      in
      begin match operand with
      | None -> Format.fprintf ppf "%s" op_name
      | Some n -> Format.fprintf ppf "%s(%d)" op_name n
      end
  | Extended ext_op ->
      begin match ext_op with
      | DW_LNE_end_sequence ->
          Format.fprintf ppf "DW_LNE_end_sequence"
      | DW_LNE_set_address addr ->
          Format.fprintf ppf "DW_LNE_set_address(%s)"
            (Code_address.to_string addr)
      | DW_LNE_define_file { name; dir_index; _ } ->
          Format.fprintf ppf "DW_LNE_define_file(%s, dir=%d)"
            name dir_index
      | DW_LNE_set_discriminator disc ->
          Format.fprintf ppf "DW_LNE_set_discriminator(%d)" disc
      end
  | Special opcode ->
      Format.fprintf ppf "Special(%d)" opcode
