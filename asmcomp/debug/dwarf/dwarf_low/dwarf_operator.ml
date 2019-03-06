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

module Int8 = Numbers.Int8
module Int16 = Numbers.Int16

module Uint8 = Numbers.Uint8
module Uint16 = Numbers.Uint16
module Uint32 = Numbers.Uint32
module Uint64 = Numbers.Uint64

module I = Dwarf_int
module V = Dwarf_value

type implicit_value =
  | Int of Targetint.t
  | Symbol of Asm_symbol.t

type t =
  | DW_op_lit0
  | DW_op_lit1
  | DW_op_lit2
  | DW_op_lit3
  | DW_op_lit4
  | DW_op_lit5
  | DW_op_lit6
  | DW_op_lit7
  | DW_op_lit8
  | DW_op_lit9
  | DW_op_lit10
  | DW_op_lit11
  | DW_op_lit12
  | DW_op_lit13
  | DW_op_lit14
  | DW_op_lit15
  | DW_op_lit16
  | DW_op_lit17
  | DW_op_lit18
  | DW_op_lit19
  | DW_op_lit20
  | DW_op_lit21
  | DW_op_lit22
  | DW_op_lit23
  | DW_op_lit24
  | DW_op_lit25
  | DW_op_lit26
  | DW_op_lit27
  | DW_op_lit28
  | DW_op_lit29
  | DW_op_lit30
  | DW_op_lit31
  | DW_op_addr of implicit_value
  | DW_op_const1u of Uint8.t
  | DW_op_const2u of Uint16.t
  | DW_op_const4u of Uint32.t
  | DW_op_const8u of Uint64.t
  | DW_op_const1s of Int8.t
  | DW_op_const2s of Int16.t
  | DW_op_const4s of Int32.t
  | DW_op_const8s of Int64.t
  | DW_op_constu of Uint64.t
  | DW_op_consts of Int64.t
  | DW_op_fbreg of { offset_in_bytes : Targetint.t; }
  | DW_op_breg0 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg1 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg2 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg3 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg4 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg5 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg6 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg7 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg8 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg9 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg10 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg11 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg12 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg13 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg14 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg15 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg16 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg17 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg18 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg19 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg20 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg21 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg22 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg23 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg24 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg25 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg26 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg27 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg28 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg29 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg30 of { offset_in_bytes : Targetint.t; }
  | DW_op_breg31 of { offset_in_bytes : Targetint.t; }
  | DW_op_bregx of { reg_number : int; offset_in_bytes : Targetint.t; }
  | DW_op_dup
  | DW_op_drop
  | DW_op_pick
  | DW_op_over
  | DW_op_swap
  | DW_op_rot
  | DW_op_deref
  | DW_op_deref_size of Uint8.t
  | DW_op_xderef
  | DW_op_xderef_size of Uint8.t
  | DW_op_push_object_address
  | DW_op_form_tls_address
  | DW_op_call_frame_cfa
  | DW_op_abs
  | DW_op_and
  | DW_op_div
  | DW_op_minus
  | DW_op_mod
  | DW_op_mul
  | DW_op_neg
  | DW_op_not
  | DW_op_or
  | DW_op_plus
  | DW_op_plus_uconst of Uint64.t
  | DW_op_shl
  | DW_op_shr
  | DW_op_shra
  | DW_op_xor
  | DW_op_le
  | DW_op_ge
  | DW_op_eq
  | DW_op_lt
  | DW_op_gt
  | DW_op_ne
  | DW_op_skip of { num_bytes_forward : Int16.t; }
  | DW_op_bra of { num_bytes_forward : Int16.t; }
  | DW_op_call2 of {
      label : Asm_label.t;
      compilation_unit_header_label : Asm_label.t;
    }
  | DW_op_call4 of {
      label : Asm_label.t;
      compilation_unit_header_label : Asm_label.t;
    }
  | DW_op_call_ref of {
      label : Asm_label.t;
      compilation_unit_header_label : Asm_label.t;
    }
  | DW_op_nop
  | DW_op_reg0
  | DW_op_reg1
  | DW_op_reg2
  | DW_op_reg3
  | DW_op_reg4
  | DW_op_reg5
  | DW_op_reg6
  | DW_op_reg7
  | DW_op_reg8
  | DW_op_reg9
  | DW_op_reg10
  | DW_op_reg11
  | DW_op_reg12
  | DW_op_reg13
  | DW_op_reg14
  | DW_op_reg15
  | DW_op_reg16
  | DW_op_reg17
  | DW_op_reg18
  | DW_op_reg19
  | DW_op_reg20
  | DW_op_reg21
  | DW_op_reg22
  | DW_op_reg23
  | DW_op_reg24
  | DW_op_reg25
  | DW_op_reg26
  | DW_op_reg27
  | DW_op_reg28
  | DW_op_reg29
  | DW_op_reg30
  | DW_op_reg31
  | DW_op_regx of { reg_number : int; }
  | DW_op_implicit_value of implicit_value
  | DW_op_stack_value
  | DW_op_piece of { size_in_bytes : Targetint.t; }
  | DW_op_bit_piece of {
      size_in_bits : Targetint.t;
      offset_in_bits : Targetint.t;
    }
  | DW_op_implicit_pointer of {
      label : Asm_label.t;
      offset_in_bytes : Targetint.t;
    }
  | DW_op_GNU_implicit_pointer of {
      label : Asm_label.t;
      offset_in_bytes : Targetint.t;
    }

let opcode_name t =
  match t with
  | DW_op_lit0 -> "DW_op_lit0"
  | DW_op_lit1 -> "DW_op_lit1"
  | DW_op_lit2 -> "DW_op_lit2"
  | DW_op_lit3 -> "DW_op_lit3"
  | DW_op_lit4 -> "DW_op_lit4"
  | DW_op_lit5 -> "DW_op_lit5"
  | DW_op_lit6 -> "DW_op_lit6"
  | DW_op_lit7 -> "DW_op_lit7"
  | DW_op_lit8 -> "DW_op_lit8"
  | DW_op_lit9 -> "DW_op_lit9"
  | DW_op_lit10 -> "DW_op_lit10"
  | DW_op_lit11 -> "DW_op_lit11"
  | DW_op_lit12 -> "DW_op_lit12"
  | DW_op_lit13 -> "DW_op_lit13"
  | DW_op_lit14 -> "DW_op_lit14"
  | DW_op_lit15 -> "DW_op_lit15"
  | DW_op_lit16 -> "DW_op_lit16"
  | DW_op_lit17 -> "DW_op_lit17"
  | DW_op_lit18 -> "DW_op_lit18"
  | DW_op_lit19 -> "DW_op_lit19"
  | DW_op_lit20 -> "DW_op_lit20"
  | DW_op_lit21 -> "DW_op_lit21"
  | DW_op_lit22 -> "DW_op_lit22"
  | DW_op_lit23 -> "DW_op_lit23"
  | DW_op_lit24 -> "DW_op_lit24"
  | DW_op_lit25 -> "DW_op_lit25"
  | DW_op_lit26 -> "DW_op_lit26"
  | DW_op_lit27 -> "DW_op_lit27"
  | DW_op_lit28 -> "DW_op_lit28"
  | DW_op_lit29 -> "DW_op_lit29"
  | DW_op_lit30 -> "DW_op_lit30"
  | DW_op_lit31 -> "DW_op_lit31"
  | DW_op_addr _ -> "DW_op_addr"
  | DW_op_const1u _ -> "DW_op_const1u"
  | DW_op_const2u _ -> "DW_op_const2u"
  | DW_op_const4u _ -> "DW_op_const4u"
  | DW_op_const8u _ -> "DW_op_const8u"
  | DW_op_const1s _ -> "DW_op_const1s"
  | DW_op_const2s _ -> "DW_op_const2s"
  | DW_op_const4s _ -> "DW_op_const4s"
  | DW_op_const8s _ -> "DW_op_const8s"
  | DW_op_constu _ -> "DW_op_constu"
  | DW_op_consts _ -> "DW_op_consts"
  | DW_op_fbreg _ -> "DW_op_fbreg"
  | DW_op_breg0 _ -> "DW_op_breg0"
  | DW_op_breg1 _ -> "DW_op_breg1"
  | DW_op_breg2 _ -> "DW_op_breg2"
  | DW_op_breg3 _ -> "DW_op_breg3"
  | DW_op_breg4 _ -> "DW_op_breg4"
  | DW_op_breg5 _ -> "DW_op_breg5"
  | DW_op_breg6 _ -> "DW_op_breg6"
  | DW_op_breg7 _ -> "DW_op_breg7"
  | DW_op_breg8 _ -> "DW_op_breg8"
  | DW_op_breg9 _ -> "DW_op_breg9"
  | DW_op_breg10 _ -> "DW_op_breg10"
  | DW_op_breg11 _ -> "DW_op_breg11"
  | DW_op_breg12 _ -> "DW_op_breg12"
  | DW_op_breg13 _ -> "DW_op_breg13"
  | DW_op_breg14 _ -> "DW_op_breg14"
  | DW_op_breg15 _ -> "DW_op_breg15"
  | DW_op_breg16 _ -> "DW_op_breg16"
  | DW_op_breg17 _ -> "DW_op_breg17"
  | DW_op_breg18 _ -> "DW_op_breg18"
  | DW_op_breg19 _ -> "DW_op_breg19"
  | DW_op_breg20 _ -> "DW_op_breg20"
  | DW_op_breg21 _ -> "DW_op_breg21"
  | DW_op_breg22 _ -> "DW_op_breg22"
  | DW_op_breg23 _ -> "DW_op_breg23"
  | DW_op_breg24 _ -> "DW_op_breg24"
  | DW_op_breg25 _ -> "DW_op_breg25"
  | DW_op_breg26 _ -> "DW_op_breg26"
  | DW_op_breg27 _ -> "DW_op_breg27"
  | DW_op_breg28 _ -> "DW_op_breg28"
  | DW_op_breg29 _ -> "DW_op_breg29"
  | DW_op_breg30 _ -> "DW_op_breg30"
  | DW_op_breg31 _ -> "DW_op_breg31"
  | DW_op_bregx _ -> "DW_op_bregx"
  | DW_op_dup -> "DW_op_dup"
  | DW_op_drop -> "DW_op_drop"
  | DW_op_pick -> "DW_op_pick"
  | DW_op_over -> "DW_op_over"
  | DW_op_swap -> "DW_op_swap"
  | DW_op_rot -> "DW_op_rot"
  | DW_op_deref -> "DW_op_deref"
  | DW_op_deref_size _-> "DW_op_deref_size"
  | DW_op_xderef -> "DW_op_xderef"
  | DW_op_xderef_size _ -> "DW_op_xderef_size"
  | DW_op_push_object_address -> "DW_op_push_object_address"
  | DW_op_form_tls_address -> "DW_op_form_tls_address"
  | DW_op_call_frame_cfa -> "DW_op_call_frame_cfa"
  | DW_op_abs -> "DW_op_abs"
  | DW_op_and -> "DW_op_and"
  | DW_op_div -> "DW_op_div"
  | DW_op_minus -> "DW_op_minus"
  | DW_op_mod -> "DW_op_mod"
  | DW_op_mul -> "DW_op_mul"
  | DW_op_neg -> "DW_op_neg"
  | DW_op_not -> "DW_op_not"
  | DW_op_or -> "DW_op_or"
  | DW_op_plus -> "DW_op_plus"
  | DW_op_plus_uconst _ -> "DW_op_plus_uconst"
  | DW_op_shl -> "DW_op_shl"
  | DW_op_shr -> "DW_op_shr"
  | DW_op_shra -> "DW_op_shra"
  | DW_op_xor -> "DW_op_xor"
  | DW_op_le -> "DW_op_le"
  | DW_op_ge -> "DW_op_ge"
  | DW_op_eq -> "DW_op_eq"
  | DW_op_lt -> "DW_op_lt"
  | DW_op_gt -> "DW_op_gt"
  | DW_op_ne -> "DW_op_ne"
  | DW_op_skip _ -> "DW_op_skip"
  | DW_op_bra _ -> "DW_op_bra"
  | DW_op_call2 _ -> "DW_op_call2"
  | DW_op_call4 _ -> "DW_op_call4"
  | DW_op_call_ref _ -> "DW_op_call_ref"
  | DW_op_nop -> "DW_op_nop"
  | DW_op_reg0 -> "DW_op_reg0"
  | DW_op_reg1 -> "DW_op_reg1"
  | DW_op_reg2 -> "DW_op_reg2"
  | DW_op_reg3 -> "DW_op_reg3"
  | DW_op_reg4 -> "DW_op_reg4"
  | DW_op_reg5 -> "DW_op_reg5"
  | DW_op_reg6 -> "DW_op_reg6"
  | DW_op_reg7 -> "DW_op_reg7"
  | DW_op_reg8 -> "DW_op_reg8"
  | DW_op_reg9 -> "DW_op_reg9"
  | DW_op_reg10 -> "DW_op_reg10"
  | DW_op_reg11 -> "DW_op_reg11"
  | DW_op_reg12 -> "DW_op_reg12"
  | DW_op_reg13 -> "DW_op_reg13"
  | DW_op_reg14 -> "DW_op_reg14"
  | DW_op_reg15 -> "DW_op_reg15"
  | DW_op_reg16 -> "DW_op_reg16"
  | DW_op_reg17 -> "DW_op_reg17"
  | DW_op_reg18 -> "DW_op_reg18"
  | DW_op_reg19 -> "DW_op_reg19"
  | DW_op_reg20 -> "DW_op_reg20"
  | DW_op_reg21 -> "DW_op_reg21"
  | DW_op_reg22 -> "DW_op_reg22"
  | DW_op_reg23 -> "DW_op_reg23"
  | DW_op_reg24 -> "DW_op_reg24"
  | DW_op_reg25 -> "DW_op_reg25"
  | DW_op_reg26 -> "DW_op_reg26"
  | DW_op_reg27 -> "DW_op_reg27"
  | DW_op_reg28 -> "DW_op_reg28"
  | DW_op_reg29 -> "DW_op_reg29"
  | DW_op_reg30 -> "DW_op_reg30"
  | DW_op_reg31 -> "DW_op_reg31"
  | DW_op_regx _ -> "DW_op_regx"
  | DW_op_implicit_value _ -> "DW_op_implicit_value"
  | DW_op_stack_value -> "DW_op_stack_value"
  | DW_op_piece _ -> "DW_op_piece"
  | DW_op_bit_piece _ -> "DW_op_bit_piece"
  | DW_op_implicit_pointer _ -> "DW_op_implicit_pointer"
  | DW_op_GNU_implicit_pointer _ -> "DW_op_GNU_implicit_pointer"

(* DWARF-4 spec section 7.7.1. *)
let opcode = function
  | DW_op_lit0 -> 0x30
  | DW_op_lit1 -> 0x31
  | DW_op_lit2 -> 0x32
  | DW_op_lit3 -> 0x33
  | DW_op_lit4 -> 0x34
  | DW_op_lit5 -> 0x35
  | DW_op_lit6 -> 0x36
  | DW_op_lit7 -> 0x37
  | DW_op_lit8 -> 0x38
  | DW_op_lit9 -> 0x39
  | DW_op_lit10 -> 0x3a
  | DW_op_lit11 -> 0x3b
  | DW_op_lit12 -> 0x3c
  | DW_op_lit13 -> 0x3d
  | DW_op_lit14 -> 0x3e
  | DW_op_lit15 -> 0x3f
  | DW_op_lit16 -> 0x40
  | DW_op_lit17 -> 0x41
  | DW_op_lit18 -> 0x42
  | DW_op_lit19 -> 0x43
  | DW_op_lit20 -> 0x44
  | DW_op_lit21 -> 0x45
  | DW_op_lit22 -> 0x46
  | DW_op_lit23 -> 0x47
  | DW_op_lit24 -> 0x48
  | DW_op_lit25 -> 0x49
  | DW_op_lit26 -> 0x4a
  | DW_op_lit27 -> 0x4b
  | DW_op_lit28 -> 0x4c
  | DW_op_lit29 -> 0x4d
  | DW_op_lit30 -> 0x4e
  | DW_op_lit31 -> 0x4f
  | DW_op_addr _ -> 0x03
  | DW_op_const1u _ -> 0x08
  | DW_op_const2u _ -> 0x0a
  | DW_op_const4u _ -> 0x0c
  | DW_op_const8u _ -> 0x0e
  | DW_op_const1s _ -> 0x09
  | DW_op_const2s _ -> 0x0b
  | DW_op_const4s _ -> 0x0d
  | DW_op_const8s _ -> 0x0f
  | DW_op_constu _ -> 0x10
  | DW_op_consts _ -> 0x11
  | DW_op_fbreg _ -> 0x91
  | DW_op_breg0 _ -> 0x70
  | DW_op_breg1 _ -> 0x71
  | DW_op_breg2 _ -> 0x72
  | DW_op_breg3 _ -> 0x73
  | DW_op_breg4 _ -> 0x74
  | DW_op_breg5 _ -> 0x75
  | DW_op_breg6 _ -> 0x76
  | DW_op_breg7 _ -> 0x77
  | DW_op_breg8 _ -> 0x78
  | DW_op_breg9 _ -> 0x79
  | DW_op_breg10 _ -> 0x7a
  | DW_op_breg11 _ -> 0x7b
  | DW_op_breg12 _ -> 0x7c
  | DW_op_breg13 _ -> 0x7d
  | DW_op_breg14 _ -> 0x7e
  | DW_op_breg15 _ -> 0x7f
  | DW_op_breg16 _ -> 0x80
  | DW_op_breg17 _ -> 0x81
  | DW_op_breg18 _ -> 0x82
  | DW_op_breg19 _ -> 0x83
  | DW_op_breg20 _ -> 0x84
  | DW_op_breg21 _ -> 0x85
  | DW_op_breg22 _ -> 0x86
  | DW_op_breg23 _ -> 0x87
  | DW_op_breg24 _ -> 0x88
  | DW_op_breg25 _ -> 0x89
  | DW_op_breg26 _ -> 0x8a
  | DW_op_breg27 _ -> 0x8b
  | DW_op_breg28 _ -> 0x8c
  | DW_op_breg29 _ -> 0x8d
  | DW_op_breg30 _ -> 0x8e
  | DW_op_breg31 _ -> 0x8f
  | DW_op_bregx _ -> 0x92
  | DW_op_dup -> 0x12
  | DW_op_drop -> 0x13
  | DW_op_pick -> 0x15
  | DW_op_over -> 0x14
  | DW_op_swap -> 0x16
  | DW_op_rot -> 0x17
  | DW_op_deref -> 0x06
  | DW_op_deref_size _ -> 0x94
  | DW_op_xderef -> 0x18
  | DW_op_xderef_size _ -> 0x95
  | DW_op_push_object_address -> 0x97
  | DW_op_form_tls_address -> 0x9b
  | DW_op_call_frame_cfa -> 0x9c
  | DW_op_abs -> 0x19
  | DW_op_and -> 0x1a
  | DW_op_div -> 0x1b
  | DW_op_minus -> 0x1c
  | DW_op_mod -> 0x1d
  | DW_op_mul -> 0x1e
  | DW_op_neg -> 0x1f
  | DW_op_not -> 0x20
  | DW_op_or -> 0x21
  | DW_op_plus -> 0x22
  | DW_op_plus_uconst _ -> 0x23
  | DW_op_shl -> 0x24
  | DW_op_shr -> 0x25
  | DW_op_shra -> 0x26
  | DW_op_xor -> 0x27
  | DW_op_le -> 0x2c
  | DW_op_ge -> 0x2a
  | DW_op_eq -> 0x29
  | DW_op_lt -> 0x2d
  | DW_op_gt -> 0x2b
  | DW_op_ne -> 0x2e
  | DW_op_skip _ -> 0x2f
  | DW_op_bra _ -> 0x28
  | DW_op_call2 _ -> 0x98
  | DW_op_call4 _ -> 0x99
  | DW_op_call_ref _ -> 0x9a
  | DW_op_nop -> 0x96
  | DW_op_reg0 -> 0x50
  | DW_op_reg1 -> 0x51
  | DW_op_reg2 -> 0x52
  | DW_op_reg3 -> 0x53
  | DW_op_reg4 -> 0x54
  | DW_op_reg5 -> 0x55
  | DW_op_reg6 -> 0x56
  | DW_op_reg7 -> 0x57
  | DW_op_reg8 -> 0x58
  | DW_op_reg9 -> 0x59
  | DW_op_reg10 -> 0x5a
  | DW_op_reg11 -> 0x5b
  | DW_op_reg12 -> 0x5c
  | DW_op_reg13 -> 0x5d
  | DW_op_reg14 -> 0x5e
  | DW_op_reg15 -> 0x5f
  | DW_op_reg16 -> 0x60
  | DW_op_reg17 -> 0x61
  | DW_op_reg18 -> 0x62
  | DW_op_reg19 -> 0x63
  | DW_op_reg20 -> 0x64
  | DW_op_reg21 -> 0x65
  | DW_op_reg22 -> 0x66
  | DW_op_reg23 -> 0x67
  | DW_op_reg24 -> 0x68
  | DW_op_reg25 -> 0x69
  | DW_op_reg26 -> 0x6a
  | DW_op_reg27 -> 0x6b
  | DW_op_reg28 -> 0x6c
  | DW_op_reg29 -> 0x6d
  | DW_op_reg30 -> 0x6e
  | DW_op_reg31 -> 0x6f
  | DW_op_regx _ -> 0x90
  | DW_op_implicit_value _ -> 0x9e
  | DW_op_stack_value -> 0x9f
  | DW_op_piece _ -> 0x93
  | DW_op_bit_piece _ -> 0x9d
  | DW_op_implicit_pointer _ -> 0xa0
  | DW_op_GNU_implicit_pointer _ -> 0xf2

external caml_string_set32 : bytes -> index:int -> Int32.t -> unit
  = "%caml_string_set32"

external caml_string_set64 : bytes -> index:int -> Int64.t -> unit
  = "%caml_string_set64"

module Make (M : sig
  type param
  type result
  val unit_result : unit -> result
  val opcode : param -> t -> result
  val value : param -> V.t -> result
  val (>>>) : param -> result -> (unit -> result) -> result
end) = struct
  let run param t =
    let unit_result = M.unit_result () in
    let opcode = M.opcode param in
    let value = M.value param in
    let (>>>) = M.(>>>) param in
    opcode t
    >>> fun () ->
    match t with
    | DW_op_lit0
    | DW_op_lit1
    | DW_op_lit2
    | DW_op_lit3
    | DW_op_lit4
    | DW_op_lit5
    | DW_op_lit6
    | DW_op_lit7
    | DW_op_lit8
    | DW_op_lit9
    | DW_op_lit10
    | DW_op_lit11
    | DW_op_lit12
    | DW_op_lit13
    | DW_op_lit14
    | DW_op_lit15
    | DW_op_lit16
    | DW_op_lit17
    | DW_op_lit18
    | DW_op_lit19
    | DW_op_lit20
    | DW_op_lit21
    | DW_op_lit22
    | DW_op_lit23
    | DW_op_lit24
    | DW_op_lit25
    | DW_op_lit26
    | DW_op_lit27
    | DW_op_lit28
    | DW_op_lit29
    | DW_op_lit30
    | DW_op_lit31 -> unit_result
    | DW_op_addr (Int addr) -> value (V.absolute_address addr)
    | DW_op_addr (Symbol sym) -> value (V.code_address_from_symbol sym)
    | DW_op_const1u n -> value (V.uint8 ~comment:"  arg of DW_OP_const1u" n)
    | DW_op_const2u n -> value (V.uint16 ~comment:"  arg of DW_OP_const2u" n)
    | DW_op_const4u n -> value (V.uint32 ~comment:"  arg of DW_OP_const4u" n)
    | DW_op_const8u n -> value (V.uint64 ~comment:"  arg of DW_OP_const8u" n)
    | DW_op_const1s n -> value (V.int8 ~comment:"  arg of DW_OP_const1s" n)
    | DW_op_const2s n -> value (V.int16 ~comment:"  arg of DW_OP_const2s" n)
    | DW_op_const4s n -> value (V.int32 ~comment:"  arg of DW_OP_const4s" n)
    | DW_op_const8s n -> value (V.int64 ~comment:"  arg of DW_OP_const8s" n)
    | DW_op_constu n -> value (V.uleb128 ~comment:"  arg of DW_OP_constu" n)
    | DW_op_consts n -> value (V.sleb128 ~comment:"  arg of DW_OP_consts" n)
    | DW_op_fbreg { offset_in_bytes; } ->
      let offset_in_bytes = Targetint.to_int64 offset_in_bytes in
      value (V.sleb128 ~comment:"offset in bytes" offset_in_bytes)
    | DW_op_breg0 { offset_in_bytes; }
    | DW_op_breg1 { offset_in_bytes; }
    | DW_op_breg2 { offset_in_bytes; }
    | DW_op_breg3 { offset_in_bytes; }
    | DW_op_breg4 { offset_in_bytes; }
    | DW_op_breg5 { offset_in_bytes; }
    | DW_op_breg6 { offset_in_bytes; }
    | DW_op_breg7 { offset_in_bytes; }
    | DW_op_breg8 { offset_in_bytes; }
    | DW_op_breg9 { offset_in_bytes; }
    | DW_op_breg10 { offset_in_bytes; }
    | DW_op_breg11 { offset_in_bytes; }
    | DW_op_breg12 { offset_in_bytes; }
    | DW_op_breg13 { offset_in_bytes; }
    | DW_op_breg14 { offset_in_bytes; }
    | DW_op_breg15 { offset_in_bytes; }
    | DW_op_breg16 { offset_in_bytes; }
    | DW_op_breg17 { offset_in_bytes; }
    | DW_op_breg18 { offset_in_bytes; }
    | DW_op_breg19 { offset_in_bytes; }
    | DW_op_breg20 { offset_in_bytes; }
    | DW_op_breg21 { offset_in_bytes; }
    | DW_op_breg22 { offset_in_bytes; }
    | DW_op_breg23 { offset_in_bytes; }
    | DW_op_breg24 { offset_in_bytes; }
    | DW_op_breg25 { offset_in_bytes; }
    | DW_op_breg26 { offset_in_bytes; }
    | DW_op_breg27 { offset_in_bytes; }
    | DW_op_breg28 { offset_in_bytes; }
    | DW_op_breg29 { offset_in_bytes; }
    | DW_op_breg30 { offset_in_bytes; }
    | DW_op_breg31 { offset_in_bytes; } ->
      let offset_in_bytes = Targetint.to_int64 offset_in_bytes in
      value (V.sleb128 ~comment:"offset in bytes" offset_in_bytes)
    | DW_op_bregx { reg_number; offset_in_bytes; } ->
      let offset_in_bytes = Targetint.to_int64 offset_in_bytes in
      value (V.uleb128 ~comment:"DWARF reg number"
        (Uint64.of_int_exn reg_number))
      >>> fun () ->
      value (V.sleb128 ~comment:"offset in bytes" offset_in_bytes)
    | DW_op_dup
    | DW_op_drop
    | DW_op_pick
    | DW_op_over
    | DW_op_swap
    | DW_op_rot
    | DW_op_deref
    | DW_op_deref_size _
    | DW_op_xderef -> unit_result
    | DW_op_xderef_size n -> value (V.uint8 ~comment:"size" n)
    | DW_op_push_object_address
    | DW_op_form_tls_address
    | DW_op_call_frame_cfa
    | DW_op_abs
    | DW_op_and
    | DW_op_div
    | DW_op_minus
    | DW_op_mod
    | DW_op_mul
    | DW_op_neg
    | DW_op_not
    | DW_op_or
    | DW_op_plus -> unit_result
    | DW_op_plus_uconst const -> value (V.uleb128 const)
    | DW_op_shl
    | DW_op_shr
    | DW_op_shra
    | DW_op_xor
    | DW_op_le
    | DW_op_ge
    | DW_op_eq
    | DW_op_lt
    | DW_op_gt
    | DW_op_ne -> unit_result
    | DW_op_skip { num_bytes_forward; } ->
      value (V.int16 ~comment:"num bytes to jump forward/backward"
        num_bytes_forward)
    | DW_op_bra { num_bytes_forward; } ->
      value (V.int16 ~comment:"num bytes forward/backward, if TOS non-zero"
        num_bytes_forward)
    | DW_op_call2 { label; compilation_unit_header_label; } ->
      value (V.distance_between_labels_16_bit ~comment:"call2 target"
        ~upper:label ~lower:compilation_unit_header_label ())
    | DW_op_call4 { label; compilation_unit_header_label; } ->
      value (V.distance_between_labels_32_bit ~comment:"call4 target"
        ~upper:label ~lower:compilation_unit_header_label ())
    | DW_op_call_ref { label; compilation_unit_header_label; } ->
      begin match Dwarf_format.get () with
      | Thirty_two ->
        value (V.distance_between_labels_32_bit ~comment:"call_ref target"
          ~upper:label ~lower:compilation_unit_header_label ())
      | Sixty_four ->
        value (V.distance_between_labels_64_bit ~comment:"call_ref target"
          ~upper:label ~lower:compilation_unit_header_label ())
      end
    | DW_op_nop
    | DW_op_reg0
    | DW_op_reg1
    | DW_op_reg2
    | DW_op_reg3
    | DW_op_reg4
    | DW_op_reg5
    | DW_op_reg6
    | DW_op_reg7
    | DW_op_reg8
    | DW_op_reg9
    | DW_op_reg10
    | DW_op_reg11
    | DW_op_reg12
    | DW_op_reg13
    | DW_op_reg14
    | DW_op_reg15
    | DW_op_reg16
    | DW_op_reg17
    | DW_op_reg18
    | DW_op_reg19
    | DW_op_reg20
    | DW_op_reg21
    | DW_op_reg22
    | DW_op_reg23
    | DW_op_reg24
    | DW_op_reg25
    | DW_op_reg26
    | DW_op_reg27
    | DW_op_reg28
    | DW_op_reg29
    | DW_op_reg30
    | DW_op_reg31 -> unit_result
    | DW_op_regx { reg_number; } ->
      value (V.uleb128 ~comment:"DWARF reg number"
        (Uint64.of_int_exn reg_number))
    | DW_op_implicit_value (Int i) ->
      let buf =
        match Arch.size_int with
        | 4 ->
          let buf = Bytes.create 4 in
          caml_string_set32 buf ~index:0 (Targetint.to_int32 i);
          buf
        | 8 ->
          let buf = Bytes.create 8 in
          caml_string_set64 buf ~index:0 (Targetint.to_int64 i);
          buf
        | n ->
          Misc.fatal_errorf "Dwarf_operator: bad Arch.size_int = %d" n
      in
      let comment =
        if !Clflags.keep_asm_file then
          Some (Format.asprintf "implicit value %a" Targetint.print i)
        else
          None
      in
      value (V.uleb128 ?comment (Uint64.of_int_exn (Bytes.length buf)))
      >>> fun () ->
      value (V.string (Bytes.to_string buf))
    | DW_op_implicit_value (Symbol symbol) ->
      value (V.uleb128 ~comment:"Arch.size_addr"
        (Uint64.of_int_exn Arch.size_addr))
      >>> fun () ->
      value (V.code_address_from_symbol symbol)
    | DW_op_stack_value -> unit_result
    | DW_op_piece { size_in_bytes; } ->
      let size_in_bytes = Targetint.to_uint64_exn size_in_bytes in
      value (V.uleb128 ~comment:"size in bytes" size_in_bytes)
    | DW_op_bit_piece { size_in_bits; offset_in_bits; } ->
      let size_in_bits = Targetint.to_uint64_exn size_in_bits in
      let offset_in_bits = Targetint.to_uint64_exn offset_in_bits in
      value (V.uleb128 ~comment:"size in bits" size_in_bits)
      >>> fun () ->
      value (V.uleb128 ~comment:"offset in bits" offset_in_bits)
    | DW_op_implicit_pointer { offset_in_bytes; label; }
    | DW_op_GNU_implicit_pointer { offset_in_bytes; label; } ->
      let offset_in_bytes = Targetint.to_int64 offset_in_bytes in
      value (V.offset_into_debug_info label)
      >>> fun () ->
      value (V.sleb128 ~comment:"offset in bytes" offset_in_bytes)
end

module Print = Make (struct
  type param = Format.formatter
  type result = unit
  let unit_result () = ()

  let opcode ppf t = Format.pp_print_string ppf (opcode_name t)
  let value ppf v = V.print ppf v
  let (>>>) ppf () f = Format.pp_print_string ppf " "; f ()
end)

module Size = Make (struct
  type param = unit
  type result = I.t
  let unit_result () = I.zero ()

  let opcode () _ = I.one ()
  let value () v = V.size v
  let (>>>) () size f = I.add size (f ())
end)

module Emit = Make (struct
  type param = unit
  type result = unit
  let unit_result () = ()

  let opcode () t =
    let comment = opcode_name t in
    V.emit (V.uint8 ~comment (Uint8.of_int_exn (opcode t)))
  let value () v = V.emit v
  let (>>>) () () f = f ()
end)

let print ppf t = Print.run ppf t
let size t = Size.run () t
let emit t = Emit.run () t
