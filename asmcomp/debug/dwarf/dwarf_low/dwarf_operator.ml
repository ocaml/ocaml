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

type t =
  | DW_OP_addr | DW_OP_deref
  | DW_OP_const1u | DW_OP_const1s | DW_OP_const2u | DW_OP_const2s
  | DW_OP_const4u | DW_OP_const4s | DW_OP_const8u | DW_OP_const8s
  | DW_OP_constu | DW_OP_consts
  | DW_OP_dup | DW_OP_drop | DW_OP_over | DW_OP_pick | DW_OP_swap | DW_OP_rot
  | DW_OP_xderef | DW_OP_abs | DW_OP_and | DW_OP_div | DW_OP_minus | DW_OP_mod
  | DW_OP_mul | DW_OP_neg | DW_OP_not | DW_OP_or | DW_OP_plus | DW_OP_plus_uconst
  | DW_OP_shl | DW_OP_shr | DW_OP_shra | DW_OP_xor
  | DW_OP_skip | DW_OP_bra
  | DW_OP_eq | DW_OP_ge | DW_OP_gt | DW_OP_le | DW_OP_lt | DW_OP_ne
  | DW_OP_fbreg
  | DW_OP_breg0  | DW_OP_breg1  | DW_OP_breg2  | DW_OP_breg3
  | DW_OP_breg4  | DW_OP_breg5  | DW_OP_breg6  | DW_OP_breg7
  | DW_OP_breg8  | DW_OP_breg9  | DW_OP_breg10 | DW_OP_breg11
  | DW_OP_breg12 | DW_OP_breg13 | DW_OP_breg14 | DW_OP_breg15
  | DW_OP_breg16 | DW_OP_breg17 | DW_OP_breg18 | DW_OP_breg19
  | DW_OP_breg20 | DW_OP_breg21 | DW_OP_breg22 | DW_OP_breg23
  | DW_OP_breg24 | DW_OP_breg25 | DW_OP_breg26 | DW_OP_breg27
  | DW_OP_breg28 | DW_OP_breg29 | DW_OP_breg30 | DW_OP_breg31
  | DW_OP_regx | DW_OP_bregx
  | DW_OP_lit0  | DW_OP_lit1  | DW_OP_lit2  | DW_OP_lit3
  | DW_OP_lit4  | DW_OP_lit5  | DW_OP_lit6  | DW_OP_lit7
  | DW_OP_lit8  | DW_OP_lit9  | DW_OP_lit10 | DW_OP_lit11
  | DW_OP_lit12 | DW_OP_lit13 | DW_OP_lit14 | DW_OP_lit15
  | DW_OP_lit16 | DW_OP_lit17 | DW_OP_lit18 | DW_OP_lit19
  | DW_OP_lit20 | DW_OP_lit21 | DW_OP_lit22 | DW_OP_lit23
  | DW_OP_lit24 | DW_OP_lit25 | DW_OP_lit26 | DW_OP_lit27
  | DW_OP_lit28 | DW_OP_lit29 | DW_OP_lit30 | DW_OP_lit31
  | DW_OP_reg0  | DW_OP_reg1  | DW_OP_reg2  | DW_OP_reg3
  | DW_OP_reg4  | DW_OP_reg5  | DW_OP_reg6  | DW_OP_reg7
  | DW_OP_reg8  | DW_OP_reg9  | DW_OP_reg10 | DW_OP_reg11
  | DW_OP_reg12 | DW_OP_reg13 | DW_OP_reg14 | DW_OP_reg15
  | DW_OP_reg16 | DW_OP_reg17 | DW_OP_reg18 | DW_OP_reg19
  | DW_OP_reg20 | DW_OP_reg21 | DW_OP_reg22 | DW_OP_reg23
  | DW_OP_reg24 | DW_OP_reg25 | DW_OP_reg26 | DW_OP_reg27
  | DW_OP_reg28 | DW_OP_reg29 | DW_OP_reg30 | DW_OP_reg31
  | DW_OP_nop | DW_OP_piece | DW_OP_deref_size | DW_OP_xderef_size
  | DW_OP_push_object_address | DW_OP_call2 | DW_OP_call4 | DW_OP_call_ref
  | DW_OP_form_tls_address | DW_OP_call_frame_cfa | DW_OP_bit_piece
  | DW_OP_implicit_value | DW_OP_stack_value
  | DW_OP_implicit_pointer | DW_OP_addrx | DW_OP_constx | DW_OP_entry_value
  | DW_OP_const_type | DW_OP_regval_type | DW_OP_deref_type | DW_OP_xderef_type
  | DW_OP_convert | DW_OP_reinterpret

(* DWARF 4 specification section 7.7.1 *)
let to_code = function
  | DW_OP_addr -> 0x03
  | DW_OP_deref -> 0x06
  | DW_OP_const1u -> 0x08 | DW_OP_const1s -> 0x09
  | DW_OP_const2u -> 0x0a | DW_OP_const2s -> 0x0b
  | DW_OP_const4u -> 0x0c | DW_OP_const4s -> 0x0d
  | DW_OP_const8u -> 0x0e | DW_OP_const8s -> 0x0f
  | DW_OP_constu -> 0x10 | DW_OP_consts -> 0x11
  | DW_OP_dup -> 0x12 | DW_OP_drop -> 0x13
  | DW_OP_over -> 0x14 | DW_OP_pick -> 0x15
  | DW_OP_swap -> 0x16 | DW_OP_rot -> 0x17
  | DW_OP_xderef -> 0x18
  | DW_OP_abs -> 0x19 | DW_OP_and -> 0x1a | DW_OP_div -> 0x1b
  | DW_OP_minus -> 0x1c | DW_OP_mod -> 0x1d | DW_OP_mul -> 0x1e
  | DW_OP_neg -> 0x1f | DW_OP_not -> 0x20 | DW_OP_or -> 0x21
  | DW_OP_plus -> 0x22 | DW_OP_plus_uconst -> 0x23
  | DW_OP_shl -> 0x24 | DW_OP_shr -> 0x25 | DW_OP_shra -> 0x26
  | DW_OP_xor -> 0x27
  | DW_OP_skip -> 0x2f | DW_OP_bra -> 0x28
  | DW_OP_eq -> 0x29 | DW_OP_ge -> 0x2a | DW_OP_gt -> 0x2b
  | DW_OP_le -> 0x2c | DW_OP_lt -> 0x2d | DW_OP_ne -> 0x2e
  | DW_OP_lit0  -> 0x30 | DW_OP_lit1  -> 0x31 | DW_OP_lit2  -> 0x32 | DW_OP_lit3  -> 0x33
  | DW_OP_lit4  -> 0x34 | DW_OP_lit5  -> 0x35 | DW_OP_lit6  -> 0x36 | DW_OP_lit7  -> 0x37
  | DW_OP_lit8  -> 0x38 | DW_OP_lit9  -> 0x39 | DW_OP_lit10 -> 0x3a | DW_OP_lit11 -> 0x3b
  | DW_OP_lit12 -> 0x3c | DW_OP_lit13 -> 0x3d | DW_OP_lit14 -> 0x3e | DW_OP_lit15 -> 0x3f
  | DW_OP_lit16 -> 0x40 | DW_OP_lit17 -> 0x41 | DW_OP_lit18 -> 0x42 | DW_OP_lit19 -> 0x43
  | DW_OP_lit20 -> 0x44 | DW_OP_lit21 -> 0x45 | DW_OP_lit22 -> 0x46 | DW_OP_lit23 -> 0x47
  | DW_OP_lit24 -> 0x48 | DW_OP_lit25 -> 0x49 | DW_OP_lit26 -> 0x4a | DW_OP_lit27 -> 0x4b
  | DW_OP_lit28 -> 0x4c | DW_OP_lit29 -> 0x4d | DW_OP_lit30 -> 0x4e | DW_OP_lit31 -> 0x4f
  | DW_OP_reg0  -> 0x50 | DW_OP_reg1  -> 0x51 | DW_OP_reg2  -> 0x52 | DW_OP_reg3  -> 0x53
  | DW_OP_reg4  -> 0x54 | DW_OP_reg5  -> 0x55 | DW_OP_reg6  -> 0x56 | DW_OP_reg7  -> 0x57
  | DW_OP_reg8  -> 0x58 | DW_OP_reg9  -> 0x59 | DW_OP_reg10 -> 0x5a | DW_OP_reg11 -> 0x5b
  | DW_OP_reg12 -> 0x5c | DW_OP_reg13 -> 0x5d | DW_OP_reg14 -> 0x5e | DW_OP_reg15 -> 0x5f
  | DW_OP_reg16 -> 0x60 | DW_OP_reg17 -> 0x61 | DW_OP_reg18 -> 0x62 | DW_OP_reg19 -> 0x63
  | DW_OP_reg20 -> 0x64 | DW_OP_reg21 -> 0x65 | DW_OP_reg22 -> 0x66 | DW_OP_reg23 -> 0x67
  | DW_OP_reg24 -> 0x68 | DW_OP_reg25 -> 0x69 | DW_OP_reg26 -> 0x6a | DW_OP_reg27 -> 0x6b
  | DW_OP_reg28 -> 0x6c | DW_OP_reg29 -> 0x6d | DW_OP_reg30 -> 0x6e | DW_OP_reg31 -> 0x6f
  | DW_OP_breg0  -> 0x70 | DW_OP_breg1  -> 0x71 | DW_OP_breg2  -> 0x72 | DW_OP_breg3  -> 0x73
  | DW_OP_breg4  -> 0x74 | DW_OP_breg5  -> 0x75 | DW_OP_breg6  -> 0x76 | DW_OP_breg7  -> 0x77
  | DW_OP_breg8  -> 0x78 | DW_OP_breg9  -> 0x79 | DW_OP_breg10 -> 0x7a | DW_OP_breg11 -> 0x7b
  | DW_OP_breg12 -> 0x7c | DW_OP_breg13 -> 0x7d | DW_OP_breg14 -> 0x7e | DW_OP_breg15 -> 0x7f
  | DW_OP_breg16 -> 0x80 | DW_OP_breg17 -> 0x81 | DW_OP_breg18 -> 0x82 | DW_OP_breg19 -> 0x83
  | DW_OP_breg20 -> 0x84 | DW_OP_breg21 -> 0x85 | DW_OP_breg22 -> 0x86 | DW_OP_breg23 -> 0x87
  | DW_OP_breg24 -> 0x88 | DW_OP_breg25 -> 0x89 | DW_OP_breg26 -> 0x8a | DW_OP_breg27 -> 0x8b
  | DW_OP_breg28 -> 0x8c | DW_OP_breg29 -> 0x8d | DW_OP_breg30 -> 0x8e | DW_OP_breg31 -> 0x8f
  | DW_OP_regx -> 0x90 | DW_OP_fbreg -> 0x91 | DW_OP_bregx -> 0x92
  | DW_OP_piece -> 0x93 | DW_OP_deref_size -> 0x94 | DW_OP_xderef_size -> 0x95
  | DW_OP_nop -> 0x96
  | DW_OP_push_object_address -> 0x97
  | DW_OP_call2 -> 0x98 | DW_OP_call4 -> 0x99 | DW_OP_call_ref -> 0x9a
  | DW_OP_form_tls_address -> 0x9b
  | DW_OP_call_frame_cfa -> 0x9c
  | DW_OP_bit_piece -> 0x9d
  | DW_OP_implicit_value -> 0x9e
  | DW_OP_stack_value -> 0x9f
  (* DWARF 5 *)
  | DW_OP_implicit_pointer -> 0xa0
  | DW_OP_addrx -> 0xa1 | DW_OP_constx -> 0xa2
  | DW_OP_entry_value -> 0xa3
  | DW_OP_const_type -> 0xa4
  | DW_OP_regval_type -> 0xa5
  | DW_OP_deref_type -> 0xa6 | DW_OP_xderef_type -> 0xa7
  | DW_OP_convert -> 0xa8 | DW_OP_reinterpret -> 0xa9

let to_string op =
  match op with
  | DW_OP_addr -> "DW_OP_addr" | DW_OP_deref -> "DW_OP_deref"
  | DW_OP_const1u -> "DW_OP_const1u" | DW_OP_const1s -> "DW_OP_const1s"
  | DW_OP_const2u -> "DW_OP_const2u" | DW_OP_const2s -> "DW_OP_const2s"
  | DW_OP_const4u -> "DW_OP_const4u" | DW_OP_const4s -> "DW_OP_const4s"
  | DW_OP_const8u -> "DW_OP_const8u" | DW_OP_const8s -> "DW_OP_const8s"
  | DW_OP_constu -> "DW_OP_constu" | DW_OP_consts -> "DW_OP_consts"
  | DW_OP_dup -> "DW_OP_dup" | DW_OP_drop -> "DW_OP_drop"
  | DW_OP_over -> "DW_OP_over" | DW_OP_pick -> "DW_OP_pick"
  | DW_OP_swap -> "DW_OP_swap" | DW_OP_rot -> "DW_OP_rot"
  | DW_OP_xderef -> "DW_OP_xderef"
  | DW_OP_abs -> "DW_OP_abs" | DW_OP_and -> "DW_OP_and" | DW_OP_div -> "DW_OP_div"
  | DW_OP_minus -> "DW_OP_minus" | DW_OP_mod -> "DW_OP_mod" | DW_OP_mul -> "DW_OP_mul"
  | DW_OP_neg -> "DW_OP_neg" | DW_OP_not -> "DW_OP_not" | DW_OP_or -> "DW_OP_or"
  | DW_OP_plus -> "DW_OP_plus" | DW_OP_plus_uconst -> "DW_OP_plus_uconst"
  | DW_OP_shl -> "DW_OP_shl" | DW_OP_shr -> "DW_OP_shr" | DW_OP_shra -> "DW_OP_shra"
  | DW_OP_xor -> "DW_OP_xor"
  | DW_OP_skip -> "DW_OP_skip" | DW_OP_bra -> "DW_OP_bra"
  | DW_OP_eq -> "DW_OP_eq" | DW_OP_ge -> "DW_OP_ge" | DW_OP_gt -> "DW_OP_gt"
  | DW_OP_le -> "DW_OP_le" | DW_OP_lt -> "DW_OP_lt" | DW_OP_ne -> "DW_OP_ne"
  | DW_OP_fbreg -> "DW_OP_fbreg"
  | DW_OP_breg0  -> "DW_OP_breg0"  | DW_OP_breg1  -> "DW_OP_breg1"
  | DW_OP_breg2  -> "DW_OP_breg2"  | DW_OP_breg3  -> "DW_OP_breg3"
  | DW_OP_breg4  -> "DW_OP_breg4"  | DW_OP_breg5  -> "DW_OP_breg5"
  | DW_OP_breg6  -> "DW_OP_breg6"  | DW_OP_breg7  -> "DW_OP_breg7"
  | DW_OP_breg8  -> "DW_OP_breg8"  | DW_OP_breg9  -> "DW_OP_breg9"
  | DW_OP_breg10 -> "DW_OP_breg10" | DW_OP_breg11 -> "DW_OP_breg11"
  | DW_OP_breg12 -> "DW_OP_breg12" | DW_OP_breg13 -> "DW_OP_breg13"
  | DW_OP_breg14 -> "DW_OP_breg14" | DW_OP_breg15 -> "DW_OP_breg15"
  | DW_OP_breg16 -> "DW_OP_breg16" | DW_OP_breg17 -> "DW_OP_breg17"
  | DW_OP_breg18 -> "DW_OP_breg18" | DW_OP_breg19 -> "DW_OP_breg19"
  | DW_OP_breg20 -> "DW_OP_breg20" | DW_OP_breg21 -> "DW_OP_breg21"
  | DW_OP_breg22 -> "DW_OP_breg22" | DW_OP_breg23 -> "DW_OP_breg23"
  | DW_OP_breg24 -> "DW_OP_breg24" | DW_OP_breg25 -> "DW_OP_breg25"
  | DW_OP_breg26 -> "DW_OP_breg26" | DW_OP_breg27 -> "DW_OP_breg27"
  | DW_OP_breg28 -> "DW_OP_breg28" | DW_OP_breg29 -> "DW_OP_breg29"
  | DW_OP_breg30 -> "DW_OP_breg30" | DW_OP_breg31 -> "DW_OP_breg31"
  | DW_OP_regx -> "DW_OP_regx" | DW_OP_bregx -> "DW_OP_bregx"
  | DW_OP_lit0  -> "DW_OP_lit0"  | DW_OP_lit1  -> "DW_OP_lit1"
  | DW_OP_lit2  -> "DW_OP_lit2"  | DW_OP_lit3  -> "DW_OP_lit3"
  | DW_OP_lit4  -> "DW_OP_lit4"  | DW_OP_lit5  -> "DW_OP_lit5"
  | DW_OP_lit6  -> "DW_OP_lit6"  | DW_OP_lit7  -> "DW_OP_lit7"
  | DW_OP_lit8  -> "DW_OP_lit8"  | DW_OP_lit9  -> "DW_OP_lit9"
  | DW_OP_lit10 -> "DW_OP_lit10" | DW_OP_lit11 -> "DW_OP_lit11"
  | DW_OP_lit12 -> "DW_OP_lit12" | DW_OP_lit13 -> "DW_OP_lit13"
  | DW_OP_lit14 -> "DW_OP_lit14" | DW_OP_lit15 -> "DW_OP_lit15"
  | DW_OP_lit16 -> "DW_OP_lit16" | DW_OP_lit17 -> "DW_OP_lit17"
  | DW_OP_lit18 -> "DW_OP_lit18" | DW_OP_lit19 -> "DW_OP_lit19"
  | DW_OP_lit20 -> "DW_OP_lit20" | DW_OP_lit21 -> "DW_OP_lit21"
  | DW_OP_lit22 -> "DW_OP_lit22" | DW_OP_lit23 -> "DW_OP_lit23"
  | DW_OP_lit24 -> "DW_OP_lit24" | DW_OP_lit25 -> "DW_OP_lit25"
  | DW_OP_lit26 -> "DW_OP_lit26" | DW_OP_lit27 -> "DW_OP_lit27"
  | DW_OP_lit28 -> "DW_OP_lit28" | DW_OP_lit29 -> "DW_OP_lit29"
  | DW_OP_lit30 -> "DW_OP_lit30" | DW_OP_lit31 -> "DW_OP_lit31"
  | DW_OP_reg0  -> "DW_OP_reg0"  | DW_OP_reg1  -> "DW_OP_reg1"
  | DW_OP_reg2  -> "DW_OP_reg2"  | DW_OP_reg3  -> "DW_OP_reg3"
  | DW_OP_reg4  -> "DW_OP_reg4"  | DW_OP_reg5  -> "DW_OP_reg5"
  | DW_OP_reg6  -> "DW_OP_reg6"  | DW_OP_reg7  -> "DW_OP_reg7"
  | DW_OP_reg8  -> "DW_OP_reg8"  | DW_OP_reg9  -> "DW_OP_reg9"
  | DW_OP_reg10 -> "DW_OP_reg10" | DW_OP_reg11 -> "DW_OP_reg11"
  | DW_OP_reg12 -> "DW_OP_reg12" | DW_OP_reg13 -> "DW_OP_reg13"
  | DW_OP_reg14 -> "DW_OP_reg14" | DW_OP_reg15 -> "DW_OP_reg15"
  | DW_OP_reg16 -> "DW_OP_reg16" | DW_OP_reg17 -> "DW_OP_reg17"
  | DW_OP_reg18 -> "DW_OP_reg18" | DW_OP_reg19 -> "DW_OP_reg19"
  | DW_OP_reg20 -> "DW_OP_reg20" | DW_OP_reg21 -> "DW_OP_reg21"
  | DW_OP_reg22 -> "DW_OP_reg22" | DW_OP_reg23 -> "DW_OP_reg23"
  | DW_OP_reg24 -> "DW_OP_reg24" | DW_OP_reg25 -> "DW_OP_reg25"
  | DW_OP_reg26 -> "DW_OP_reg26" | DW_OP_reg27 -> "DW_OP_reg27"
  | DW_OP_reg28 -> "DW_OP_reg28" | DW_OP_reg29 -> "DW_OP_reg29"
  | DW_OP_reg30 -> "DW_OP_reg30" | DW_OP_reg31 -> "DW_OP_reg31"
  | DW_OP_nop -> "DW_OP_nop" | DW_OP_piece -> "DW_OP_piece"
  | DW_OP_deref_size -> "DW_OP_deref_size" | DW_OP_xderef_size -> "DW_OP_xderef_size"
  | DW_OP_push_object_address -> "DW_OP_push_object_address"
  | DW_OP_call2 -> "DW_OP_call2" | DW_OP_call4 -> "DW_OP_call4"
  | DW_OP_call_ref -> "DW_OP_call_ref"
  | DW_OP_form_tls_address -> "DW_OP_form_tls_address"
  | DW_OP_call_frame_cfa -> "DW_OP_call_frame_cfa"
  | DW_OP_bit_piece -> "DW_OP_bit_piece"
  | DW_OP_implicit_value -> "DW_OP_implicit_value"
  | DW_OP_stack_value -> "DW_OP_stack_value"
  | DW_OP_implicit_pointer -> "DW_OP_implicit_pointer"
  | DW_OP_addrx -> "DW_OP_addrx" | DW_OP_constx -> "DW_OP_constx"
  | DW_OP_entry_value -> "DW_OP_entry_value"
  | DW_OP_const_type -> "DW_OP_const_type"
  | DW_OP_regval_type -> "DW_OP_regval_type"
  | DW_OP_deref_type -> "DW_OP_deref_type" | DW_OP_xderef_type -> "DW_OP_xderef_type"
  | DW_OP_convert -> "DW_OP_convert" | DW_OP_reinterpret -> "DW_OP_reinterpret"

let print ppf op =
  Format.fprintf ppf "%s" (to_string op)
