(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type implicit_value =
  | Int of Targetint.t
  | Symbol of Asm_symbol.t

(* CR mshinwell: Remove "DW_op" prefix to be consistent *)
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
  | DW_op_const1u of Numbers.Uint8.t
  | DW_op_const2u of Numbers.Uint16.t
  | DW_op_const4u of Numbers.Uint32.t
  | DW_op_const8u of Numbers.Uint64.t
  | DW_op_const1s of Numbers.Int8.t
  | DW_op_const2s of Numbers.Int16.t
  | DW_op_const4s of Int32.t
  | DW_op_const8s of Int64.t
  | DW_op_constu of Numbers.Uint64.t
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
    (** The [reg_number] is variable-length, but we will assume that [int]
        is sufficient. *)
  | DW_op_dup
  | DW_op_drop
  | DW_op_pick
  | DW_op_over
  | DW_op_swap
  | DW_op_rot
  | DW_op_deref
  | DW_op_deref_size of Numbers.Uint8.t
  | DW_op_xderef
  | DW_op_xderef_size of Numbers.Uint8.t
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
  | DW_op_plus_uconst of Numbers.Uint64.t
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
  | DW_op_skip of { num_bytes_forward : Numbers.Int16.t; }
  | DW_op_bra of { num_bytes_forward : Numbers.Int16.t; }
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
    (** The [reg_number] is variable-length, but we will assume that [int]
        is sufficient. *)
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

val print : Format.formatter -> t -> unit

include Dwarf_emittable.S with type t := t
