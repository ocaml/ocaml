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
  | Int of Int64.t
  | Symbol of string

type t =
  | DW_op_addr of string
  | DW_op_regx of { reg_number : int; }
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
  | DW_op_fbreg of { offset_in_bytes : Int64.t; }
  | DW_op_breg0 of { offset_in_bytes : Int64.t; }
  | DW_op_breg1 of { offset_in_bytes : Int64.t; }
  | DW_op_breg2 of { offset_in_bytes : Int64.t; }
  | DW_op_breg3 of { offset_in_bytes : Int64.t; }
  | DW_op_breg4 of { offset_in_bytes : Int64.t; }
  | DW_op_breg5 of { offset_in_bytes : Int64.t; }
  | DW_op_breg6 of { offset_in_bytes : Int64.t; }
  | DW_op_breg7 of { offset_in_bytes : Int64.t; }
  | DW_op_breg8 of { offset_in_bytes : Int64.t; }
  | DW_op_breg9 of { offset_in_bytes : Int64.t; }
  | DW_op_breg10 of { offset_in_bytes : Int64.t; }
  | DW_op_breg11 of { offset_in_bytes : Int64.t; }
  | DW_op_breg12 of { offset_in_bytes : Int64.t; }
  | DW_op_breg13 of { offset_in_bytes : Int64.t; }
  | DW_op_breg14 of { offset_in_bytes : Int64.t; }
  | DW_op_breg15 of { offset_in_bytes : Int64.t; }
  | DW_op_breg16 of { offset_in_bytes : Int64.t; }
  | DW_op_breg17 of { offset_in_bytes : Int64.t; }
  | DW_op_breg18 of { offset_in_bytes : Int64.t; }
  | DW_op_breg19 of { offset_in_bytes : Int64.t; }
  | DW_op_breg20 of { offset_in_bytes : Int64.t; }
  | DW_op_breg21 of { offset_in_bytes : Int64.t; }
  | DW_op_breg22 of { offset_in_bytes : Int64.t; }
  | DW_op_breg23 of { offset_in_bytes : Int64.t; }
  | DW_op_breg24 of { offset_in_bytes : Int64.t; }
  | DW_op_breg25 of { offset_in_bytes : Int64.t; }
  | DW_op_breg26 of { offset_in_bytes : Int64.t; }
  | DW_op_breg27 of { offset_in_bytes : Int64.t; }
  | DW_op_breg28 of { offset_in_bytes : Int64.t; }
  | DW_op_breg29 of { offset_in_bytes : Int64.t; }
  | DW_op_breg30 of { offset_in_bytes : Int64.t; }
  | DW_op_breg31 of { offset_in_bytes : Int64.t; }
  | DW_op_bregx of { reg_number : int; offset_in_bytes : Int64.t; }
  | DW_op_deref
  | DW_op_plus_uconst of Int64.t
  | DW_op_consts of Int64.t
  | DW_op_call_frame_cfa
  | DW_op_minus
  | DW_op_implicit_value of implicit_value
  | DW_op_stack_value
  | DW_op_GNU_implicit_pointer of { offset_in_bytes : int; label : Cmm.label; }
  | DW_op_implicit_pointer of { offset_in_bytes : int; label : Cmm.label; }
  | DW_op_piece of { size_in_bytes : int; }
  | DW_op_call4 of { label : Cmm.label;
      compilation_unit_header_label : Cmm.label; }
  | DW_op_skip of { num_bytes_forward : int; }
  | DW_op_bra of { num_bytes_forward : int; }
  | DW_op_drop
  | DW_op_dup
  | DW_op_swap
  | DW_op_nop

val print : Format.formatter -> t -> unit

include Dwarf_emittable.S with type t := t
