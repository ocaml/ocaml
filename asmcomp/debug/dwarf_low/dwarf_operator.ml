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
  (* CR-someday mshinwell: Should probably use DW_op_call_ref, but gdb doesn't
     currently support it.  This would remove the "header label" nonsense. *)
  | DW_op_call4 of { label : Cmm.label;
      compilation_unit_header_label : Cmm.label; }
  | DW_op_skip of { num_bytes_forward : int; }
  | DW_op_bra of { num_bytes_forward : int; }
  | DW_op_drop
  | DW_op_dup
  | DW_op_swap
  | DW_op_nop

let print ppf t =
  let fprintf = Format.fprintf in
  match t with
  | DW_op_addr sym -> fprintf ppf "DW_op_addr %s" sym
  | DW_op_regx { reg_number; } -> fprintf ppf "DW_op_regx %d" reg_number
  | DW_op_reg0 -> fprintf ppf "DW_op_reg0"
  | DW_op_reg1 -> fprintf ppf "DW_op_reg1"
  | DW_op_reg2 -> fprintf ppf "DW_op_reg2"
  | DW_op_reg3 -> fprintf ppf "DW_op_reg3"
  | DW_op_reg4 -> fprintf ppf "DW_op_reg4"
  | DW_op_reg5 -> fprintf ppf "DW_op_reg5"
  | DW_op_reg6 -> fprintf ppf "DW_op_reg6"
  | DW_op_reg7 -> fprintf ppf "DW_op_reg7"
  | DW_op_reg8 -> fprintf ppf "DW_op_reg8"
  | DW_op_reg9 -> fprintf ppf "DW_op_reg9"
  | DW_op_reg10 -> fprintf ppf "DW_op_reg10"
  | DW_op_reg11 -> fprintf ppf "DW_op_reg11"
  | DW_op_reg12 -> fprintf ppf "DW_op_reg12"
  | DW_op_reg13 -> fprintf ppf "DW_op_reg13"
  | DW_op_reg14 -> fprintf ppf "DW_op_reg14"
  | DW_op_reg15 -> fprintf ppf "DW_op_reg15"
  | DW_op_reg16 -> fprintf ppf "DW_op_reg16"
  | DW_op_reg17 -> fprintf ppf "DW_op_reg17"
  | DW_op_reg18 -> fprintf ppf "DW_op_reg18"
  | DW_op_reg19 -> fprintf ppf "DW_op_reg19"
  | DW_op_reg20 -> fprintf ppf "DW_op_reg20"
  | DW_op_reg21 -> fprintf ppf "DW_op_reg21"
  | DW_op_reg22 -> fprintf ppf "DW_op_reg22"
  | DW_op_reg23 -> fprintf ppf "DW_op_reg23"
  | DW_op_reg24 -> fprintf ppf "DW_op_reg24"
  | DW_op_reg25 -> fprintf ppf "DW_op_reg25"
  | DW_op_reg26 -> fprintf ppf "DW_op_reg26"
  | DW_op_reg27 -> fprintf ppf "DW_op_reg27"
  | DW_op_reg28 -> fprintf ppf "DW_op_reg28"
  | DW_op_reg29 -> fprintf ppf "DW_op_reg29"
  | DW_op_reg30 -> fprintf ppf "DW_op_reg30"
  | DW_op_reg31 -> fprintf ppf "DW_op_reg31"
  | DW_op_fbreg { offset_in_bytes; } ->
    fprintf ppf "DW_op_fbreg 0x%Lx" offset_in_bytes
  | DW_op_breg0 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg0 0x%Lx" offset_in_bytes
  | DW_op_breg1 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg1 0x%Lx" offset_in_bytes
  | DW_op_breg2 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg2 0x%Lx" offset_in_bytes
  | DW_op_breg3 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg3 0x%Lx" offset_in_bytes
  | DW_op_breg4 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg4 0x%Lx" offset_in_bytes
  | DW_op_breg5 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg5 0x%Lx" offset_in_bytes
  | DW_op_breg6 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg6 0x%Lx" offset_in_bytes
  | DW_op_breg7 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg7 0x%Lx" offset_in_bytes
  | DW_op_breg8 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg8 0x%Lx" offset_in_bytes
  | DW_op_breg9 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg9 0x%Lx" offset_in_bytes
  | DW_op_breg10 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg10 0x%Lx" offset_in_bytes
  | DW_op_breg11 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg11 0x%Lx" offset_in_bytes
  | DW_op_breg12 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg12 0x%Lx" offset_in_bytes
  | DW_op_breg13 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg13 0x%Lx" offset_in_bytes
  | DW_op_breg14 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg14 0x%Lx" offset_in_bytes
  | DW_op_breg15 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg15 0x%Lx" offset_in_bytes
  | DW_op_breg16 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg16 0x%Lx" offset_in_bytes
  | DW_op_breg17 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg17 0x%Lx" offset_in_bytes
  | DW_op_breg18 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg18 0x%Lx" offset_in_bytes
  | DW_op_breg19 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg19 0x%Lx" offset_in_bytes
  | DW_op_breg20 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg20 0x%Lx" offset_in_bytes
  | DW_op_breg21 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg21 0x%Lx" offset_in_bytes
  | DW_op_breg22 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg22 0x%Lx" offset_in_bytes
  | DW_op_breg23 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg23 0x%Lx" offset_in_bytes
  | DW_op_breg24 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg24 0x%Lx" offset_in_bytes
  | DW_op_breg25 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg25 0x%Lx" offset_in_bytes
  | DW_op_breg26 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg26 0x%Lx" offset_in_bytes
  | DW_op_breg27 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg27 0x%Lx" offset_in_bytes
  | DW_op_breg28 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg28 0x%Lx" offset_in_bytes
  | DW_op_breg29 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg29 0x%Lx" offset_in_bytes
  | DW_op_breg30 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg30 0x%Lx" offset_in_bytes
  | DW_op_breg31 { offset_in_bytes; } ->
    fprintf ppf "DW_op_breg31 0x%Lx" offset_in_bytes
  | DW_op_bregx { reg_number; offset_in_bytes; } ->
    fprintf ppf "DW_op_bregx %d 0x%Lx" reg_number offset_in_bytes
  | DW_op_deref -> fprintf ppf "DW_op_deref"
  | DW_op_plus_uconst i -> fprintf ppf "DW_op_plus_uconst %Ld" i
  | DW_op_consts i -> fprintf ppf "DW_op_consts %Ld" i
  | DW_op_call_frame_cfa -> fprintf ppf "DW_op_call_frame_cfa"
  | DW_op_minus -> fprintf ppf "DW_op_minus"
  | DW_op_implicit_value (Int i) ->
    fprintf ppf "DW_op_implicit_value %Ld" i
  | DW_op_implicit_value (Symbol symbol) ->
    fprintf ppf "DW_op_implicit_value %s" symbol
  | DW_op_stack_value -> fprintf ppf "DW_op_stack_value"
  | DW_op_GNU_implicit_pointer { offset_in_bytes; label; } ->
    fprintf ppf "DW_op_GNU_implicit_pointer offset=%d label=%d" offset_in_bytes
      label
  | DW_op_implicit_pointer { offset_in_bytes; label; } ->
    fprintf ppf "DW_op_implicit_pointer offset=%d label=%d" offset_in_bytes
      label
  | DW_op_piece { size_in_bytes; } ->
    fprintf ppf "DW_op_piece %d" size_in_bytes
  | DW_op_call4 { label; _ } ->
    fprintf ppf "DW_op_call4 %d" label
  | DW_op_skip { num_bytes_forward; } ->
    fprintf ppf "DW_op_skip %d" num_bytes_forward
  | DW_op_bra { num_bytes_forward; } ->
    fprintf ppf "DW_op_bra %d" num_bytes_forward
  | DW_op_drop -> fprintf ppf "DW_op_drop"
  | DW_op_dup -> fprintf ppf "DW_op_dup"
  | DW_op_swap -> fprintf ppf "DW_op_swap"
  | DW_op_nop -> fprintf ppf "DW_op_nop"

external caml_string_set32 : bytes -> index:int -> Int32.t -> unit
  = "%caml_string_set32"

external caml_string_set64 : bytes -> index:int -> Int64.t -> unit
  = "%caml_string_set64"

(* DWARF-4 spec section 7.7.1. *)
let opcode = function
  | DW_op_addr _ -> 0x03
  | DW_op_deref -> 0x06
  | DW_op_consts _ -> 0x11
  | DW_op_minus -> 0x1c
  | DW_op_plus_uconst _ -> 0x23
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
  | DW_op_regx _ -> 0x90
  | DW_op_fbreg _ -> 0x91
  | DW_op_bregx _ -> 0x92
  | DW_op_piece _ -> 0x93
  | DW_op_call4 _ -> 0x99
  | DW_op_call_frame_cfa -> 0x9c
  | DW_op_implicit_value _ -> 0x9e
  | DW_op_stack_value -> 0x9f
  | DW_op_GNU_implicit_pointer _ -> 0xf2
  | DW_op_implicit_pointer _ -> 0xa0
  | DW_op_skip { num_bytes_forward = _; } -> 0x2f
  | DW_op_bra { num_bytes_forward = _; } -> 0x28
  | DW_op_drop -> 0x13
  | DW_op_dup -> 0x12
  | DW_op_swap -> 0x16
  | DW_op_nop -> 0x96

let size t =
  let opcode_size = Int64.of_int 1 in
  let args_size =
    match t with
    | DW_op_addr _addr -> Int64.of_int Arch.size_addr
    | DW_op_regx { reg_number; } ->
      Dwarf_value.size (Uleb128 (Int64.of_int reg_number))
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
    | DW_op_reg31 -> 0L
    | DW_op_fbreg { offset_in_bytes; } ->
      Dwarf_value.size (Sleb128 offset_in_bytes)
    | DW_op_bregx { reg_number; offset_in_bytes; } ->
      Int64.add (Dwarf_value.size (Uleb128 (Int64.of_int reg_number)))
        (Dwarf_value.size (Sleb128 offset_in_bytes))
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
      Dwarf_value.size (Sleb128 offset_in_bytes)
    | DW_op_implicit_value (Int _) ->
      let size_int = Int64.of_int Arch.size_int in
      Int64.add (Dwarf_value.size (Sleb128 size_int)) size_int
    | DW_op_implicit_value (Symbol _) ->
      let size_addr = Int64.of_int Arch.size_addr in
      Int64.add (Dwarf_value.size (Sleb128 size_addr)) size_addr
    | DW_op_plus_uconst const -> Dwarf_value.size (Uleb128 const)
    | DW_op_consts const -> Dwarf_value.size (Sleb128 const)
    | DW_op_deref
    | DW_op_minus
    | DW_op_call_frame_cfa
    | DW_op_stack_value -> 0L
    | DW_op_GNU_implicit_pointer { offset_in_bytes; label; }
    | DW_op_implicit_pointer { offset_in_bytes; label; } ->
      Int64.add (Dwarf_value.size (Offset_into_debug_info label))
        (Dwarf_value.size (Sleb128 (Int64.of_int offset_in_bytes)))
    | DW_op_piece { size_in_bytes; } ->
      Dwarf_value.size (Uleb128 (Int64.of_int size_in_bytes))
    | DW_op_call4 { label; compilation_unit_header_label } ->
      Dwarf_value.size (
        Distance_between_labels_32bit
          { upper = label; lower = compilation_unit_header_label; })
    | DW_op_skip { num_bytes_forward; }
    | DW_op_bra { num_bytes_forward; } ->
      Dwarf_value.size (Int16 (Numbers.Int16.of_int_exn num_bytes_forward))
    | DW_op_drop
    | DW_op_dup
    | DW_op_swap
    | DW_op_nop -> 0L
  in
  Int64.add opcode_size args_size

let emit t =
  Dwarf_value.emit (Dwarf_value.Int8 (Numbers.Int8.of_int_exn (opcode t)));
  match t with
  | DW_op_addr sym -> Dwarf_value.emit (Code_address_from_symbol sym)
  | DW_op_regx { reg_number ; } ->
    Dwarf_value.emit (Uleb128 (Int64.of_int reg_number))
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
  | DW_op_reg31 -> ()
  | DW_op_fbreg { offset_in_bytes; } ->
    Dwarf_value.emit (Sleb128 offset_in_bytes)
  | DW_op_bregx { reg_number; offset_in_bytes; } ->
    Dwarf_value.emit (Uleb128 (Int64.of_int reg_number));
    Dwarf_value.emit (Sleb128 offset_in_bytes)
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
    Dwarf_value.emit (Sleb128 offset_in_bytes)
  | DW_op_implicit_value (Int i) ->
    let buf =
      match Arch.size_int with
      | 4 ->
        let buf = Bytes.create 4 in
        caml_string_set32 buf ~index:0 (Int64.to_int32 i);
        buf
      | 8 ->
        let buf = Bytes.create 8 in
        caml_string_set64 buf ~index:0 i;
        buf
      | n ->
        Misc.fatal_errorf "Dwarf_operator: bad Arch.size_int = %d" n
    in
    Dwarf_value.emit (Sleb128 (Int64.of_int (Bytes.length buf)));
    Dwarf_value.emit (String (Bytes.to_string buf))
  | DW_op_implicit_value (Symbol symbol) ->
    Dwarf_value.emit (Sleb128 (Int64.of_int Arch.size_addr));
    Dwarf_value.emit (Code_address_from_symbol symbol)
  | DW_op_plus_uconst const -> Dwarf_value.emit (Uleb128 const)
  | DW_op_consts const -> Dwarf_value.emit (Sleb128 const)
  | DW_op_deref
  | DW_op_minus
  | DW_op_call_frame_cfa
  | DW_op_stack_value -> ()
  | DW_op_GNU_implicit_pointer { offset_in_bytes; label; }
  | DW_op_implicit_pointer { offset_in_bytes; label; } ->
    Dwarf_value.emit (Offset_into_debug_info label);
    Dwarf_value.emit (Sleb128 (Int64.of_int offset_in_bytes))
  | DW_op_piece { size_in_bytes; } ->
    Dwarf_value.emit (Uleb128 (Int64.of_int size_in_bytes))
  | DW_op_call4 { label; compilation_unit_header_label; } ->
    Dwarf_value.emit (
        Distance_between_labels_32bit
          { upper = label; lower = compilation_unit_header_label; })
  | DW_op_skip { num_bytes_forward; }
  | DW_op_bra { num_bytes_forward; } ->
    Dwarf_value.emit (Int16 (Numbers.Int16.of_int_exn num_bytes_forward))
  | DW_op_drop
  | DW_op_dup
  | DW_op_swap
  | DW_op_nop -> ()
