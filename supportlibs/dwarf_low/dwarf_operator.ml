(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type implicit_value =
  | Int of Int64.t
  | Symbol of Symbol.t

type t =
  | DW_op_addr of Symbol.t
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

let print ppf t =
  let fprintf = Format.fprintf in
  match t with
  | DW_op_addr sym -> fprintf ppf "DW_op_addr %a" Symbol.print sym
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
    fprintf ppf "DW_op_implicit_value %a" Symbol.print symbol
  | DW_op_stack_value -> fprintf ppf "DW_op_stack_value"

let contents_of_register ~reg_number =
  DW_op_bregx { reg_number; offset_in_bytes = 0L; }

let _ = DW_op_fbreg { offset_in_bytes = 0L; }  (* silence compiler warning *)

let contents_of_stack_slot ~offset_in_bytes =
  let offset_in_bytes = Int64.of_int offset_in_bytes in [
    (* Note that this isn't target-dependent.  The target dependent part
       is the thing (typically in emit.mlp) that fills in the offsets in
       [Lcapture_stack_offset] instructions. *)
    DW_op_call_frame_cfa;
    DW_op_consts offset_in_bytes;
    DW_op_minus;
    DW_op_deref;
  ]

let value_of_symbol symbol = DW_op_addr symbol

let signed_int_const i = DW_op_consts i

let add_unsigned_const i =
  if Int64.compare i 0L < 0 then begin
    Misc.fatal_error "Dwarf_operator.add_unsigned_const only takes \
      integers >= 0"
  end;
  DW_op_plus_uconst i

let deref () = DW_op_deref

let stack_value () = DW_op_stack_value

let optimize_sequence ts =
  let rec optimize ts =
    match ts with
    | [] -> []
    | DW_op_bregx { reg_number; offset_in_bytes = 0L; }
        :: DW_op_stack_value :: [] ->
      [DW_op_regx { reg_number; }]
    | DW_op_deref :: DW_op_stack_value :: [] -> []
    | (DW_op_bregx { reg_number; offset_in_bytes = 0L; })
        :: (DW_op_plus_uconst offset_in_bytes)
        :: ts ->
      (DW_op_bregx { reg_number; offset_in_bytes; }) :: (optimize ts)
    | DW_op_addr symbol :: DW_op_stack_value :: [] ->
      [DW_op_implicit_value (Symbol symbol)]
    | DW_op_consts i :: DW_op_stack_value :: [] ->
      [DW_op_implicit_value (Int i)]
    | DW_op_stack_value :: [] -> ts
    | DW_op_stack_value :: _ ->
      Misc.fatal_error "DW_op_stack_value may only occur at the end"
    | (DW_op_plus_uconst i) :: ts' ->
      if Int64.compare i 0L < 0 then assert false;  (* see above *)
      let rec total t =
        match t with
        | [] -> 0L
        | (DW_op_plus_uconst i) :: ts -> Int64.add i (total ts)
        | _ -> 0L
      in
      let total = total ts in
      if Int64.compare total 0L = 0 then optimize ts'
      else (DW_op_plus_uconst total) :: (optimize ts')
    | t::ts -> t :: (optimize ts)
  in
  List.map (function
      | (DW_op_regx { reg_number = reg; }) as op ->
        begin match reg with
        | 0 -> DW_op_reg0
        | 1 -> DW_op_reg1
        | 2 -> DW_op_reg2
        | 3 -> DW_op_reg3
        | 4 -> DW_op_reg4
        | 5 -> DW_op_reg5
        | 6 -> DW_op_reg6
        | 7 -> DW_op_reg7
        | 8 -> DW_op_reg8
        | 9 -> DW_op_reg9
        | 10 -> DW_op_reg10
        | 11 -> DW_op_reg11
        | 12 -> DW_op_reg12
        | 13 -> DW_op_reg13
        | 14 -> DW_op_reg14
        | 15 -> DW_op_reg15
        | 16 -> DW_op_reg16
        | 17 -> DW_op_reg17
        | 18 -> DW_op_reg18
        | 19 -> DW_op_reg19
        | 20 -> DW_op_reg20
        | 21 -> DW_op_reg21
        | 22 -> DW_op_reg22
        | 23 -> DW_op_reg23
        | 24 -> DW_op_reg24
        | 25 -> DW_op_reg25
        | 26 -> DW_op_reg26
        | 27 -> DW_op_reg27
        | 28 -> DW_op_reg28
        | 29 -> DW_op_reg29
        | 30 -> DW_op_reg30
        | 31 -> DW_op_reg31
        | _ -> op
        end
      | (DW_op_bregx { reg_number = reg; offset_in_bytes; }) as op ->
        begin match reg with
        | 0 -> DW_op_breg0 { offset_in_bytes; }
        | 1 -> DW_op_breg1 { offset_in_bytes; }
        | 2 -> DW_op_breg2 { offset_in_bytes; }
        | 3 -> DW_op_breg3 { offset_in_bytes; }
        | 4 -> DW_op_breg4 { offset_in_bytes; }
        | 5 -> DW_op_breg5 { offset_in_bytes; }
        | 6 -> DW_op_breg6 { offset_in_bytes; }
        | 7 -> DW_op_breg7 { offset_in_bytes; }
        | 8 -> DW_op_breg8 { offset_in_bytes; }
        | 9 -> DW_op_breg9 { offset_in_bytes; }
        | 10 -> DW_op_breg10 { offset_in_bytes; }
        | 11 -> DW_op_breg11 { offset_in_bytes; }
        | 12 -> DW_op_breg12 { offset_in_bytes; }
        | 13 -> DW_op_breg13 { offset_in_bytes; }
        | 14 -> DW_op_breg14 { offset_in_bytes; }
        | 15 -> DW_op_breg15 { offset_in_bytes; }
        | 16 -> DW_op_breg16 { offset_in_bytes; }
        | 17 -> DW_op_breg17 { offset_in_bytes; }
        | 18 -> DW_op_breg18 { offset_in_bytes; }
        | 19 -> DW_op_breg19 { offset_in_bytes; }
        | 20 -> DW_op_breg20 { offset_in_bytes; }
        | 21 -> DW_op_breg21 { offset_in_bytes; }
        | 22 -> DW_op_breg22 { offset_in_bytes; }
        | 23 -> DW_op_breg23 { offset_in_bytes; }
        | 24 -> DW_op_breg24 { offset_in_bytes; }
        | 25 -> DW_op_breg25 { offset_in_bytes; }
        | 26 -> DW_op_breg26 { offset_in_bytes; }
        | 27 -> DW_op_breg27 { offset_in_bytes; }
        | 28 -> DW_op_breg28 { offset_in_bytes; }
        | 29 -> DW_op_breg29 { offset_in_bytes; }
        | 30 -> DW_op_breg30 { offset_in_bytes; }
        | 31 -> DW_op_breg31 { offset_in_bytes; }
        | _ -> op
        end
      (* CR-soon mshinwell: optimize DW_op_consts *)
      | op -> op)
    (optimize ts)

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
  | DW_op_call_frame_cfa -> 0x9c
  | DW_op_implicit_value _ -> 0x9e
  | DW_op_stack_value -> 0x9f

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
  in
  Int64.add opcode_size args_size

let emit t asm =
  Dwarf_value.emit (Dwarf_value.Int8 (Numbers.Int8.of_int_exn (opcode t))) asm;
  match t with
  | DW_op_addr sym -> Dwarf_value.emit (Code_address_from_symbol sym) asm
  | DW_op_regx { reg_number ; } ->
    Dwarf_value.emit (Uleb128 (Int64.of_int reg_number)) asm
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
    Dwarf_value.emit (Sleb128 offset_in_bytes) asm
  | DW_op_bregx { reg_number; offset_in_bytes; } ->
    Dwarf_value.emit (Uleb128 (Int64.of_int reg_number)) asm;
    Dwarf_value.emit (Sleb128 offset_in_bytes) asm
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
    Dwarf_value.emit (Sleb128 offset_in_bytes) asm
  | DW_op_implicit_value (Int i) ->
    (* The buffer must contain the integer as an OCaml value. *)
    let i = Int64.logor (Int64.shift_left i 1) 1L in
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
    Dwarf_value.emit (Sleb128 (Int64.of_int (Bytes.length buf))) asm;
    Dwarf_value.emit (String (Bytes.to_string buf)) asm
  | DW_op_implicit_value (Symbol symbol) ->
    Dwarf_value.emit (Sleb128 (Int64.of_int Arch.size_addr)) asm;
    Dwarf_value.emit (Code_address_from_symbol symbol) asm
  | DW_op_plus_uconst const -> Dwarf_value.emit (Uleb128 const) asm
  | DW_op_consts const -> Dwarf_value.emit (Sleb128 const) asm
  | DW_op_deref
  | DW_op_minus
  | DW_op_call_frame_cfa
  | DW_op_stack_value -> ()
