(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All righops reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module O = Dwarf_operator

let dw_op_regx ~dwarf_reg_number : O.t =
  match dwarf_reg_number with
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
  | _ -> DW_op_regx { reg_number = dwarf_reg_number; }

let dw_op_bregx ~dwarf_reg_number ~offset_in_bytes : O.t =
  match dwarf_reg_number with
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
  | _ -> DW_op_bregx { reg_number = dwarf_reg_number; offset_in_bytes; }

let register_as_lvalue ~dwarf_reg_number =
  dw_op_regx ~dwarf_reg_number

let contents_of_register ~dwarf_reg_number =
  dw_op_bregx ~dwarf_reg_number ~offset_in_bytes:Targetint.zero

let address_of_stack_slot ~offset_in_bytes =
  (* Note that this isn't target-dependent.  The target dependent part
     is the calculation of the [offset_in_bytes]. *)
  [ O.DW_op_call_frame_cfa;
    O.DW_op_consts offset_in_bytes;
    O.DW_op_minus;
  ]

let contents_of_stack_slot ~offset_in_bytes =
  (* Same comment as per [address_of_stack_slot]. *)
  [ O.DW_op_call_frame_cfa;
    O.DW_op_consts offset_in_bytes;
    O.DW_op_minus;
    O.DW_op_deref;
  ]

let value_of_symbol ~symbol : O.t = DW_op_addr symbol

let signed_int_const i : O.t = DW_op_consts i

let add_unsigned_const i : O.t =
  if Targetint.compare i Targetint.zero < 0 then begin
    Misc.fatal_error "[Operator_builder.add_unsigned_const] only takes \
      integers >= 0"
  end;
  DW_op_plus_uconst i

let implicit_pointer ~offset_in_bytes ~die_label dwarf_version : O.t =
  if Dwarf_version.compare dwarf_version Dwarf_version.four < 0 then
    Misc.fatal_error "DWARF implicit pointers not supported at this version"
  else if Dwarf_version.compare dwarf_version Dwarf_version.four = 0 then
    DW_op_GNU_implicit_pointer { offset_in_bytes; label = die_label; }
  else
    DW_op_implicit_pointer { offset_in_bytes; label = die_label; }

let call ~die_label ~compilation_unit_header_label : O.t =
  DW_op_call4 { label = die_label; compilation_unit_header_label; }

let conditional ~if_zero ~if_nonzero =
  let nonzero_branch_size =
    List.fold_left (fun nonzero_branch_size op ->
        Dwarf_int.add (O.size op) nonzero_branch_size)
      (Dwarf_int.zero ())
      if_nonzero
  in
  let nonzero_branch_size = Dwarf_int.to_int64 nonzero_branch_size in
  let max_branch_size = Int64.of_int ((1 lsl 16) - 1) in
  let (>) a b = Int64.compare a b > 0 in
  if nonzero_branch_size > max_branch_size then begin
    Misc.fatal_error "Dwarf_operator.conditional: nonzero branch too long"
  end;
  let nonzero_branch_size = Numbers.Int16.of_int64_exn nonzero_branch_size in
  let if_zero =
    if_zero @
      [O.DW_op_skip { num_bytes_forward = nonzero_branch_size; }]
  in
  let zero_branch_size =
    List.fold_left (fun zero_branch_size op ->
        Dwarf_int.add (O.size op) zero_branch_size)
      (Dwarf_int.zero ())
      if_zero
  in
  let zero_branch_size = Dwarf_int.to_int64 zero_branch_size in
  if zero_branch_size > max_branch_size then begin
    Misc.fatal_error "Dwarf_operator.conditional: zero branch too long"
  end;
  let zero_branch_size = Numbers.Int16.of_int64_exn zero_branch_size in
  (* The [DW_op_nop] is there in case no other operator follows (this is a
     branch target). *)
  O.DW_op_bra { num_bytes_forward = zero_branch_size; }
    :: if_zero @ if_nonzero @ [O.DW_op_nop]

let optimize_sequence ops =
  let rec optimize (ops : O.t list) =
    match ops with
    | [] -> []
    | DW_op_deref :: DW_op_stack_value :: [] -> []
    | (DW_op_bregx { reg_number; offset_in_bytes = offset_in_bytes'; })
        :: (DW_op_plus_uconst offset_in_bytes)
        :: ops
        when Targetint.equal offset_in_bytes' Targetint.zero ->
      (O.DW_op_bregx { reg_number; offset_in_bytes; }) :: (optimize ops)
    | DW_op_addr symbol :: DW_op_stack_value :: [] ->
      [O.DW_op_implicit_value (Symbol symbol)]
    | DW_op_consts i :: DW_op_stack_value :: [] ->
      [O.DW_op_implicit_value (Int i)]
    | (DW_op_plus_uconst _) :: _ ->
      let rec total (ops : O.t list) =
        match ops with
        | [] -> Targetint.zero, []
        | (DW_op_plus_uconst i) :: ops ->
          if Targetint.compare i Targetint.zero < 0 then begin
            Misc.fatal_error "Found [DW_op_plus_uconst] with negative argument"
          end;
          let total, rest = total ops in
          Targetint.add i total, rest
        | _ -> Targetint.zero, ops
      in
      let total, rest = total ops in
      if Targetint.compare total Targetint.zero = 0 then optimize rest
      else (O.DW_op_plus_uconst total) :: (optimize rest)
    | DW_op_nop :: ops -> optimize ops
    | t::ops -> t :: (optimize ops)
  in
  optimize ops
