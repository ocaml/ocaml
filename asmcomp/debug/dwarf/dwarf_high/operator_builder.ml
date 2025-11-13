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

type t = {
  ops : Dwarf_operator.t list;
}

let create () = { ops = [] }

let add_op t op = { ops = op :: t.ops }

(* Stack operations *)

let push_reg t reg =
  if reg >= 0 && reg <= 31 then
    (* Use DW_OP_regN for registers 0-31 *)
    let op = match reg with
      | 0 -> Dwarf_operator.DW_OP_reg0 | 1 -> Dwarf_operator.DW_OP_reg1
      | 2 -> Dwarf_operator.DW_OP_reg2 | 3 -> Dwarf_operator.DW_OP_reg3
      | 4 -> Dwarf_operator.DW_OP_reg4 | 5 -> Dwarf_operator.DW_OP_reg5
      | 6 -> Dwarf_operator.DW_OP_reg6 | 7 -> Dwarf_operator.DW_OP_reg7
      | 8 -> Dwarf_operator.DW_OP_reg8 | 9 -> Dwarf_operator.DW_OP_reg9
      | 10 -> Dwarf_operator.DW_OP_reg10 | 11 -> Dwarf_operator.DW_OP_reg11
      | 12 -> Dwarf_operator.DW_OP_reg12 | 13 -> Dwarf_operator.DW_OP_reg13
      | 14 -> Dwarf_operator.DW_OP_reg14 | 15 -> Dwarf_operator.DW_OP_reg15
      | 16 -> Dwarf_operator.DW_OP_reg16 | 17 -> Dwarf_operator.DW_OP_reg17
      | 18 -> Dwarf_operator.DW_OP_reg18 | 19 -> Dwarf_operator.DW_OP_reg19
      | 20 -> Dwarf_operator.DW_OP_reg20 | 21 -> Dwarf_operator.DW_OP_reg21
      | 22 -> Dwarf_operator.DW_OP_reg22 | 23 -> Dwarf_operator.DW_OP_reg23
      | 24 -> Dwarf_operator.DW_OP_reg24 | 25 -> Dwarf_operator.DW_OP_reg25
      | 26 -> Dwarf_operator.DW_OP_reg26 | 27 -> Dwarf_operator.DW_OP_reg27
      | 28 -> Dwarf_operator.DW_OP_reg28 | 29 -> Dwarf_operator.DW_OP_reg29
      | 30 -> Dwarf_operator.DW_OP_reg30 | 31 -> Dwarf_operator.DW_OP_reg31
      | _ -> assert false
    in
    add_op t op
  else
    (* Use DW_OP_regx for registers > 31 *)
    add_op t Dwarf_operator.DW_OP_regx

let push_constant t n =
  if n >= 0 && n <= 31 then
    (* Use DW_OP_litN for small constants *)
    let op = match n with
      | 0 -> Dwarf_operator.DW_OP_lit0 | 1 -> Dwarf_operator.DW_OP_lit1
      | 2 -> Dwarf_operator.DW_OP_lit2 | 3 -> Dwarf_operator.DW_OP_lit3
      | 4 -> Dwarf_operator.DW_OP_lit4 | 5 -> Dwarf_operator.DW_OP_lit5
      | 6 -> Dwarf_operator.DW_OP_lit6 | 7 -> Dwarf_operator.DW_OP_lit7
      | 8 -> Dwarf_operator.DW_OP_lit8 | 9 -> Dwarf_operator.DW_OP_lit9
      | 10 -> Dwarf_operator.DW_OP_lit10 | 11 -> Dwarf_operator.DW_OP_lit11
      | 12 -> Dwarf_operator.DW_OP_lit12 | 13 -> Dwarf_operator.DW_OP_lit13
      | 14 -> Dwarf_operator.DW_OP_lit14 | 15 -> Dwarf_operator.DW_OP_lit15
      | 16 -> Dwarf_operator.DW_OP_lit16 | 17 -> Dwarf_operator.DW_OP_lit17
      | 18 -> Dwarf_operator.DW_OP_lit18 | 19 -> Dwarf_operator.DW_OP_lit19
      | 20 -> Dwarf_operator.DW_OP_lit20 | 21 -> Dwarf_operator.DW_OP_lit21
      | 22 -> Dwarf_operator.DW_OP_lit22 | 23 -> Dwarf_operator.DW_OP_lit23
      | 24 -> Dwarf_operator.DW_OP_lit24 | 25 -> Dwarf_operator.DW_OP_lit25
      | 26 -> Dwarf_operator.DW_OP_lit26 | 27 -> Dwarf_operator.DW_OP_lit27
      | 28 -> Dwarf_operator.DW_OP_lit28 | 29 -> Dwarf_operator.DW_OP_lit29
      | 30 -> Dwarf_operator.DW_OP_lit30 | 31 -> Dwarf_operator.DW_OP_lit31
      | _ -> assert false
    in
    add_op t op
  else
    (* Use DW_OP_consts for other constants *)
    add_op t Dwarf_operator.DW_OP_consts

let push_constant64 t _n =
  add_op t Dwarf_operator.DW_OP_const8s

(* Arithmetic operations *)

let add t = add_op t Dwarf_operator.DW_OP_plus

let subtract t = add_op t Dwarf_operator.DW_OP_minus

let add_constant t _n = add_op t Dwarf_operator.DW_OP_plus_uconst

(* Memory operations *)

let deref t = add_op t Dwarf_operator.DW_OP_deref

let deref_size t _size = add_op t Dwarf_operator.DW_OP_deref_size

(* Frame-relative addressing *)

let frame_base_offset t _offset =
  add_op t Dwarf_operator.DW_OP_fbreg

let rec stack_offset t offset =
  (* Assume stack pointer is register 31 (ARM64) or 7 (x86_64 rsp) *)
  reg_offset t 31 offset

and reg_offset t reg _offset =
  if reg >= 0 && reg <= 31 then
    let op = match reg with
      | 0 -> Dwarf_operator.DW_OP_breg0 | 1 -> Dwarf_operator.DW_OP_breg1
      | 2 -> Dwarf_operator.DW_OP_breg2 | 3 -> Dwarf_operator.DW_OP_breg3
      | 4 -> Dwarf_operator.DW_OP_breg4 | 5 -> Dwarf_operator.DW_OP_breg5
      | 6 -> Dwarf_operator.DW_OP_breg6 | 7 -> Dwarf_operator.DW_OP_breg7
      | 8 -> Dwarf_operator.DW_OP_breg8 | 9 -> Dwarf_operator.DW_OP_breg9
      | 10 -> Dwarf_operator.DW_OP_breg10 | 11 -> Dwarf_operator.DW_OP_breg11
      | 12 -> Dwarf_operator.DW_OP_breg12 | 13 -> Dwarf_operator.DW_OP_breg13
      | 14 -> Dwarf_operator.DW_OP_breg14 | 15 -> Dwarf_operator.DW_OP_breg15
      | 16 -> Dwarf_operator.DW_OP_breg16 | 17 -> Dwarf_operator.DW_OP_breg17
      | 18 -> Dwarf_operator.DW_OP_breg18 | 19 -> Dwarf_operator.DW_OP_breg19
      | 20 -> Dwarf_operator.DW_OP_breg20 | 21 -> Dwarf_operator.DW_OP_breg21
      | 22 -> Dwarf_operator.DW_OP_breg22 | 23 -> Dwarf_operator.DW_OP_breg23
      | 24 -> Dwarf_operator.DW_OP_breg24 | 25 -> Dwarf_operator.DW_OP_breg25
      | 26 -> Dwarf_operator.DW_OP_breg26 | 27 -> Dwarf_operator.DW_OP_breg27
      | 28 -> Dwarf_operator.DW_OP_breg28 | 29 -> Dwarf_operator.DW_OP_breg29
      | 30 -> Dwarf_operator.DW_OP_breg30 | 31 -> Dwarf_operator.DW_OP_breg31
      | _ -> assert false
    in
    add_op t op
  else
    add_op t Dwarf_operator.DW_OP_bregx

(* Stack manipulation *)

let dup t = add_op t Dwarf_operator.DW_OP_dup

let drop t = add_op t Dwarf_operator.DW_OP_drop

let piece t _size = add_op t Dwarf_operator.DW_OP_piece

let stack_value t = add_op t Dwarf_operator.DW_OP_stack_value

let call_frame_cfa t = add_op t Dwarf_operator.DW_OP_call_frame_cfa

(* Conversion to bytes *)

let operators t = List.rev t.ops

let to_bytes t =
  (* This is a simplified version - proper implementation would encode
     operators with their operands according to DWARF spec *)
  let ops = operators t in
  let buf = Buffer.create 16 in
  List.iter (fun op ->
    let code = Dwarf_operator.to_code op in
    Buffer.add_char buf (Char.chr code)
  ) ops;
  Bytes.of_string (Buffer.contents buf)

let print ppf t =
  Format.fprintf ppf "@[<hov 2>[";
  let ops = operators t in
  List.iteri (fun i op ->
    if i > 0 then Format.fprintf ppf ";@ ";
    Dwarf_operator.print ppf op
  ) ops;
  Format.fprintf ppf "]@]"

(* Common patterns *)

let in_register reg =
  let t = create () in
  let t = push_reg t reg in
  to_bytes t

let at_frame_offset offset =
  let t = create () in
  let t = frame_base_offset t offset in
  to_bytes t

let at_stack_offset offset =
  let t = create () in
  let t = stack_offset t offset in
  to_bytes t

let at_reg_offset ~reg ~offset =
  let t = create () in
  let t = reg_offset t reg offset in
  to_bytes t
