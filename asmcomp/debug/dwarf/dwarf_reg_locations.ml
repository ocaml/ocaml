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

module SLDL = Simple_location_description_lang

let reg_location_description (reg : Reg.t) ~offset_from_cfa_in_bytes
      ~need_rvalue =
  let module SLD = Simple_location_description in
  match reg.loc with
  | Unknown ->
    Misc.fatal_errorf "Register without location: %a" Printmach.reg reg
  | Reg n ->
    let dwarf_reg_number =
      let reg_class = Proc.register_class reg in
      let first_available_reg = Proc.first_available_register.(reg_class) in
      let num_hard_regs = Proc.num_available_registers.(reg_class) in
      let n = n - first_available_reg in
      (* This [None] case isn't an error to cover situations such as found
         in the i386 backend where [num_available_registers] does not extend
         to the end of the register arrays (in that case for the x87 top of
         stack register). *)
      if n < 0 || n >= num_hard_regs then None
      else Some (Proc.dwarf_register_numbers ~reg_class).(n)
    in
    begin match dwarf_reg_number with
    | None -> None
    | Some dwarf_reg_number ->
      let location_description =
        if not need_rvalue then
          SLDL.compile (SLDL.of_lvalue (
            SLDL.Lvalue.in_register ~dwarf_reg_number))
        else
          SLDL.compile (SLDL.of_rvalue (
            SLDL.Rvalue.in_register ~dwarf_reg_number))
      in
      Some location_description
    end
  | Stack _ ->
    match offset_from_cfa_in_bytes with
    | None ->
      Misc.fatal_errorf "Register %a assigned to stack but no offset \
          from CFA provided"
        Printmach.reg reg
    | Some offset_from_cfa_in_bytes ->
      if offset_from_cfa_in_bytes mod Arch.size_addr <> 0 then begin
        Misc.fatal_errorf "Dwarf.location_list_entry: misaligned stack \
            slot at offset %d (reg %a)"
          offset_from_cfa_in_bytes
          Printmach.reg reg
      end;
      (* CR-soon mshinwell: use [offset_in_bytes] instead *)
      let offset_in_words =
        Targetint.of_int_exn (offset_from_cfa_in_bytes / Arch.size_addr)
      in
      if not need_rvalue then
        Some (SLDL.compile (SLDL.of_lvalue (
          SLDL.Lvalue.in_stack_slot ~offset_in_words)))
      else
        Some (SLDL.compile (SLDL.of_rvalue (
          SLDL.Rvalue.in_stack_slot ~offset_in_words)))

(* CR mshinwell: Share with [Available_ranges_vars]. *)
let offset_from_cfa_in_bytes reg stack_loc ~stack_offset =
  let frame_size = Proc.frame_size ~stack_offset in
  let slot_offset =
    Proc.slot_offset stack_loc ~reg_class:(Proc.register_class reg)
      ~stack_offset
  in
  Some (frame_size - slot_offset)
