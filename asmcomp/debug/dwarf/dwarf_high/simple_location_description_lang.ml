(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module O = Dwarf_operator
module OB = Operator_builder

type t = Simple_location_description.t

type lvalue = t

type normal
type last

type 'rvalue_kind rvalue = t

let empty = []

module Lvalue = struct
  type t = lvalue

  let in_register ~dwarf_reg_number =
    [OB.register_as_lvalue ~dwarf_reg_number]

  let in_stack_slot ~offset_in_words =
    let offset_in_bytes =
      Targetint.mul offset_in_words Targetint.size_in_bytes_as_targetint
    in
    OB.address_of_stack_slot ~offset_in_bytes

  let const_symbol symbol = [OB.value_of_symbol ~symbol]

  let in_symbol_field symbol ~field =
    let offset_in_bytes =
      Targetint.mul field Targetint.size_in_bytes_as_targetint
    in
    (OB.value_of_symbol ~symbol) :: [
      OB.add_unsigned_const offset_in_bytes;
    ]

  let location_from_another_die ~die_label ~compilation_unit_header_label =
    [OB.call ~die_label ~compilation_unit_header_label]
end

module Rvalue = struct
  (* CR-someday mshinwell: We append [DW_op_stack_value] to the ends of all
     expressions here so that they can be used in two contexts:
     - When called, to yield an rvalue, by one of our other expressions.
       In this case [DW_op_stack_value] is maybe not needed.
     - When passed directly to GDB (for example to describe a signed
       integer constant).  In this case [DW_op_stack_value] is needed,
       otherwise GDB will treat the top of stack as an lvalue, and
       dereference it.
     It may be that we should have a more nuanced partitioning of expressions
     that reflects this distinction.  Alternatively if [DW_op_stack_value]
     turns out to be always needed in both of the above cases, there is
     nothing to do.
     Note: page 10 of the DWARF-4 spec, line 26, distinguishes "location
     descriptions" from "expressions".  Maybe this is the correct
     terminology.
  *)

  type 'a t = 'a rvalue

  let signed_int_const i = [
    OB.signed_int_const i;
    O.DW_op_stack_value;
  ]

  let float_const i = [
    OB.float_const i;
    O.DW_op_stack_value;
  ]

  let const_symbol symbol = [
    OB.value_of_symbol ~symbol;
    O.DW_op_stack_value;
  ]

  let in_register ~dwarf_reg_number = [
    OB.contents_of_register ~dwarf_reg_number;
    O.DW_op_stack_value;
  ]

  let in_stack_slot ~offset_in_words = 
    let offset_in_bytes =
      Targetint.mul offset_in_words Targetint.size_in_bytes_as_targetint
    in
    (OB.contents_of_stack_slot ~offset_in_bytes) @ [
      O.DW_op_stack_value;
    ]

  let read_field ~block ~field =
    let offset_in_bytes =
      Targetint.mul field Targetint.size_in_bytes_as_targetint
    in
    (* We emit special code to catch the case where evaluation of [block]
       fails (for example due to unavailability). *)
    (OB.signed_int_const Targetint.zero) ::
      block @ [
        O.DW_op_dup;
      ] @
      OB.conditional ~if_zero:[
        ]
        ~if_nonzero:[
          O.DW_op_swap;
          O.DW_op_drop;
          OB.add_unsigned_const offset_in_bytes;
          O.DW_op_deref;
        ]
        ~at_join:[
          O.DW_op_stack_value;
        ]

  let read_symbol_field symbol ~field =
    read_field ~block:(Lvalue.const_symbol symbol) ~field

  let offset_pointer lvalue ~offset_in_words =
    let offset_in_bytes =
      Targetint.mul offset_in_words Targetint.size_in_bytes_as_targetint
    in
    (* Similar to [read_field], above. *)
    (OB.signed_int_const Targetint.zero) ::
      lvalue @ [
        O.DW_op_dup;
      ] @
      OB.conditional ~if_zero:[
        ]
        ~if_nonzero:[
          O.DW_op_swap;
          O.DW_op_drop;
          OB.add_unsigned_const offset_in_bytes;
        ]
        ~at_join:[
          O.DW_op_stack_value;
        ]

  (* CR mshinwell: Check if this needs DW_op_stack_value (suspect not). *)
  let implicit_pointer ~offset_in_bytes ~die_label dwarf_version =
    [OB.implicit_pointer ~offset_in_bytes ~die_label dwarf_version]

  let location_from_another_die ~die_label ~compilation_unit_header_label =
    [OB.call ~die_label ~compilation_unit_header_label]
end

let of_lvalue t = t
let of_rvalue t = t

let compile t = OB.optimize_sequence t
