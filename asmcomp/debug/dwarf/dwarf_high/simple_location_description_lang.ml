(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
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
type lvalue_without_address = t
type normal
type implicit
type _ rvalue = t

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

  let in_symbol_field symbol ~field =
    let offset_in_bytes =
      Targetint.mul field Targetint.size_in_bytes_as_targetint
    in
    (OB.value_of_symbol ~symbol) :: OB.add_unsigned_const offset_in_bytes

  let read_field ~block ~field =
    let offset_in_bytes =
      Targetint.mul field Targetint.size_in_bytes_as_targetint
    in
    (* We emit special code to catch the case where evaluation of [block]
       fails (for example due to unavailability).  In the event of an
       unavailability failure, the [DW_OP_call*] evaluation of [block] does
       nothing to the stack. *)
    (OB.signed_int_const Targetint.zero) ::
      block @ [
        O.DW_op_dup;
      ] @
      OB.conditional ~if_zero:[
        ]
        ~if_nonzero:([
          O.DW_op_swap;
          O.DW_op_drop;
        ] @ OB.add_unsigned_const offset_in_bytes)
        ~at_join:[
        ]

  let offset_pointer t ~offset_in_words =
    let offset_in_bytes =
      Targetint.mul offset_in_words Targetint.size_in_bytes_as_targetint
    in
    (* Similar to [read_field], above. *)
    (OB.signed_int_const Targetint.zero) ::
      t @ [
        O.DW_op_dup;
      ] @
      OB.conditional ~if_zero:[
        ]
        ~if_nonzero:([
          O.DW_op_swap;
          O.DW_op_drop
        ] @ OB.add_unsigned_const offset_in_bytes)
        ~at_join:[
        ]

  let location_from_another_die ~die_label ~compilation_unit_header_label =
    [OB.call ~die_label ~compilation_unit_header_label]
end

module Lvalue_without_address = struct
  type t = lvalue_without_address

  (* Note that due to the phantom parameter in the .mli on type [_ Rvalue.t],
     this function can never be called with an implicit pointer construction.
     This is important since it would be wrong to put [DW_OP_stack_value]
     after such a construction. *)
  let of_rvalue t =
    t @ [O.DW_op_stack_value]

  let implicit_pointer ~offset_in_bytes ~die_label dwarf_version =
    [OB.implicit_pointer ~offset_in_bytes ~die_label dwarf_version]
end

module Rvalue = struct
  type 'a t = 'a rvalue

  let signed_int_const i = [
    OB.signed_int_const i;
  ]

  let float_const i = [
    OB.float_const i;
  ]

  let const_symbol symbol = [
    OB.value_of_symbol ~symbol;
  ]

  let in_register ~dwarf_reg_number = [
    OB.contents_of_register ~dwarf_reg_number;
  ]

  let in_stack_slot ~offset_in_words = 
    let offset_in_bytes =
      Targetint.mul offset_in_words Targetint.size_in_bytes_as_targetint
    in
    OB.contents_of_stack_slot ~offset_in_bytes

  let read_field ~block ~field =
    let offset_in_bytes =
      Targetint.mul field Targetint.size_in_bytes_as_targetint
    in
    (OB.signed_int_const Targetint.zero) ::
      block @ [
        O.DW_op_dup;
      ] @
      OB.conditional ~if_zero:[
        ]
        ~if_nonzero:([
          O.DW_op_swap;
          O.DW_op_drop;
        ] @ OB.add_unsigned_const offset_in_bytes @ [
          O.DW_op_deref
        ])
        ~at_join:[
        ]

  let read_symbol_field symbol ~field =
    read_field ~block:(const_symbol symbol) ~field

  let location_from_another_die ~die_label ~compilation_unit_header_label =
    [OB.call ~die_label ~compilation_unit_header_label]

  let implicit_pointer ~offset_in_bytes ~die_label dwarf_version =
    [OB.implicit_pointer ~offset_in_bytes ~die_label dwarf_version]
end

let of_lvalue t = t
let of_lvalue_without_address t = t
let of_rvalue t = t

let compile t = t
