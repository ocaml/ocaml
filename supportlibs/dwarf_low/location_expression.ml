(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module SLE = Simple_location_expression

(* We do not currently need composite location descriptions. *)
type t = SLE.t

let const_symbol = SLE.const_symbol
let const_int = SLE.const_int
let in_register = SLE.in_register
let in_stack_slot = SLE.in_stack_slot
let read_symbol_field = SLE.read_symbol_field
let read_field = SLE.read_field
let offset_pointer = SLE.offset_pointer

let size = SLE.size
let emit = SLE.emit
