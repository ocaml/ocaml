(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t

include Dwarf_emittable.S with type t := t

module type S = sig
  type t

  val empty : t
  val const_symbol : Symbol.t -> t
  val const_int : Int64.t -> t
  val const_int_not_ocaml_encoded : Int64.t -> t
  (* CR-soon mshinwell: consider a new type to identify whose register
     numbering is in use here *)
  val in_register : reg_number:int -> t
  val in_stack_slot : offset_in_words:int -> t
  val in_register_yielding_stack_value : reg_number:int -> t
  val in_stack_slot_yielding_stack_value : offset_in_words:int -> t
  val read_symbol_field : symbol:Symbol.t -> field:int -> t
  val read_symbol_field_yielding_rvalue : symbol:Symbol.t -> field:int -> t
  val read_field : t -> field:int -> t
  val offset_pointer : t -> offset_in_words:int -> t
  val location_from_another_die
     : die_label:Cmm.label
    -> compilation_unit_header_label:Linearize.label
    -> t
  val implicit_pointer
     : offset_in_bytes:int
    -> die_label:Cmm.label
    -> dwarf_version:Dwarf_version.t
    -> t
end

include S with type t := t
