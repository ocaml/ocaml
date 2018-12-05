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

module A = Asm_directives

type 'payload entry =
  | End_of_list
  | Base_addressx of Address_index.t
  | Startx_endx of {
      start_inclusive : Address_index.t;
      end_exclusive : Address_index.t;
      payload : 'a;
    }
  | Startx_length of {
      start_inclusive : Address_index.t;
      length : Targetint.t;
      payload : 'a;
    }
  | Offset_pair of {
      start_offset_inclusive : Targetint.t;
      end_offset_exclusive : Targetint.t;
      payload : 'a;
    }
  | Base_address of Asm_symbol.t
  | Start_end of {
      start_inclusive : Asm_label.t;
      end_exclusive : Asm_label.t;
      end_adjustment : int;
      payload : 'a;
    }
  | Start_length of {
      start_inclusive : Asm_label.t;
      length : Targetint.t;
      payload : 'a;
    }

module type S = sig
  type payload
  type nonrec entry = payload entry
  type t

  val create : entry -> start_of_code_symbol:Asm_symbol.t -> t

  include Dwarf_emittable.S with type t := t
end

module Make (P : sig
  module Payload : Dwarf_emittable.S
  val code_for_entry_kind : _ entry -> int
end) = struct
  module P = P.Payload

  type t = {
    entry : entry;
    start_of_code_symbol : Asm_symbol.t;
  }

  let create entry ~start_of_code_symbol =
    { entry;
      start_of_code_symbol;
    }

  let label_address t label ~adjustment =
    let adjustment = Targetint.of_int_exn adjustment in
    Dwarf_value.code_address_from_label_symbol_diff
      ~comment:"ending address"
      ~upper:label
      ~lower:t.start_of_code_symbol
      ~offset_upper:adjustment

  let size0 t =
    match t.entry with
    | End_of_list -> Dwarf_int.zero
    | Base_addressx addr_index -> Address_index.size addr_index
    | Startx_endx {
        start_inclusive;
        end_exclusive;
        loc_desc;
      } ->
      Dwarf_int.add (Address_index.size start_inclusive)
        (Dwarf_int.add (Address_index.size end_exclusive)
          (Payload.size loc_desc))
    | Startx_length {
        start_inclusive;
        length;
        loc_desc;
      } ->
      Dwarf_int.add (Address_index.size start_inclusive)
        (Dwarf_int.of_int (Targetint.width_in_bytes length)
          (Payload.size loc_desc))
    | Offset_pair of {
        start_offset_inclusive;
        end_offset_exclusive;
        loc_desc;
      } ->
      let start_offset_inclusive =
        Dwarf_value.leb128 (Targetint.to_int64 start_offset_inclusive)
      in
      let end_offset_exclusive =
        Dwarf_value.leb128 (Targetint.to_int64 end_offset_exclusive)
      in
      Dwarf_int.add (Dwarf_value.size start_offset_inclusive)
        (Dwarf_int.add (Dwarf_value.size end_offset_exclusive)
          (Payload.size loc_desc))
    | Base_address _sym ->
      Dwarf_int.of_int_exn Arch.size_addr
    | Start_end {
        start_inclusive;
        end_exclusive;
        end_adjustment;
        loc_desc;
      } ->
      Dwarf_int.add (Dwarf_int.of_int_exn Arch.size_addr)
        (Dwarf_int.add (Dwarf_int.of_int_exn Arch.size_addr)
          (Payload.size loc_desc))
    | Start_length {
        start_inclusive;
        length;
        loc_desc;
      } ->
      let length = Dwarf_value.uleb128 (Targetint.to_int64 length) in
      Dwarf_int.add (Dwarf_int.of_int_exn Arch.size_addr)
        (Dwarf_int.add (Dwarf_value.size length)
          (Payload.size loc_desc))

  let size t =
    Dwarf_int.add Dwarf_int.one (size0 t)

  let emit t =
    (* DWARF-5 spec page 44 lines 14--15. *)
    A.int8 (Int8.of_int_exn (P.code_for_entry_kind t.entry));
    match t.entry with
    | End_of_list -> ()
    | Base_addressx addr_index ->
      Address_index.emit addr_index
    | Startx_endx {
        start_inclusive;
        end_exclusive;
        payload;
      } ->
      Address_index.emit start_inclusive;
      Address_index.emit end_exclusive;
      Payload.emit payload
    | Startx_length {
        start_inclusive;
        length;
        payload;
      } ->
      Address_index.emit start_inclusive;
      A.targetint length;
      Payload.emit payload
    | Offset_pair of {
        start_offset_inclusive;
        end_offset_exclusive;
        payload;
      } ->
      Dwarf_value.emit (Dwarf_value.leb128 (
        Targetint.to_int64 start_offset_inclusive));
      Dwarf_value.emit (Dwarf_value.leb128 (
        Targetint.to_int64 end_offset_exclusive));
      Payload.emit payload
    | Base_address sym ->
      A.symbol sym
    | Start_end {
        start_inclusive;
        end_exclusive;
        end_adjustment;
        payload;
      } ->
      Dwarf_value.emit (label_address start_inclusive ~adjustment:0);
      Dwarf_value.emit (label_address end_exclusive ~adjustment:end_adjustment);
      Payload.emit payload
    | Start_length {
        start_inclusive;
        length;
        payload;
      } ->
      Dwarf_value.emit (label_address start_inclusive ~adjustment:0);
      Dwarf_value.emit (Dwarf_value.uleb128 (Targetint.to_int64 length));
      Payload.emit payload
end
