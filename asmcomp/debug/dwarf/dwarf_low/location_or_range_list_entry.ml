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
module Int8 = Numbers.Int8

type 'payload entry =
  | End_of_list
  | Base_addressx of Address_index.t
  | Startx_endx of {
      start_inclusive : Address_index.t;
      end_exclusive : Address_index.t;
      payload : 'payload;
    }
  | Startx_length of {
      start_inclusive : Address_index.t;
      length : Targetint.t;
      payload : 'payload;
    }
  | Offset_pair of {
      start_offset_inclusive : Targetint.t;
      end_offset_exclusive : Targetint.t;
      payload : 'payload;
    }
  | Base_address of Asm_symbol.t
  | Start_end of {
      start_inclusive : Asm_label.t;
      end_exclusive : Asm_label.t;
      end_adjustment : int;
      payload : 'payload;
    }
  | Start_length of {
      start_inclusive : Asm_label.t;
      length : Targetint.t;
      payload : 'payload;
    }

module type S = sig
  type payload
  type nonrec entry = payload entry
  type t

  val create : entry -> start_of_code_symbol:Asm_symbol.t -> t

  val section : Asm_section.dwarf_section

  include Dwarf_emittable.S with type t := t
end

module Make (P : sig
  module Payload : Dwarf_emittable.S
  val code_for_entry_kind : _ entry -> int
  val section : Asm_section.dwarf_section
end) = struct
  module Payload = P.Payload

  type payload = Payload.t
  type nonrec entry = Payload.t entry

  type t = {
    entry : entry;
    start_of_code_symbol : Asm_symbol.t;
  }

  let create entry ~start_of_code_symbol =
    { entry;
      start_of_code_symbol;
    }

  let label_address ~comment t label ~adjustment =
    let adjustment = Targetint.of_int_exn adjustment in
    Dwarf_value.code_address_from_label_symbol_diff
      ~comment
      ~upper:label
      ~lower:t.start_of_code_symbol
      ~offset_upper:adjustment
      ()

  let section = P.section

  let size0 t =
    match t.entry with
    | End_of_list -> Dwarf_int.zero ()
    | Base_addressx addr_index -> Address_index.size addr_index
    | Startx_endx {
        start_inclusive;
        end_exclusive;
        payload;
      } ->
      Dwarf_int.add (Address_index.size start_inclusive)
        (Dwarf_int.add (Address_index.size end_exclusive)
          (Payload.size payload))
    | Startx_length {
        start_inclusive;
        length = _;
        payload;
      } ->
      Dwarf_int.add (Address_index.size start_inclusive)
        (Dwarf_int.add
          (Dwarf_int.of_targetint_exn (Targetint.size_in_bytes_as_targetint))
          (Payload.size payload))
    | Offset_pair {
        start_offset_inclusive;
        end_offset_exclusive;
        payload;
      } ->
      let start_offset_inclusive =
        Dwarf_value.uleb128 (Targetint.to_uint64_exn start_offset_inclusive)
      in
      let end_offset_exclusive =
        Dwarf_value.uleb128 (Targetint.to_uint64_exn end_offset_exclusive)
      in
      Dwarf_int.add (Dwarf_value.size start_offset_inclusive)
        (Dwarf_int.add (Dwarf_value.size end_offset_exclusive)
          (Payload.size payload))
    | Base_address _sym ->
      Dwarf_int.of_host_int_exn Arch.size_addr
    | Start_end {
        start_inclusive = _;
        end_exclusive = _;
        end_adjustment = _;
        payload;
      } ->
      Dwarf_int.add (Dwarf_int.of_host_int_exn Arch.size_addr)
        (Dwarf_int.add (Dwarf_int.of_host_int_exn Arch.size_addr)
          (Payload.size payload))
    | Start_length {
        start_inclusive = _;
        length;
        payload;
      } ->
      let length = Dwarf_value.uleb128 (Targetint.to_uint64_exn length) in
      Dwarf_int.add (Dwarf_int.of_host_int_exn Arch.size_addr)
        (Dwarf_int.add (Dwarf_value.size length)
          (Payload.size payload))

  let size t =
    Dwarf_int.succ (size0 t)

  let emit t =
    (* DWARF-5 spec page 44 lines 14--15. *)
    A.comment "List entry:";
    let comment =
      if !Clflags.keep_asm_file then
        let comment =
          match t.entry with
          | End_of_list -> "End_of_list"
          | Base_addressx _ -> "Base_addressx"
          | Startx_endx _ -> "Startx_endx"
          | Startx_length _ -> "Startx_length"
          | Offset_pair _ -> "Offset_pair"
          | Base_address _ -> "Base_address"
          | Start_end _ -> "Start_end"
          | Start_length _ -> "Start_length"
        in
        Some comment
      else
        None
    in
    A.int8 ?comment (Int8.of_int_exn (P.code_for_entry_kind t.entry));
    begin match t.entry with
    | End_of_list -> ()
    | Base_addressx addr_index ->
      Address_index.emit ~comment:"base address" addr_index
    | Startx_endx {
        start_inclusive;
        end_exclusive;
        payload;
      } ->
      Address_index.emit ~comment:"start_inclusive" start_inclusive;
      Address_index.emit ~comment:"end_exclusive" end_exclusive;
      Payload.emit payload
    | Startx_length {
        start_inclusive;
        length;
        payload;
      } ->
      Address_index.emit ~comment:"start_inclusive" start_inclusive;
      A.targetint ~comment:"length" length;
      Payload.emit payload
    | Offset_pair {
        start_offset_inclusive;
        end_offset_exclusive;
        payload;
      } ->
      Dwarf_value.emit (Dwarf_value.sleb128 ~comment:"start_offset_inclusive" (
        Targetint.to_int64 start_offset_inclusive));
      Dwarf_value.emit (Dwarf_value.sleb128 ~comment:"end_offset_exclusive" (
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
      Dwarf_value.emit (
        label_address ~comment:"start_inclusive" t start_inclusive
          ~adjustment:0);
      Dwarf_value.emit (
        label_address ~comment:"end_exclusive" t end_exclusive
          ~adjustment:end_adjustment);
      Payload.emit payload
    | Start_length {
        start_inclusive;
        length;
        payload;
      } ->
      Dwarf_value.emit (
        label_address ~comment:"start_inclusive" t start_inclusive
          ~adjustment:0);
      Dwarf_value.emit (
        Dwarf_value.uleb128 ~comment:"length" (Targetint.to_uint64_exn length));
      Payload.emit payload
    end;
    if !Clflags.keep_asm_file then begin
      A.new_line ()
    end
end
