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
module Uint8 = Numbers.Uint8
module Uint32 = Numbers.Uint32
module Uint64 = Numbers.Uint64

module Make (Location_or_range_list : Dwarf_emittable.S) = struct
  type t = {
    base_addr : Asm_label.t;
    mutable num_lists : int;
    mutable current_offset_from_first_list : Dwarf_int.t;
    mutable lists_with_offsets_from_first_list
      : (Location_or_range_list.t * Dwarf_int.t) list;
  }

  module Index = struct
    type t = Uint64.t

    let create index = Uint64.of_int_exn index

    let to_uint64 t = t
  end

  let create () =
    { base_addr = Asm_label.create ();
      num_lists = 0;
      current_offset_from_first_list = Dwarf_int.zero ();
      lists_with_offsets_from_first_list = [];
    }

  let add t list =
    let which_index = t.num_lists in
    t.lists_with_offsets_from_first_list
      <- (list, t.current_offset_from_first_list)
           :: t.lists_with_offsets_from_first_list;
    let next_offset_from_first_list =
      let list_size = Location_or_range_list.size list in
      Dwarf_int.add list_size t.current_offset_from_first_list
    in
    t.current_offset_from_first_list <- next_offset_from_first_list;
    t.num_lists <- t.num_lists + 1;
    Index.create which_index

  let base_addr t = t.base_addr

  let offset_entry_count t =
    Uint32.of_int_exn (List.length t.lists_with_offsets_from_first_list)

  let offset_array_size t =
    Dwarf_int.of_int64_exn (
      Int64.mul (Dwarf_int.width_as_int64 ())
        (Uint32.to_int64 (offset_entry_count t)))

  let initial_length t =
    let lists_size =
      List.fold_left (fun lists_size (list, _offset_from_first_list) ->
          Dwarf_int.add lists_size (Location_or_range_list.size list))
        (Dwarf_int.eight ())  (* DWARF-5 spec page 242 lines 12--20. *)
        t.lists_with_offsets_from_first_list
    in
    Initial_length.create (Dwarf_int.add (offset_array_size t) lists_size)

  let size t =
    let initial_length = initial_length t in
    Dwarf_int.add (Initial_length.size initial_length)
      (Initial_length.to_dwarf_int initial_length)

  let emit t =
    Initial_length.emit (initial_length t);
    Dwarf_version.emit Dwarf_version.five;
    A.uint8 ~comment:"Arch.size_addr" (Uint8.of_int_exn Arch.size_addr);
    A.uint8 ~comment:"Segment selector size" Uint8.zero;
    A.uint32 ~comment:"Offset entry count" (offset_entry_count t);
    A.comment "Base label:";
    A.define_label t.base_addr;
    A.comment "Offset array:";
    let offset_array_size = offset_array_size t in
    List.iter (fun (_list, offset_from_first_list) ->
        let offset =
          Dwarf_int.add offset_array_size offset_from_first_list
        in
        Dwarf_int.emit offset)
      t.lists_with_offsets_from_first_list;
    A.comment "Range or location list(s):";
    List.iter (fun (list, _offset_from_first_list) ->
        Location_or_range_list.emit list)
      t.lists_with_offsets_from_first_list
end
