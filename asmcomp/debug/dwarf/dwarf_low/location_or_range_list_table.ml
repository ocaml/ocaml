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

module Make (Location_or_range_list : sig
  include Dwarf_emittable.S
  val section : Asm_section.dwarf_section
end) = struct
  type one_list = {
    list : Location_or_range_list.t;
    offset_from_first_list : Dwarf_int.t;
    label : Asm_label.t;
  }

  type t = {
    base_addr : Asm_label.t;
    mutable num_lists : int;
    mutable current_offset_from_first_list : Dwarf_int.t;
    mutable lists : one_list list;
  }

  module Index = struct
    type t = Asm_label.t * Uint64.t

    let create label index = label, Uint64.of_int_exn index

    let to_label (label, _) = label
    let to_uint64 (_, index) = index
  end

  let create () =
    { base_addr = Asm_label.create (DWARF Location_or_range_list.section);
      num_lists = 0;
      current_offset_from_first_list = Dwarf_int.zero ();
      lists = [];
    }

  let add t list =
    let which_index = t.num_lists in
    let one_list =
      { list;
        offset_from_first_list = t.current_offset_from_first_list;
        label = Asm_label.create (DWARF Location_or_range_list.section);
      }
    in
    t.lists <- one_list :: t.lists;
    let next_offset_from_first_list =
      let list_size = Location_or_range_list.size list in
      Dwarf_int.add list_size t.current_offset_from_first_list
    in
    t.current_offset_from_first_list <- next_offset_from_first_list;
    t.num_lists <- t.num_lists + 1;
    Index.create one_list.label which_index

  let base_addr t = t.base_addr

  let offset_array_supported () =
    !Clflags.gdwarf_offsets

  let offset_entry_count t =
    if offset_array_supported () then Uint32.of_int_exn (List.length t.lists)
    else Uint32.zero

  let offset_array_size t =
    if offset_array_supported () then
      Dwarf_int.of_int64_exn (
        Int64.mul (Dwarf_int.width_as_int64 ())
          (Uint32.to_int64 (offset_entry_count t)))
    else
      Dwarf_int.zero ()

  let initial_length t =
    let lists_size =
      List.fold_left (fun lists_size { list; _ } ->
          Dwarf_int.add lists_size (Location_or_range_list.size list))
        (Dwarf_int.eight ())  (* DWARF-5 spec page 242 lines 12--20. *)
        t.lists
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
    if offset_array_supported () then begin
      A.comment "Offset array:";
      let offset_array_size = offset_array_size t in
      List.iteri (fun index { offset_from_first_list; _ } ->
          let offset =
            Dwarf_int.add offset_array_size offset_from_first_list
          in
          let comment =
            if !Clflags.keep_asm_file then
              Some (Printf.sprintf "offset to list number %d" index)
            else
              None
          in
          Dwarf_int.emit ?comment offset)
        t.lists
    end;
    A.comment "Range or location list(s):";
    List.iter (fun { list; label; _ } ->
        A.define_label label;
        Location_or_range_list.emit list)
      t.lists
end
