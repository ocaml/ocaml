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

type t = Dwarf_4_range_list.t list ref

let create () = ((ref []) : t)

let insert t ~range_list =
  let attribute_referencing_the_new_list =
    let spec =
      Dwarf_attributes.Attribute_specification.create
        (Dwarf_attributes.Attribute.Dwarf_4 Ranges)
        (Dwarf_attributes.Form.Dwarf_4 Sec_offset_rangelistptr)
    in
    Dwarf_attribute_values.Attribute_value.create spec
      (Dwarf_attribute_values.Value.offset_into_debug_ranges
        (Dwarf_4_range_list.label range_list))
  in
  t := range_list :: !t;
  attribute_referencing_the_new_list

let size t =
  List.fold_left (fun size range_list ->
      Dwarf_int.add size (Dwarf_4_range_list.size range_list))
    (Dwarf_int.zero ())
    !t

let emit t =
  List.iter (fun range_list -> Dwarf_4_range_list.emit range_list) !t
