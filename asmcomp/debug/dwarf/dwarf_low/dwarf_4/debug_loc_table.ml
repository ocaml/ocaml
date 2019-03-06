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

type t = Dwarf_4_location_list.t list ref

let create () = ((ref []) : t)

let insert t location_list =
  t := location_list :: !t

let attribute_to_reference_location_list location_list =
  let spec =
    Dwarf_attributes.Attribute_specification.create
      (Dwarf_attributes.Attribute.Dwarf_4 Location)
      (Dwarf_attributes.Form.Dwarf_4 Sec_offset_loclistptr)
  in
  Dwarf_attribute_values.Attribute_value.create spec
    (Dwarf_attribute_values.Value.offset_into_debug_loc
      (Dwarf_4_location_list.label location_list))

let size t =
  List.fold_left (fun size loc_list ->
      Dwarf_int.add size (Dwarf_4_location_list.size loc_list))
    (Dwarf_int.zero ())
    !t

let emit t =
  List.iter (fun loc_list -> Dwarf_4_location_list.emit loc_list)
    (List.rev !t)
