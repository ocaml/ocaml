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

type t = Location_list.t list ref

let create () = ((ref []) : t)

let insert t ~location_list =
  let attribute_referencing_the_new_list =
    let spec =
      Dwarf_attributes.Attribute_specification.create
        Dwarf_attributes.Attribute.Location
        Dwarf_attributes.Form.Sec_offset_loclistptr
    in
    Dwarf_attribute_values.Attribute_value.create spec
      (Dwarf_attribute_values.Value.offset_into_debug_loc
        (Location_list.label location_list))
  in
  t := location_list :: !t;
  attribute_referencing_the_new_list

let size t =
  List.fold_left (fun size loc_list ->
      Int64.add size (Location_list.size loc_list))
    0L
    !t

let emit t asm =
  List.iter (fun loc_list -> Location_list.emit loc_list asm) !t
