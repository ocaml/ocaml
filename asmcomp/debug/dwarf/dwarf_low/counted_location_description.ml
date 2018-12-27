(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  loc_desc : Single_location_description.t;
  loc_desc_size : Dwarf_int.t;
}

let create loc_desc =
  { loc_desc;
    loc_desc_size = Single_location_description.size loc_desc;
  }

let size t =
  Dwarf_int.add
    (Dwarf_value.size (Dwarf_value.uleb128 (
      Dwarf_int.to_uint64_exn t.loc_desc_size)))
    t.loc_desc_size

let emit t =
  Dwarf_value.emit (Dwarf_value.uleb128 ~comment:"loc_desc_size" (
    Dwarf_int.to_uint64_exn t.loc_desc_size));
  Single_location_description.emit t.loc_desc
