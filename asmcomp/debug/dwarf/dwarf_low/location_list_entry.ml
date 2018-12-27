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

include Location_or_range_list_entry.Make (struct
  module Payload = Counted_location_description

  let code_for_entry_kind (entry : _ Location_or_range_list_entry.entry) =
    match entry with
    (* DWARF-5 spec page 227. *)
    | End_of_list -> 0x00
    | Base_addressx _ -> 0x01
    | Startx_endx _ -> 0x02
    | Startx_length _ -> 0x03
    | Offset_pair _ -> 0x04
    | Base_address _ -> 0x06
    | Start_end _ -> 0x07
    | Start_length _ -> 0x08

  let section : Asm_section.dwarf_section = Debug_loclists
end)
