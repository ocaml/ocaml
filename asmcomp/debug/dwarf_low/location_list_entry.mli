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

(** DWARF location list entries (DWARF-5 spec section 2.6.2, pages 43--45). *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The "DW_LLE_" prefix is omitted. *)
type t =
  | End_of_list
  | Base_addressx of Address_index.t
  | Startx_endx of {
      start_inclusive : Address_index.t;
      end_exclusive : Address_index.t;
      loc_desc : Counted_location_description.t;
    }
  | Startx_length of {
      start_inclusive : Address_index.t;
      length : Targetint.t;
      loc_desc : Counted_location_description.t;
    }
  | Offset_pair of {
      start_offset_inclusive : Targetint.t;
      end_offset_exclusive : Targetint.t;
      loc_desc : Counted_location_description.t;
    }
  | Default_location of Counted_location_description.t
  | Base_address of Targetint.t
  | Start_end of {
      start_inclusive : Targetint.t;
      end_exclusive : Targetint.t;
      loc_desc : Counted_location_description.t;
    }
  | Start_length of {
      start_inclusive : Targetint.t;
      length : Targetint.t;
      loc_desc : Counted_location_description.t;
    }

include Dwarf_emittable.S with type t := t
