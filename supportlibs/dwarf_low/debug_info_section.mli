(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Representation of the DWARF .debug_info section. *)

type t

(** It is recommended that the [Dwarf_high] library be used to form
    [dies] (along with the abbreviation table). *)
val create
   : dies:Debugging_information_entry.t list
  -> debug_abbrev_label:Linearize.label
  -> t

include Dwarf_emittable.S with type t := t
