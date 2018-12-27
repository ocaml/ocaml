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

(* DWARF-4 standard section 7.4. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Even on a 32-bit platform, a DWARF section may be larger than the
   maximum representable positive signed 32-bit integer... *)
type t = Dwarf_int.t

let create initial_length = initial_length

let to_dwarf_int t = t

let size t = Dwarf_int.size t

let sixty_four_bit_indicator = 0xffffffffl

let emit t =
  match Dwarf_format.get () with
  | Thirty_two ->
    Dwarf_int.emit ~comment:"32-bit initial length" t
  | Sixty_four ->
    Dwarf_value.emit (
      Dwarf_value.int32 ~comment:"64-bit indicator" sixty_four_bit_indicator);
    Dwarf_int.emit ~comment:"64-bit initial length" t
