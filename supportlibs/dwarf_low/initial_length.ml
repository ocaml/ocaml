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

(* DWARF-4 standard section 7.4. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(* Even on a 32-bit platform, a DWARF section may be larger than the
   maximum representable positive signed 32-bit integer... *)
type t = Int64.t

let create initial_length = initial_length

let size _t =
  let size =
    match Dwarf_format.get () with
    | Thirty_two -> 4
    | Sixty_four -> 4 + 8
  in
  Int64.of_int size

let sixty_four_bit_indicator = 0xffffffffl

let emit t asm =
  match Dwarf_format.get () with
  | Thirty_two ->
    (* Careful: not "Int64.of_int32 0xfffffff0l", which would sign
       extend. *)
    if Int64.compare t 0xfffffff0L >= 0 then begin
      Misc.fatal_errorf "Initial length value %Ld is too large for \
          32-bit DWARF"
        t
    end;
    (* CR mshinwell: check this "to_int32" is correct if the size is
       large *)
    Dwarf_value.emit (Dwarf_value.Int32 (Int64.to_int32 t)) asm
  | Sixty_four ->
    Dwarf_value.emit (Dwarf_value.Int32 sixty_four_bit_indicator) asm;
    Dwarf_value.emit (Dwarf_value.Int64 t) asm
