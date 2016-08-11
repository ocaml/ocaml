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

type t =
  | Dwarf_2
  | Dwarf_3
  | Dwarf_4

let two = Dwarf_2
let three = Dwarf_3
let four = Dwarf_4

let encode t =
  let code =
    match t with
    | Dwarf_2 -> 2
    | Dwarf_3 -> 3
    | Dwarf_4 -> 4
  in
  Dwarf_value.Int16 (Numbers.Int16.of_int_exn code)

let size t =
  Dwarf_value.size (encode t)

let emit t asm =
  Dwarf_value.emit (encode t) asm
