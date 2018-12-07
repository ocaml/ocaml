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

type t =
  | Dwarf_4
  | Dwarf_5

let four = Dwarf_4
let five = Dwarf_5

let code t =
  match t with
  | Dwarf_4 -> 4
  | Dwarf_5 -> 5

let encode t =
  Dwarf_value.int16 ~comment:"DWARF version" (Numbers.Int16.of_int_exn (code t))

let size t =
  Dwarf_value.size (encode t)

let emit t =
  Dwarf_value.emit (encode t)

let compare t1 t2 = Stdlib.compare (code t1) (code t2)
