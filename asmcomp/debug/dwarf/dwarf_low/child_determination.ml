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
  | Yes
  | No

let encode = function
  | Yes -> 0x01
  | No -> 0x00

let size _t = Dwarf_int.one ()

let emit t =
  let comment =
    match t with
    | Yes -> "Has children"
    | No -> "No children"
  in
  Dwarf_value.emit (Dwarf_value.int8 ~comment
    (Numbers.Int8.of_int_exn (encode t)))

let compare t1 t2 = Stdlib.compare t1 t2
