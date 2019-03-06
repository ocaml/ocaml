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
  | DW_ATE_signed
  | DW_ATE_float

let signed = DW_ATE_signed
let float = DW_ATE_float

let name t =
  match t with
  | DW_ATE_signed -> "DW_ATE_signed"
  | DW_ATE_float -> "DW_ATE_float"

let encode = function
  | DW_ATE_float -> 0x04
  | DW_ATE_signed -> 0x05

let size _t = 1

let as_dwarf_value t =
  Dwarf_value.int8 ~comment:(name t) (Numbers.Int8.of_int_exn (encode t))
