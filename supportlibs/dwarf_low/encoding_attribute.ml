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
  | DW_ATE_signed

let signed = DW_ATE_signed

let encode = function
  | DW_ATE_signed -> 0x05

let size _t = 1

let as_dwarf_value t =
  Dwarf_value.Int8 (Numbers.Int8.of_int_exn (encode t))
