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
  | Yes
  | No

let encode = function
  | Yes -> 0x01
  | No -> 0x00

let size _t = Int64.of_int 1

let emit t asm =
  Dwarf_value.emit (Dwarf_value.Int8 (
    Numbers.Int8.of_int_exn (encode t))) asm
