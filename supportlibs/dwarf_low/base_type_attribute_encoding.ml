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

type user = Int8.t

type t =
  | Address
  | Boolean
  | Complex_float
  | Float
  | Signed
  | Signed_char
  | Unsigned
  | Unsigned_char
  | Imaginary_float
  | Packed_decimal
  | Numeric_string
  | Edited
  | Signed_fixed
  | Unsigned_fixed
  | Decimal_float
  | UTF
  | User of user

let lo_user = 0x80
let hi_user = 0xff

let encode t =
  let code =
    match t with
    | Signed -> 0x05
    | Address -> 0x01
    | Boolean -> 0x02
    | Complex_float -> 0x03
    | Float -> 0x04
    | Signed -> 0x05
    | Signed_char -> 0x06
    | Unsigned -> 0x07
    | Unsigned_char -> 0x08
    | Imaginary_float -> 0x09
    | Packed_decimal -> 0x0a
    | Numeric_string -> 0x0b
    | Edited -> 0x0c
    | Signed_fixed -> 0x0d
    | Unsigned_fixed -> 0x0e
    | Decimal_float -> 0x0f
    | UTF -> 0x10
    | User code ->
      assert (code >= lo_user && code <= hi_user);
      code
  in
  Dwarf_value.constant_one_byte_int code

let emit t asm = Dwarf_value.emit (encode t) asm
let size t = Dwarf_value.size (encode t)
