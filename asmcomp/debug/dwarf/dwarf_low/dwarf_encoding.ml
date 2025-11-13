(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | DW_ATE_address
  | DW_ATE_boolean
  | DW_ATE_complex_float
  | DW_ATE_float
  | DW_ATE_signed
  | DW_ATE_signed_char
  | DW_ATE_unsigned
  | DW_ATE_unsigned_char
  | DW_ATE_imaginary_float
  | DW_ATE_packed_decimal
  | DW_ATE_numeric_string
  | DW_ATE_edited
  | DW_ATE_signed_fixed
  | DW_ATE_unsigned_fixed
  | DW_ATE_decimal_float
  | DW_ATE_UTF
  | DW_ATE_UCS
  | DW_ATE_ASCII

(* DWARF 4 specification section 7.8 *)
let to_code = function
  | DW_ATE_address -> 0x01
  | DW_ATE_boolean -> 0x02
  | DW_ATE_complex_float -> 0x03
  | DW_ATE_float -> 0x04
  | DW_ATE_signed -> 0x05
  | DW_ATE_signed_char -> 0x06
  | DW_ATE_unsigned -> 0x07
  | DW_ATE_unsigned_char -> 0x08
  | DW_ATE_imaginary_float -> 0x09
  | DW_ATE_packed_decimal -> 0x0a
  | DW_ATE_numeric_string -> 0x0b
  | DW_ATE_edited -> 0x0c
  | DW_ATE_signed_fixed -> 0x0d
  | DW_ATE_unsigned_fixed -> 0x0e
  | DW_ATE_decimal_float -> 0x0f
  | DW_ATE_UTF -> 0x10
  | DW_ATE_UCS -> 0x11
  | DW_ATE_ASCII -> 0x12

let to_string = function
  | DW_ATE_address -> "DW_ATE_address"
  | DW_ATE_boolean -> "DW_ATE_boolean"
  | DW_ATE_complex_float -> "DW_ATE_complex_float"
  | DW_ATE_float -> "DW_ATE_float"
  | DW_ATE_signed -> "DW_ATE_signed"
  | DW_ATE_signed_char -> "DW_ATE_signed_char"
  | DW_ATE_unsigned -> "DW_ATE_unsigned"
  | DW_ATE_unsigned_char -> "DW_ATE_unsigned_char"
  | DW_ATE_imaginary_float -> "DW_ATE_imaginary_float"
  | DW_ATE_packed_decimal -> "DW_ATE_packed_decimal"
  | DW_ATE_numeric_string -> "DW_ATE_numeric_string"
  | DW_ATE_edited -> "DW_ATE_edited"
  | DW_ATE_signed_fixed -> "DW_ATE_signed_fixed"
  | DW_ATE_unsigned_fixed -> "DW_ATE_unsigned_fixed"
  | DW_ATE_decimal_float -> "DW_ATE_decimal_float"
  | DW_ATE_UTF -> "DW_ATE_UTF"
  | DW_ATE_UCS -> "DW_ATE_UCS"
  | DW_ATE_ASCII -> "DW_ATE_ASCII"

let print ppf encoding =
  Format.fprintf ppf "%s" (to_string encoding)
