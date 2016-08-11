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

(** DWARF base type attribute encodings (DWARF-4 specification section 7.8). *)

type user = private Int8.t

(** We omit the "DW_ATE_" prefix. *)
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

include Dwarf_emittable with type t := t
