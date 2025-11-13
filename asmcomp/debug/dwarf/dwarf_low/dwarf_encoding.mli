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

(** DWARF base type encodings.

    These encodings specify how the bits in a base type should be
    interpreted (e.g., as a signed integer, unsigned integer, float, etc.).

    See DWARF 4 specification section 7.8. *)

type t =
  | DW_ATE_address        (** Linear machine address *)
  | DW_ATE_boolean        (** Boolean *)
  | DW_ATE_complex_float  (** Complex floating-point number *)
  | DW_ATE_float          (** Floating-point number *)
  | DW_ATE_signed         (** Signed binary integer *)
  | DW_ATE_signed_char    (** Signed character *)
  | DW_ATE_unsigned       (** Unsigned binary integer *)
  | DW_ATE_unsigned_char  (** Unsigned character *)
  | DW_ATE_imaginary_float (** Imaginary floating-point number *)
  | DW_ATE_packed_decimal (** Packed decimal *)
  | DW_ATE_numeric_string (** Numeric string *)
  | DW_ATE_edited         (** Edited string *)
  | DW_ATE_signed_fixed   (** Signed fixed-point scaled integer *)
  | DW_ATE_unsigned_fixed (** Unsigned fixed-point scaled integer *)
  | DW_ATE_decimal_float  (** Decimal floating-point number *)
  | DW_ATE_UTF            (** Unicode character *)
  | DW_ATE_UCS            (** UCS character *)
  | DW_ATE_ASCII          (** ASCII character *)

(** Convert an encoding to its numeric code *)
val to_code : t -> int

(** Convert an encoding to a human-readable string *)
val to_string : t -> string

(** Pretty-printer for encodings *)
val print : Format.formatter -> t -> unit
