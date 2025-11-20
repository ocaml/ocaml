(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Joel Reymont                                     *)
(*                                                                        *)
(*   Copyright 2024 Joel Reymont                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** DWARF attribute values.

    This module defines the various types of values that can be
    associated with DWARF attributes. *)

(** An offset within a DWARF section *)
type offset = int

(** An absolute address in the target's address space *)
type address = int64

(** A reference to a DIE, either by offset or by unique identifier *)
type die_reference =
  | Offset of offset
  | Unique_id of int

(** Constant values *)
type constant =
  | Int of int
  | Int64 of int64
  | String of string

(** Block of bytes *)
type block = bytes

(** A DWARF attribute value *)
type t =
  | Address of address
  | Label_address of string  (** Symbolic address requiring relocation *)
  | Block of block
  | Constant of constant
  | String of string
  | Flag of bool
  | Reference of die_reference
  | Expr_loc of block  (** DWARF expression/location *)
  | Sec_offset of offset  (** Section offset *)
  | Label_sec_offset of string  (** Symbolic section offset requiring relocation *)

(** Pretty-printer for values *)
val print : Format.formatter -> t -> unit
