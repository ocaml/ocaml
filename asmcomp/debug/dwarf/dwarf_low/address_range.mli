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

(** Address ranges in the target program.

    Represents contiguous ranges of code addresses, used for
    defining the scope of DIEs, location lists, and other DWARF
    structures. *)

type t

(** Create an address range from start (inclusive) to end (exclusive) *)
val create : start:Code_address.t -> end_:Code_address.t -> t

(** Get the start address of the range *)
val start : t -> Code_address.t

(** Get the end address of the range *)
val end_ : t -> Code_address.t

(** Check if an address falls within this range *)
val contains : t -> Code_address.t -> bool

(** Check if this range overlaps with another *)
val overlaps : t -> t -> bool

(** Convert to human-readable string *)
val to_string : t -> string

(** Pretty-printer *)
val print : Format.formatter -> t -> unit

(** Compare two address ranges *)
val compare : t -> t -> int

(** Equality *)
val equal : t -> t -> bool
