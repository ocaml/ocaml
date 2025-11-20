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

(** LEB128 (Little Endian Base 128) encoding for DWARF.

    LEB128 is a variable-length encoding used throughout DWARF to
    compactly represent integers. *)

(** Encode an unsigned integer as ULEB128 *)
val encode_uleb128 : int -> bytes

(** Encode a signed integer as SLEB128 *)
val encode_sleb128 : int -> bytes

(** Write ULEB128 to a buffer *)
val write_uleb128 : Buffer.t -> int -> unit

(** Write SLEB128 to a buffer *)
val write_sleb128 : Buffer.t -> int -> unit

(** Get the size in bytes of a ULEB128 encoding *)
val uleb128_size : int -> int

(** Get the size in bytes of a SLEB128 encoding *)
val sleb128_size : int -> int
