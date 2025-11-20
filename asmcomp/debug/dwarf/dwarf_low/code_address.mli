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

(** Code addresses in the target program.

    Represents addresses of instructions in the compiled code,
    used for address ranges, breakpoints, and PC values. *)

type t

(** Create a code address from a label *)
val from_label : string -> t

(** Create a code address from an absolute address *)
val from_int64 : int64 -> t

(** Check if this is a label-based address *)
val is_label : t -> bool

(** Get the label name if this is a label-based address *)
val label : t -> string option

(** Get the absolute address if this is an absolute address *)
val absolute : t -> int64 option

(** Convert to human-readable string *)
val to_string : t -> string

(** Pretty-printer *)
val print : Format.formatter -> t -> unit

(** Compare two code addresses *)
val compare : t -> t -> int

(** Equality *)
val equal : t -> t -> bool
