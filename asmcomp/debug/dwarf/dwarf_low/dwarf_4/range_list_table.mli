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

(** DWARF 4 range list table (.debug_ranges section).

    The range list table contains all range lists referenced by DIEs
    in the compilation unit. Each range list has an offset that is
    used to reference it.

    See DWARF 4 specification section 2.17. *)

type t

(** Create an empty range list table *)
val create : unit -> t

(** Add a range list and return its offset in the table *)
val add_range_list :
  t ->
  Range_list_entry.t list ->
  int

(** Get all range lists with their offsets *)
val get_all : t -> (int * Range_list_entry.t list) list

(** Check if the table is empty *)
val is_empty : t -> bool

(** Get the number of range lists in the table *)
val count : t -> int

(** Pretty-printer *)
val print : Format.formatter -> t -> unit
