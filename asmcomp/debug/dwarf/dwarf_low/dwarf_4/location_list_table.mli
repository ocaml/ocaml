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

(** DWARF 4 location list table (.debug_loc section).

    The location list table contains all location lists referenced by
    DIEs in the compilation unit. Each location list has an offset that
    is used to reference it.

    See DWARF 4 specification section 2.6. *)

type t

(** Create an empty location list table *)
val create : unit -> t

(** Add a location list and return its offset in the table *)
val add_location_list :
  t ->
  Location_list_entry.t list ->
  int

(** Get all location lists with their offsets *)
val get_all : t -> (int * Location_list_entry.t list) list

(** Check if the table is empty *)
val is_empty : t -> bool

(** Get the number of location lists in the table *)
val count : t -> int

(** Pretty-printer *)
val print : Format.formatter -> t -> unit
