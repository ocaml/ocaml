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

(** DWARF 4 location list entries.

    Location lists describe where variables are located during different
    parts of program execution. A location list is a sequence of entries,
    each specifying an address range and a location description.

    See DWARF 4 specification section 2.6. *)

type t

(** Create a location list entry for a specific address range *)
val create :
  start_address:Code_address.t ->
  end_address:Code_address.t ->
  location:bytes ->
  t

(** Get the start address of this entry *)
val start_address : t -> Code_address.t

(** Get the end address of this entry *)
val end_address : t -> Code_address.t

(** Get the location description (DWARF expression) *)
val location : t -> bytes

(** Convert to human-readable string *)
val to_string : t -> string

(** Pretty-printer *)
val print : Format.formatter -> t -> unit
