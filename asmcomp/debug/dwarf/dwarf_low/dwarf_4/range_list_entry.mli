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

(** DWARF 4 range list entries.

    Range lists describe non-contiguous address ranges associated with
    a DIE (e.g., a function split across multiple locations due to
    optimization).

    See DWARF 4 specification section 2.17. *)

type t

(** Create a range list entry from an address range *)
val create : Address_range.t -> t

(** Create from start and end addresses *)
val create_from_addresses :
  start:Code_address.t ->
  end_:Code_address.t ->
  t

(** Get the address range *)
val range : t -> Address_range.t

(** Get the start address *)
val start_address : t -> Code_address.t

(** Get the end address *)
val end_address : t -> Code_address.t

(** Convert to human-readable string *)
val to_string : t -> string

(** Pretty-printer *)
val print : Format.formatter -> t -> unit
