(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Whether we are emitting 32-bit or 64-bit DWARF.
    Note that this width does not necessarily coincide with the width of a
    native integer on the target processor.  (DWARF-4 standard section 7.4,
    page 142). *)

type t =
  | Thirty_two
  | Sixty_four

val set : t -> unit

(** [get] raises if [set] has not been called beforehand. *)
val get : unit -> t

(** Exception raised when construction of 32-bit DWARF fails due to
    outsize sections. *)
exception Too_large_for_thirty_two_bit_dwarf
