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

(** Versions of the DWARF standard. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = private
  | Four
  | Five

include Dwarf_emittable.S with type t := t

val four : t
val five : t

val compare : t -> t -> int
