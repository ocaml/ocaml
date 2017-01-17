(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** An integer that has the same width as the current DWARF format.

    Such integers are required to describe offsets within DWARF sections;
    they may be wider than the machine's native integers.

    Note that [Dwarf_format.set] must be called before using this module,
    otherwise exceptions may be raised.
*)

type t

val zero : unit -> t
val to_int64 : t -> Int64.t

include Dwarf_emittable.S with type t := t
