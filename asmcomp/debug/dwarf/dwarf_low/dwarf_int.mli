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

(** An integer that has the same width as the current DWARF format.

    Such integers (DWARF-4 specification section 7.4) are required to describe
    offsets within DWARF sections; they may be wider than the machine's native
    integers.

    All of the [size] functions for measuring the size of encoded DWARF
    constructs in this library return values of type [t], even if the encoding
    of such sizes may sometimes be done via a variable-length encoding.  The
    reason that it is correct to use values of type [t] is because the
    width of the integers giving the size of any such constructs cannot
    exceed the width of the DWARF format.  If they were to exceed such width
    then it would not be possible to encode every offset into the section
    containing such constructs using a DWARF-width integer, as mandated by the
    standard.

    Note that [Dwarf_format.set] must be called before using this module,
    otherwise exceptions may be raised.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val print : Format.formatter -> t -> unit

val zero : unit -> t
val one : unit -> t
val two : unit -> t
val four : unit -> t
val eight : unit -> t

val of_host_int_exn : int -> t
val of_int64_exn : Int64.t -> t
val of_targetint_exn : Targetint.t -> t

val to_int64 : t -> Int64.t
val to_uint64_exn : t -> Numbers.Uint64.t

val width_as_int64 : unit -> Int64.t

(** [add] and [succ] both check for overflow. *)
val add : t -> t -> t
val succ : t -> t

val size : t -> t
val emit : ?comment:string -> t -> unit
