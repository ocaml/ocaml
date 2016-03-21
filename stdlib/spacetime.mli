(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Profiling of a program's space behaviour over time.
    Currently only supported on x86-64 platforms running 64-bit code.

    This module may only be used when the -spacetime option was passed
    to the configure script for the compiler being used.

    For functions to decode the information recorded by the profiler,
    see the Spacetime offline library in otherlibs/. *)

module Series : sig

  (** Type representing a file that will hold a series of heap snapshots
      together with additional information required to interpret those
      snapshots. *)
  type t

  (** [create ~path] creates a series file at [path]. *)
  val create : path:string -> t

  (** [save_and_close series] writes information into [series] required for
      interpeting the snapshots that [series] contains and then closes the
      [series] file. This function must be called to produce a valid series
      file. *)
  val save_and_close : t -> unit

end

module Snapshot : sig

  (** [take series] takes a snapshot of the profiling annotations on the values
      in the minor and major heaps, together with GC stats, and write the
      result to the [series] file.  This function performs only a very small
      amount of allocation.  It does not explicitly trigger a GC. *)
  val take : Series.t -> unit

end
