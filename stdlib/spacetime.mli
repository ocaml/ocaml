(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2015, Jane Street Group, LLC                       *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)
(* CR mshinwell: fix comment *)
(** Profiling of a program's space behaviour over time.
    For 64-bit targets only.
    This module may only be used when the -spacetime option is passed
    to the configure script.  The option must be passed for all object
    files, library and package constructions involved in the program.
    For functions to decode the information recorded by the profiler,
    see the Spacetime offline library in otherlibs/. *)

module Annotation : sig
  (** An annotation written into a value's header.  These may be looked up
      in a [Trace.t] (see below). *)
  type t

  (* CR-someday mshinwell: consider using tag and size to increase the
     available space of annotations.  Need to be careful of [Obj.truncate].
     Could also randomise the tags on records.
  *)

  val to_int : t -> int
end

(* [erase_profiling_annotations] erases Spacetime profiling
   information on all values.  This is useful at the start of some
   benchmarking period---for example to exclude allocations associated
   with one-time startup costs from a profile.  It is also important in
   conjunction with [annotate_values_with_given_integer] (see below).
*)
val erase_profiling_annotations : unit -> unit

(* Going forward, annotate all allocated values with their allocation location
   (the default). *)
val annotate_values_with_allocation_location : unit -> unit

(* Going forward, annotate all allocated values with the given integer.
   [`Out_of_range] is returned if the supplied integer is negative or larger
   than [max_annotation_value ()].
   It is not sensible to mix this kind of annotation with the location
   annotation provided by the OCaml system.  To this end, before using
   [annotate_values_with_given_integer], it is recommended to call
   [erase_profiling_annotations] first.
*)
val annotate_values_with_given_integer : Annotation.t -> [ `Ok | `Out_of_range ]

(* Returns the profiling annotation on a given value.  This is only sensible
   to call after [annotate_values_with_given_integer]. *)
val annotation_of_value : 'a -> Annotation.t

module Heap_snapshot : sig
  (* CR mshinwell: consider "Series.create" or similar *)
  module Writer : sig
    type t

    val create : pathname_prefix:string -> t
    val save_trace_and_close : t -> unit
  end

  (** Take a snapshot of the profiling annotations on the values in the minor
      and major heaps, together with GC stats, and write the result to a file.
      This function performs only a very small amount of allocation.  It does
      not explicitly trigger a GC. *)
  val take : Writer.t -> unit
end

module Trace : sig
  (** Dump the current trace to stdout (version written in C). *)
  val debug : unit -> unit
end
