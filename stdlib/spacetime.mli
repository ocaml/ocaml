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

(** Profiling of a program's space behaviour over time.
    Currently only supported on x86-64 platforms running 64-bit code.

    This module may only be used when the -spacetime option was passed
    to the configure script for the compiler being used.

    For functions to decode the information recorded by the profiler,
    see the Spacetime offline library in otherlibs/. *)

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
