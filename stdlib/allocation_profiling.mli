(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013, Jane Street Holding                                *)
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

(* Access to the allocation profiler from OCaml programs.

   To use the allocation profiling you should:
   - compile for native code;
   - add "-allocation-profiling" to the ocamlopt command line;
   - set OCAMLRUNPARAM to include the T character before running your
     program, e.g.
        $ OCAMLRUNPARAM=T ./myocamlprogram
*)

(* Note: The following functions are not currently supported except
   on Linux systems.

        where_was_value_allocated
        where_was_value_allocated_address_only

   In other words, use the [Heap_snapshot] or [Global] modules if
   not running on Linux, for the moment.
*)

module Source_location : sig
  (* A value of type [t] identifies a point in a program's source code. *)
  type t

  val filename : t -> string option
  val function_name : t -> string option
  val line_number : t -> int option

  val to_string : t -> string
end

type t = [
  | `Not_boxed
  | `Unknown
  | `At_address of Int64.t
  | `At_source_location of Source_location.t
  | `Between_source_locations of
    Source_location.t option * Int64.t * Source_location.t option
]

(* [where_was_value_allocated v] attempts to determine the source
   location at which the value [v] was allocated.  If the source location
   cannot be determined, but the virtual memory address of the allocation
   point is known, that address will be returned instead.  The location
   of some values may be completely unknown.  If [v] is not a boxed
   (allocated) value then [`Not_boxed] will be returned.

   This function can resolve source locations even for values allocated
   outside OCaml code (for example in C bindings).

   Upon the first call to this function it will block for a short time
   whilst it reads part of the current program's executable from disk.
   The executable should not be changed on disk whilst the program is
   running if it makes use of this function.

   This function is thread safe, although, if it is called from another
   thread whilst in progress reading the executable file then it may
   return [`At_address] instead of [`At_source_location] or
   [`Between_source_locations] during that period.
*)
val where_was_value_allocated : 'a -> t

(* [where_was_value_allocated_address_only] is like
   [where_was_value_allocated], except that it does not access the
   program's executable on disk, and does not resolve addresses to
   source locations.  It runs in (short) constant time.  It is
   thread safe. *)
val where_was_value_allocated_address_only : 'a
  -> [ `Not_boxed | `Unknown | `At_address of Int64.t ]

(* [to_string t] produces a human-readable representation of [t]. *)
val to_string : t -> string

module Heap_snapshot : sig
  (* This module contains profiling functions that work on snapshots
     of the Caml heap.

     All of these functions cause a minor garbage collection
     accompanied by the usual associated slice of major collection.
  *)

  (* [dump_allocators_of_major_heap_blocks] writes a file that may be
     decoded using tools/allocation-profiling/decode-major-heap.sh
     in order to show, for each block in the major heap, where it was
     allocated.  For example:
       decode-major-heap.sh profile-output-file executable-file
  *)
  val dump_allocators_of_major_heap_blocks : filename:string -> unit

  (* [dump_heapgraph] writes two files that may be decoded using
     tools/allocation-profiling/decode-heapgraph.sh in order to show
     the graph of values in the heap quotiented by the equivalence
     relation that identifies two values iff they were allocated at
     the same source location.  This enables judgements such as
     "a lot of values were allocated at foo.ml line 36 and they were
     pointed at by values allocated at bar.ml line 42".
     Example script invocation:
       decode-heapgraph.sh node-filename edge-filename executable-file
  *)
  val dump_heapgraph : node_filename:string
    -> edge_filename:string
    -> unit

  (* [forget_where_values_were_allocated] erases allocation profiling
     information on all values.  This is useful at the start of some
     benchmarking period---for example to exclude allocations associated
     with one-time startup costs from a profile.

     This function does not just affect the results of the functions
     in this [Heap_snapshot] module; it also affects the results of
     [where_was_value_allocated], etc, above.  It does not however
     affect the functions in the [Global] module below.
  *)
  val forget_where_values_were_allocated : unit -> unit
end

module Global : sig
  (* This module contains functions that provide access to global
     profiling information rather than working on current snapshots
     of the heap.
  *)

  (* [dump_allocations_by_address] writes a file that may be decoded
     using tools/allocation-profiling/decode-by-address.sh in order
     to show the total number of values and words allocated at each
     program source location.  These counts will increment from the
     start of the program unless the reset function, below, is
     called.  Example script invocation:
       decode-by-address.sh profile-output-file executable-file
  *)
  val dump_allocations_by_address : filename:string -> unit

  (* [reset_allocations_by_address] resets all counts to zero. *)
  val reset_allocations_by_address : unit -> unit
end

(* The following is only for the internal use of the OCaml system.
   User code should use the functions provided above. *)
module Source_location_map : sig
  type t
  val create_from_dwarf_then_stuff_into_elf_section_exn : executable:string
    -> run_command:(string -> unit)
    -> unit
  val create_from_elf_section : executable:string -> t option
  val resolve : t
    -> instr_pointer:Int64.t
    -> [ `Exact of Source_location.t
       | `Between of Source_location.t option * Source_location.t option] option
end

(* CR mshinwell: enhance functionality to permit discovery of _when_
   a value was allocated. *)
