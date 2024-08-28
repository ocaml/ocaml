(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                          Sadiq Jaffer, Opsian                          *)
(*                                                                        *)
(*   Copyright 2021 Opsian Ltd                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Runtime events - ring buffer-based runtime tracing

    This module enables users to enable and subscribe to tracing events
    from the Garbage Collector and other parts of the OCaml runtime. This can
    be useful for diagnostic or performance monitoring purposes. This module
    can be used to subscribe to events for the current process or external
    processes asynchronously.

    When enabled (either via setting the OCAML_RUNTIME_EVENTS_START environment
    variable or calling Runtime_events.start) a file with the pid of the process
    and extension .events will be created. By default this is in the
    current directory but can be over-ridden by the OCAML_RUNTIME_EVENTS_DIR
    environment variable. Each domain maintains its own ring buffer in a section
    of the larger file into which it emits events.

    There is additionally a set of C APIs in runtime_events.h that can enable
    zero-impact monitoring of the current process or bindings for other
    languages.

    The runtime events system's behaviour can be controlled by the following
    environment variables:

    - OCAML_RUNTIME_EVENTS_START if set will cause the runtime events system
    to be started as part of the OCaml runtime initialization.

    - OCAML_RUNTIME_EVENTS_DIR sets the directory where the runtime events
    ring buffers will be located. If not present the program's working directory
    will be used.

  - OCAML_RUNTIME_EVENTS_PRESERVE if set will prevent the OCaml runtime from
    removing its ring buffers when it terminates. This can help if monitoring
    very short running programs.
*)

(** The type for counter events emitted by the runtime. Counter events are used
  to measure a quantity at a point in time or record the occurence of an event.
  In the latter case their value will be one. *)
type runtime_counter =
| EV_C_FORCE_MINOR_ALLOC_SMALL
(**
Triggering of a minor collection due to a full minor heap.
@since 5.0
*)
| EV_C_FORCE_MINOR_MAKE_VECT
(**
Triggering of a minor collection due to Array.make.
@since 5.0
*)
| EV_C_FORCE_MINOR_SET_MINOR_HEAP_SIZE
(**
Triggering of a minor collection due to Gc.minor_heap_size.
@since 5.0
*)
| EV_C_FORCE_MINOR_MEMPROF
(**
Triggering of a minor collection during memprof young sampling.
@since 5.3
*)
| EV_C_MINOR_PROMOTED
(**
Total words promoted from the minor heap to the major in the last minor
collection.
@since 5.0
*)
| EV_C_MINOR_ALLOCATED
(**
Total {b bytes} allocated in the minor heap in the last minor collection.
@since 5.0
*)
| EV_C_REQUEST_MAJOR_ALLOC_SHR
(**
Major slice requested due to allocation in major heap.
@since 5.0
*)
| EV_C_REQUEST_MAJOR_ADJUST_GC_SPEED
(**
Major slice requested by [caml_adjust_gc_speed].
@since 5.0
*)
| EV_C_REQUEST_MINOR_REALLOC_REF_TABLE
(**
Triggering of a minor collection due to ref table reallocation.
@since 5.0
*)
| EV_C_REQUEST_MINOR_REALLOC_EPHE_REF_TABLE
(**
Triggering of a minor collection due to ephe_ref table reallocation.
@since 5.0
*)
| EV_C_REQUEST_MINOR_REALLOC_CUSTOM_TABLE
(**
Triggering of a minor collection due to custom table reallocation.
@since 5.0
*)
| EV_C_MAJOR_HEAP_POOL_WORDS
(**
Total words in a Domain's major heap pools. This is the sum of unallocated and
live words in each pool.
@since 5.1 *)
| EV_C_MAJOR_HEAP_POOL_LIVE_WORDS
(**
Current live words in a Domain's major heap pools.
@since 5.1 *)
| EV_C_MAJOR_HEAP_LARGE_WORDS
(**
Total words of a Domain's major heap large allocations.
A large allocation is an allocation larger than the largest sized pool.
@since 5.1 *)
| EV_C_MAJOR_HEAP_POOL_FRAG_WORDS
(**
Words in a Domain's major heap pools lost to fragmentation. This is due to
there not being a pool with the exact size of an allocation and a larger sized
pool needing to be used.
@since 5.1 *)
| EV_C_MAJOR_HEAP_POOL_LIVE_BLOCKS
(**
Live blocks of a Domain's major heap pools.
@since 5.1 *)
| EV_C_MAJOR_HEAP_LARGE_BLOCKS
(**
Live blocks of a Domain's major heap large allocations.
@since 5.1 *)
| EV_C_MAJOR_HEAP_WORDS
(**
Major heap size in words of a Domain.
@since 5.3 *)
| EV_C_MAJOR_ALLOCATED_WORDS
(**
Allocations to the major heap of this Domain in words, since the last major
slice.
@since 5.3
*)
| EV_C_MAJOR_ALLOCATED_WORK
(**
The amount of major GC 'work' needing to be done as a result of allocations to
the major heap of this Domain in words, since the last major slice.
@since 5.3
*)
| EV_C_MAJOR_DEPENDENT_WORK
(**
The amount of major GC 'work' needing to be done as a result of dependent
allocations to the major heap of this Domain in words, since the last major
slice. Dependent memory is non-heap memory that depends on heap memory being
collected in order to be freed.
@since 5.3
*)
| EV_C_MAJOR_EXTRA_WORK
(**
The amount of major GC 'work' needing to be done as a result of extra
non-memory resources that are dependent on heap memory being collected in order
to be freed.
@since 5.3
*)
| EV_C_MAJOR_WORK_COUNTER
(**
The global amount of major GC 'work' done by all domains since the program
began.
@since 5.3
*)
| EV_C_MAJOR_ALLOC_COUNTER
(**
The global words of major GC allocations done by all domains since the program
began.
@since 5.3
*)
| EV_C_MAJOR_SLICE_TARGET
(**
The target amount of global 'work' that should be done by all domains at the
end of the major slice (see EV_C_MAJOR_SLICE_COUNTER).
@since 5.3
*)
| EV_C_MAJOR_SLICE_BUDGET
(**
The budget in 'work' that a domain has to do during the major slice.
@since 5.3
*)

(** The type for span events emitted by the runtime. *)
type runtime_phase =
| EV_EXPLICIT_GC_SET
(**
Event spanning a call to Gc.set.
@since 5.0
*)
| EV_EXPLICIT_GC_STAT
(**
Event spanning a call to Gc.stat.
@since 5.0
*)
| EV_EXPLICIT_GC_MINOR
(**
Event spanning a call to Gc.minor, which forces a minor collection.
@since 5.0
*)
| EV_EXPLICIT_GC_MAJOR
(**
Event spanning a call to Gc.major, which forces a major collection.
@since 5.0
*)
| EV_EXPLICIT_GC_FULL_MAJOR
(**
Event spanning a call to Gc.full_major, which forces a full major collection.
@since 5.0
*)
| EV_EXPLICIT_GC_COMPACT
(**
Event spanning a call to Gc.compact, which triggers a compaction.
@since 5.0
*)
| EV_MAJOR
(**
Event spanning any major GC work.
@since 5.0
*)
| EV_MAJOR_SWEEP
(**
Event spanning the sweeping work of a major GC.
@since 5.0
*)
| EV_MAJOR_MARK_ROOTS
(**
Event spanning the marking of roots in a major GC.
@since 5.0
*)
| EV_MAJOR_MEMPROF_ROOTS
(**
Event spanning the marking of memprof roots in a major GC.
@since 5.3
*)
| EV_MAJOR_MARK
(**
Event spanning the marking of the heap in a major GC.
@since 5.0
*)
| EV_MINOR
(**
Event spanning any minor GC work.
@since 5.0
*)
| EV_MINOR_LOCAL_ROOTS
(**
Event spanning the scanning and major allocation of local roots during a minor
GC.
@since 5.0
*)
| EV_MINOR_MEMPROF_ROOTS
(**
Event spanning the scanning and promotion of memprof roots in a minor GC.
@since 5.3
*)
| EV_MINOR_MEMPROF_CLEAN
(**
Event spanning cleaning and updating of memprof structures at the end of a
minor GC.
@since 5.3
*)
| EV_MINOR_FINALIZED
(**
Event spanning the running of finalisers for dead custom blocks at the end of a
minor GC.
@since 5.0
*)
| EV_EXPLICIT_GC_MAJOR_SLICE
(**
Event spanning a call to Gc.major_slice.
@since 5.0
*)
| EV_FINALISE_UPDATE_FIRST
(**
Event spanning time spent in the first phase of finalisation at the end of a
major GC cycle.
@since 5.0
*)
| EV_FINALISE_UPDATE_LAST
(**
Event spanning time spent in the last phase of finalisation at the end of a
major GC cycle.
@since 5.0
*)
| EV_INTERRUPT_REMOTE
(**
Event spanning work triggered by an interrupt from another domain. This is
usually a stop-the-world request.
@since 5.0
*)
| EV_MAJOR_EPHE_MARK
(**
Event spanning the marking of ephemeron tables in a major GC.
@since 5.0
*)
| EV_MAJOR_EPHE_SWEEP
(**
Event spanning the sweeping of ephemeron tables in a major GC.
@since 5.0
*)
| EV_MAJOR_FINISH_MARKING
(**
Event spanning work done at the end of marking in a major GC.
@since 5.0
*)
| EV_MAJOR_GC_CYCLE_DOMAINS
(**
Event spanning work done at the end of a major GC cycle. This includes a
minor collection.
@since 5.0
*)
| EV_MAJOR_GC_PHASE_CHANGE
(**
Event spanning the change of phase in the major GC which involves a global
barrier.
@since 5.0
*)
| EV_MAJOR_GC_STW
(**
Event spanning the stop-the-world phase done at the end of a major GC cycle.
@since 5.0
*)
| EV_MAJOR_MARK_OPPORTUNISTIC
(**
Event spanning the work done during opportunistic marking in a major GC.
@since 5.0
*)
| EV_MAJOR_SLICE
(**
Event spanning the work done during a major slice in a major GC.
@since 5.0
*)
| EV_MAJOR_FINISH_CYCLE
(**
Event spanning attempts to drive all domains to the end of a major GC cycle.
@since 5.0
*)
| EV_MINOR_CLEAR
(**
Event spanning the cleaning of the minor heap and supporting structures at the
end of a minor GC.
@since 5.0
*)
| EV_MINOR_FINALIZERS_OLDIFY
(**
Event spanning the promotion of finalisers during a minor GC.
@since 5.0
*)
| EV_MINOR_GLOBAL_ROOTS
(**
Event spanning the scanning and major allocation of global roots during a minor
GC.
@since 5.0
*)
| EV_MINOR_LEAVE_BARRIER
(**
Event spanning the time spent in the barrier at the end of a minor GC, waiting
for all domains to finish their work.
@since 5.0
*)
| EV_STW_API_BARRIER
(**
Event spanning the time spent waiting for all other domains to reach the
stop-the-world entry barrier.
@since 5.0
*)
| EV_STW_HANDLER
(**
Event spanning the time spent in the stop-the-world handler, including time
spent in the stop-the-world callback itself.
@since 5.0
*)
| EV_STW_LEADER
(**
Event spanning the time spent as the leader of a stop-the-world.
@since 5.0
*)
| EV_MAJOR_FINISH_SWEEPING
(**
Event spanning the time spent finishing sweeping when forced to as part of
domain termination.
@since 5.0
*)
| EV_MAJOR_MEMPROF_CLEAN
(**
Event spanning the time spent cleaning memprof structures at the end of a major
GC.
@since 5.3
*)
| EV_MINOR_FINALIZERS_ADMIN
(**
Event spanning finalisers book-keeping at the end of a minor GC.
@since 5.0
*)
| EV_MINOR_REMEMBERED_SET
(**
Event spanning the scanning and major allocation of remembered sets during a
minor GC.
@since 5.0
*)
| EV_MINOR_REMEMBERED_SET_PROMOTE
(**
Event spanning the promotion of blocks in the remembered set and global roots
during a minor GC.
@since 5.0
*)
| EV_MINOR_LOCAL_ROOTS_PROMOTE
(**
Event spanning the promotion of local roots during a minor GC.
@since 5.0
*)
| EV_DOMAIN_CONDITION_WAIT
(**
Event spanning waiting in Condition.wait.
@since 5.0
*)
| EV_DOMAIN_RESIZE_HEAP_RESERVATION
(**
Event spanning resizing the domain heap reservation, as a result of minor heap
size changes.
@since 5.0
*)
| EV_COMPACT
(**
Event spanning compaction of the heap during a call to Gc.compact.
@since 5.2
*)
| EV_COMPACT_EVACUATE
(**
Event spanning evacuating major GC pools during a compaction.
@since 5.2
*)
| EV_COMPACT_FORWARD
(**
Event spanning the walking of the heap to update changed pointers after an
evacuation during a compaction.
@since 5.2
*)
| EV_COMPACT_RELEASE
(**
Event spanning releasing the evacuated pools at the end of a compaction.
@since 5.2
*)
| EV_EMPTY_MINOR
(**
Event spanning a domain needing to empty its minor heap for a new allocation.
This includes time spent trying to become stop-the-world leader.
@since 5.4
*)

(** Lifecycle events for Runtime_events and domains. *)
type lifecycle =
  EV_RING_START
(**
Event indicating that the Runtime_events ring buffer has been started. Includes
the PID of the process as an argument.
@since 5.0
*)
| EV_RING_STOP
(**
Event indicating that the Runtime_events ring buffer has been stopped.
@since 5.0
*)
| EV_RING_PAUSE
(**
Event indicating that the Runtime_events ring buffer has been paused.
@since 5.0
*)
| EV_RING_RESUME
(**
Event indicating that the Runtime_events ring buffer has been resumed.
@since 5.0
*)
| EV_FORK_PARENT
(**
Event indicating that a fork has occurred and the current domain is the parent.
Includes the PID of the child as an argument.
@since 5.0
*)
| EV_FORK_CHILD
(**
Event indicating that a fork has occurred and the current domain is the child.
@since 5.0
*)
| EV_DOMAIN_SPAWN
(**
Event indicating that a new domain has been spawned. Includes the PID of the
new domain as an argument.
@since 5.0
*)
| EV_DOMAIN_TERMINATE
(**
Event indicating that a domain has terminated. Includes the PID of the domain
as an argument.
@since 5.0
*)

val lifecycle_name : lifecycle -> string
(** Return a string representation of a given lifecycle event type. *)

val runtime_phase_name : runtime_phase -> string
(** Return a string representation of a given runtime phase event type. *)

val runtime_counter_name : runtime_counter -> string
(** Return a string representation of a given runtime counter type. *)

type cursor
(** Type of the cursor used when consuming. *)

module Timestamp : sig
    type t
    (** Type for the int64 timestamp to allow for future changes. *)

    val to_int64 : t -> int64
end

module Type : sig
  type 'a t
  (** The type for a user event content type. *)

  val unit : unit t
  (** An event that has no data associated with it. *)

  type span = Begin | End

  val span : span t
  (** An event that has a beginning and an end. *)

  val int : int t
  (** An event containing an integer value. *)

  val register : encode:(bytes -> 'a -> int) -> decode:(bytes -> int -> 'a)
                                                                        -> 'a t
  (** Registers a custom type by providing an encoder and a decoder. The encoder
      writes the value in the provided buffer and returns the number of bytes
      written. The decoder gets a slice of the buffer of specified length, and
      returns the decoded value.

      The maximum value length is 1024 bytes. *)
end

module User : sig
  (** User events is a way for libraries to provide runtime events that can be
      consumed by other tools. These events can carry known data types or custom
      values. The current maximum number of user events is 8192. *)

  type tag = ..
  (** The type for a user event tag. Tags are used to discriminate between
      user events of the same type. *)

  type 'value t
  (** The type for a user event. User events describe their tag, carried data
      type and an unique string-based name. *)

  val register : string -> tag -> 'value Type.t -> 'value t
  (** [register name tag ty] registers a new event with an unique [name],
      carrying a [tag] and values of type [ty]. *)

  val write : 'value t -> 'value -> unit
  (** [write t v] emits value [v] for event [t]. *)

  val name : _ t -> string
  (** [name t] is the unique identifying name of event [t]. *)

  val tag : 'a t -> tag
  (** [tag t] is the associated tag of event [t], when it is known.
      An event can be unknown if it was not registered in the consumer
      program. *)

end

module Callbacks : sig
  type t
  (** Type of callbacks. *)

  val create : ?runtime_begin:(int -> Timestamp.t -> runtime_phase
                                -> unit) ->
             ?runtime_end:(int -> Timestamp.t -> runtime_phase
                                -> unit) ->
             ?runtime_counter:(int -> Timestamp.t -> runtime_counter
                                -> int -> unit) ->
             ?alloc:(int -> Timestamp.t -> int array -> unit) ->
             ?lifecycle:(int -> Timestamp.t -> lifecycle
                            -> int option -> unit) ->
             ?lost_events:(int -> int -> unit) -> unit -> t
  (** Create a [Callback] that optionally subscribes to one or more runtime
      events. The first int supplied to callbacks is the ring buffer index.
      Each domain owns a single ring buffer for the duration of the domain's
      existence. After a domain terminates, a newly spawned domain may take
      ownership of the ring buffer. A [runtime_begin] callback is called when
      the runtime enters a new phase (e.g a runtime_begin with EV_MINOR is
      called at the start of a minor GC). A [runtime_end] callback is called
      when the runtime leaves a certain phase. The [runtime_counter] callback
      is called when a counter is emitted by the runtime. [lifecycle] callbacks
      are called when the ring undergoes a change in lifecycle and a consumer
      may need to respond. [alloc] callbacks are currently only called on the
      instrumented runtime. [lost_events] callbacks are called if the consumer
      code detects some unconsumed events have been overwritten.
      *)

  val add_user_event : 'a Type.t ->
                        (int -> Timestamp.t -> 'a User.t -> 'a -> unit) ->
                        t -> t
  (** [add_user_event ty callback t] extends [t] to additionally subscribe to
      user events of type [ty]. When such an event happens, [callback] is called
      with the corresponding event and payload. *)
end

val start : unit -> unit
(** [start ()] will start the collection of events in the runtime if not already
  started.

  Events can be consumed by creating a cursor with [create_cursor] and providing
  a set of callbacks to be called for each type of event.
*)

val path : unit -> string option
(** If runtime events are being collected, [path ()] returns [Some p] where [p]
  is a path to the runtime events file. Otherwise, it returns None. *)

val pause : unit -> unit
(** [pause ()] will pause the collection of events in the runtime.
   Traces are collected if the program has called [Runtime_events.start ()] or
   the OCAML_RUNTIME_EVENTS_START environment variable has been set.
*)

val resume : unit -> unit
(** [resume ()] will resume the collection of events in the runtime.
   Traces are collected if the program has called [Runtime_events.start ()] or
   the OCAML_RUNTIME_EVENTS_START environment variable has been set.
*)

val create_cursor : (string * int) option -> cursor
(** [create_cursor path_pid] creates a cursor to read from an runtime_events.
   Cursors can be created for runtime_events in and out of process. A
   runtime_events ring-buffer may have multiple cursors reading from it at any
   point in time and a program may have multiple cursors open concurrently
  (for example if multiple consumers want different sets of events). If
   [path_pid] is None then a cursor is created for the current process.
   Otherwise the pair contains a string [path] to the directory that contains
   the [pid].events file and int [pid] for the runtime_events of an
   external process to monitor. *)

val free_cursor : cursor -> unit
(** Free a previously created runtime_events cursor. *)

val read_poll : cursor -> Callbacks.t -> int option -> int
(** [read_poll cursor callbacks max_option] calls the corresponding functions
    on [callbacks] for up to [max_option] events read off [cursor]'s
    runtime_events and returns the number of events read. *)
