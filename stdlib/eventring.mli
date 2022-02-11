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

(** Eventring - ring buffer-based runtime tracing

    This module enables users to enable and subscribe to tracing events
    from the Garbage Collector and othe parts of the OCaml runtime. This can
    be useful for diagnostic or performance monitoring purposes. This module
    can be used to subscribe to events for the current process or external
    processes asynchronously.

    When enabled (either via setting the OCAML_EVENTRING_START environment
    variable or calling Eventring.start) a file with the pid of the process and
    extension .eventring will be created. By default this is in the current
    directory but can be over-ridden by the OCAML_EVENTRING_DIR environent
    variable. Each domain maintains its own ring buffer in a section of the
    larger file into which it emits events.

    There is additionally a set of C APIs in eventring.h that can enable
    zero-impact monitoring of the current process or bindings for other
    languages.
*)

type runtime_counter =
  EV_C_ALLOC_JUMP
| EV_C_FORCE_MINOR_ALLOC_SMALL
| EV_C_FORCE_MINOR_MAKE_VECT
| EV_C_FORCE_MINOR_SET_MINOR_HEAP_SIZE
| EV_C_FORCE_MINOR_WEAK
| EV_C_FORCE_MINOR_MEMPROF
| EV_C_MAJOR_MARK_SLICE_REMAIN
| EV_C_MAJOR_MARK_SLICE_FIELDS
| EV_C_MAJOR_MARK_SLICE_POINTERS
| EV_C_MAJOR_WORK_EXTRA
| EV_C_MAJOR_WORK_MARK
| EV_C_MAJOR_WORK_SWEEP
| EV_C_MINOR_PROMOTED
| EV_C_REQUEST_MAJOR_ALLOC_SHR
| EV_C_REQUEST_MAJOR_ADJUST_GC_SPEED
| EV_C_REQUEST_MINOR_REALLOC_REF_TABLE
| EV_C_REQUEST_MINOR_REALLOC_EPHE_REF_TABLE
| EV_C_REQUEST_MINOR_REALLOC_CUSTOM_TABLE
(** The type for counter events emitted by the runtime *)

type runtime_phase =
  EV_COMPACT_MAIN
| EV_COMPACT_RECOMPACT
| EV_EXPLICIT_GC_SET
| EV_EXPLICIT_GC_STAT
| EV_EXPLICIT_GC_MINOR
| EV_EXPLICIT_GC_MAJOR
| EV_EXPLICIT_GC_FULL_MAJOR
| EV_EXPLICIT_GC_COMPACT
| EV_MAJOR
| EV_MAJOR_ROOTS
| EV_MAJOR_SWEEP
| EV_MAJOR_MARK_ROOTS
| EV_MAJOR_MARK_MAIN
| EV_MAJOR_MARK_FINAL
| EV_MAJOR_MARK
| EV_MAJOR_MARK_GLOBAL_ROOTS_SLICE
| EV_MAJOR_ROOTS_GLOBAL
| EV_MAJOR_ROOTS_DYNAMIC_GLOBAL
| EV_MAJOR_ROOTS_LOCAL
| EV_MAJOR_ROOTS_C
| EV_MAJOR_ROOTS_FINALISED
| EV_MAJOR_ROOTS_MEMPROF
| EV_MAJOR_ROOTS_HOOK
| EV_MAJOR_CHECK_AND_COMPACT
| EV_MINOR
| EV_MINOR_LOCAL_ROOTS
| EV_MINOR_REF_TABLES
| EV_MINOR_COPY
| EV_MINOR_UPDATE_WEAK
| EV_MINOR_FINALIZED
| EV_EXPLICIT_GC_MAJOR_SLICE
| EV_DOMAIN_SEND_INTERRUPT
| EV_DOMAIN_IDLE_WAIT
| EV_FINALISE_UPDATE_FIRST
| EV_FINALISE_UPDATE_LAST
| EV_INTERRUPT_REMOTE
| EV_MAJOR_EPHE_MARK
| EV_MAJOR_EPHE_SWEEP
| EV_MAJOR_FINISH_MARKING
| EV_MAJOR_GC_CYCLE_DOMAINS
| EV_MAJOR_GC_PHASE_CHANGE
| EV_MAJOR_GC_STW
| EV_MAJOR_MARK_OPPORTUNISTIC
| EV_MAJOR_SLICE
| EV_MINOR_CLEAR
| EV_MINOR_FINALIZERS_OLDIFY
| EV_MINOR_GLOBAL_ROOTS
| EV_MINOR_LEAVE_BARRIER
| EV_STW_API_BARRIER
| EV_STW_HANDLER
| EV_STW_LEADER
| EV_MAJOR_FINISH_SWEEPING
| EV_MINOR_FINALIZERS_ADMIN
| EV_MINOR_REMEMBERED_SET
| EV_MINOR_REMEMBERED_SET_PROMOTE
| EV_MINOR_LOCAL_ROOTS_PROMOTE
| EV_DOMAIN_CONDITION_WAIT
| EV_DOMAIN_RESIZE_HEAP_RESERVATION
(** The type for span events emitted by the runtime *)

type ev_lifecycle =
  EV_RING_START
| EV_RING_STOP
| EV_RING_PAUSE
| EV_RING_RESUME
| EV_FORK_PARENT
| EV_FORK_CHILD
| EV_DOMAIN_SPAWN
| EV_DOMAIN_TERMINATE
(** Lifecycle events for the ring itself *)

type cursor
(** Type of the cursor used when consuming *)

module Timestamp : sig
    type t
    (** Type for the int64 timestamp to allow for future changes *)

    val to_int64 : t -> int64
end

module Callbacks : sig
  type t
  (** Type of callbacks *)

  val create : ?runtime_begin:(Domain.id -> Timestamp.t -> runtime_phase
                                -> unit) ->
             ?runtime_end:(Domain.id -> Timestamp.t -> runtime_phase
                                -> unit) ->
             ?runtime_counter:(Domain.id -> Timestamp.t -> runtime_counter
                                -> int -> unit) ->
             ?alloc:(Domain.id -> Timestamp.t -> int array -> unit) ->
             ?lifecycle:(Domain.id -> Timestamp.t -> ev_lifecycle
                            -> int option -> unit) ->
             ?lost_events:(Domain.id -> int -> unit) -> unit -> t
  (** Create a [Callback] that optionally subscribes to one or more runtime
      events. A [runtime_begin] callback is called when the runtime enters a new
      phase (e.g a runtime_begin with EV_MINOR is called at the start of a minor
      GC). A [runtime_end] callback is called when the runtime leaves a certain
      phase. The [runtime_counter] callback is called when a counter is emitted
      by the runtime. [lifecycle] callbacks are called when the ring undergoes
      a change in lifecycle and a consumer may need to respond. [alloc]
      callbacks are currently only called on the instrumented runtime.
      [lost_events] callbacks are called if the consumer code detects some
      unconsumed events have been overwritten.
      *)
end

val start : unit -> unit
(** [start ()] will start the collection of events in the runtime if not already
  started.

  Events can be consumed by creating a cursor with [create_cursor] and providing
  a set of callbacks to be called for each type of event.
*)

val pause : unit -> unit
(** [pause ()] will pause the collection of events in the runtime.
   Traces are collected if the program has called [Eventring.start ()] or
   the OCAML_EVENTRING_START environment variable has been set.
*)

val resume : unit -> unit
(** [resume ()] will resume the collection of events in the runtime.
   Traces are collected if the program has called [Eventring.start ()] or
   the OCAML_EVENTRING_START environment variable has been set.
*)

val create_cursor : (string * int) option -> cursor
(** [create_cursor path_pid] creates a cursor to read from an eventring. If
    [path_pid] is None then a cursor is created for the current process.
    Otherwise the pair contains a string [path] and int [pid] for the
    eventring of an external process to monitor. *)

val free_cursor : cursor -> unit
(** Free a previously created eventring cursor *)

val read_poll : cursor -> Callbacks.t -> int option -> int
(** [read_poll cursor callbacks max_option] calls the corresponding functions
    on [callbacks] for up to [max_option] events read off [cursor]'s eventring
    and returns the number of events read. *)
