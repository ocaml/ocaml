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
| EV_MAJOR_FINISH_CYCLE
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

type ev_lifecycle =
  EV_RING_START
| EV_RING_STOP
| EV_RING_PAUSE
| EV_RING_RESUME
| EV_FORK_PARENT
| EV_FORK_CHILD
| EV_DOMAIN_SPAWN
| EV_DOMAIN_TERMINATE

type cursor

module Timestamp = struct
  type t = int64

  let to_int64 t =
    t
end

module Callbacks = struct
    type[@warning "-69"] t = {
      runtime_begin: (Domain.id -> Timestamp.t -> runtime_phase -> unit) option;
      runtime_end: (Domain.id -> Timestamp.t -> runtime_phase -> unit) option;
      runtime_counter: (Domain.id -> Timestamp.t -> runtime_counter
                        -> int -> unit) option;
      alloc: (Domain.id -> Timestamp.t -> int array -> unit) option;
      lifecycle: (Domain.id -> Timestamp.t -> ev_lifecycle
                  -> int option -> unit) option;
      lost_events: (Domain.id -> int -> unit) option
    }

    let create ?runtime_begin ?runtime_end ?runtime_counter ?alloc ?lifecycle
               ?lost_events () =
      { runtime_begin; runtime_end; runtime_counter;
          alloc; lifecycle; lost_events}
  end

external start : unit -> unit = "caml_eventring_start"
external pause : unit -> unit = "caml_eventring_pause"
external resume : unit -> unit = "caml_eventring_resume"

external create_cursor : (string * int) option -> cursor
                                        = "caml_ml_eventring_create_cursor"
external free_cursor : cursor -> unit = "caml_ml_eventring_free_cursor"
external read_poll : cursor -> Callbacks.t -> int option -> int
                                        = "caml_ml_eventring_read_poll"
