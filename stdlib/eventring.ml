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

type lifecycle =
  EV_RING_START
| EV_RING_STOP
| EV_RING_PAUSE
| EV_RING_RESUME
| EV_FORK_PARENT
| EV_FORK_CHILD
| EV_DOMAIN_SPAWN
| EV_DOMAIN_TERMINATE

let runtime_counter_name counter =
  match counter with
    EV_C_ALLOC_JUMP -> "alloc_jump"
  | EV_C_FORCE_MINOR_ALLOC_SMALL -> "force_minor_alloc_small"
  | EV_C_FORCE_MINOR_MAKE_VECT -> "force_minor_make_vect"
  | EV_C_FORCE_MINOR_SET_MINOR_HEAP_SIZE -> "force_minor_set_minor_heap_size"
  | EV_C_FORCE_MINOR_WEAK -> "force_minor_weak"
  | EV_C_FORCE_MINOR_MEMPROF -> "force_minor_memprof"
  | EV_C_MAJOR_MARK_SLICE_REMAIN -> "major_mark_slice_remain"
  | EV_C_MAJOR_MARK_SLICE_FIELDS -> "major_mark_slice_fields"
  | EV_C_MAJOR_MARK_SLICE_POINTERS -> "major_mark_slice_pointers"
  | EV_C_MAJOR_WORK_EXTRA -> "major_work_extra"
  | EV_C_MAJOR_WORK_MARK -> "major_work_mark"
  | EV_C_MAJOR_WORK_SWEEP -> "major_work_sweep"
  | EV_C_MINOR_PROMOTED -> "minor_promoted"
  | EV_C_REQUEST_MAJOR_ALLOC_SHR -> "request_major_alloc_shr"
  | EV_C_REQUEST_MAJOR_ADJUST_GC_SPEED -> "request_major_adjust_gc_speed"
  | EV_C_REQUEST_MINOR_REALLOC_REF_TABLE -> "request_minor_realloc_ref_table"
  | EV_C_REQUEST_MINOR_REALLOC_EPHE_REF_TABLE -> "request_minor_realloc_ephe_ref_table"
  | EV_C_REQUEST_MINOR_REALLOC_CUSTOM_TABLE -> "request_minor_realloc_custom_table"

let runtime_phase_name phase =
  match phase with
    EV_COMPACT_MAIN -> "compact_main"
  | EV_COMPACT_RECOMPACT -> "compact_recompact"
  | EV_EXPLICIT_GC_SET -> "explicit_gc_set"
  | EV_EXPLICIT_GC_STAT -> "explicit_gc_stat"
  | EV_EXPLICIT_GC_MINOR -> "explicit_gc_minor"
  | EV_EXPLICIT_GC_MAJOR -> "explicit_gc_major"
  | EV_EXPLICIT_GC_FULL_MAJOR -> "explicit_gc_full_major"
  | EV_EXPLICIT_GC_COMPACT -> "explicit_gc_compact"
  | EV_MAJOR -> "major"
  | EV_MAJOR_ROOTS -> "major_roots"
  | EV_MAJOR_SWEEP -> "major_sweep"
  | EV_MAJOR_MARK_ROOTS -> "major_mark_roots"
  | EV_MAJOR_MARK_MAIN -> "major_mark_main"
  | EV_MAJOR_MARK_FINAL -> "major_mark_final"
  | EV_MAJOR_MARK -> "major_mark"
  | EV_MAJOR_MARK_GLOBAL_ROOTS_SLICE -> "major_mark_global_roots_slice"
  | EV_MAJOR_ROOTS_GLOBAL -> "major_roots_global"
  | EV_MAJOR_ROOTS_DYNAMIC_GLOBAL -> "major_roots_dynamic_global"
  | EV_MAJOR_ROOTS_LOCAL -> "major_roots_local"
  | EV_MAJOR_ROOTS_C -> "major_roots_c"
  | EV_MAJOR_ROOTS_FINALISED -> "major_roots_finalised"
  | EV_MAJOR_ROOTS_MEMPROF -> "major_roots_memprof"
  | EV_MAJOR_ROOTS_HOOK -> "major_roots_hook"
  | EV_MAJOR_CHECK_AND_COMPACT -> "major_check_and_compact"
  | EV_MINOR -> "minor"
  | EV_MINOR_LOCAL_ROOTS -> "minor_local_roots"
  | EV_MINOR_REF_TABLES -> "minor_ref_tables"
  | EV_MINOR_COPY -> "minor_copy"
  | EV_MINOR_UPDATE_WEAK -> "minor_update_weak"
  | EV_MINOR_FINALIZED -> "minor_finalized"
  | EV_EXPLICIT_GC_MAJOR_SLICE -> "explicit_gc_major_slice"
  | EV_DOMAIN_SEND_INTERRUPT -> "domain_send_interrupt"
  | EV_DOMAIN_IDLE_WAIT -> "domain_idle_wait"
  | EV_FINALISE_UPDATE_FIRST -> "finalise_update_first"
  | EV_FINALISE_UPDATE_LAST -> "finalise_update_last"
  | EV_INTERRUPT_REMOTE -> "interrupt_remote"
  | EV_MAJOR_EPHE_MARK -> "major_ephe_mark"
  | EV_MAJOR_EPHE_SWEEP -> "major_ephe_sweep"
  | EV_MAJOR_FINISH_MARKING -> "major_finish_marking"
  | EV_MAJOR_GC_CYCLE_DOMAINS -> "major_gc_cycle_domains"
  | EV_MAJOR_GC_PHASE_CHANGE -> "major_gc_phase_change"
  | EV_MAJOR_GC_STW -> "major_gc_stw"
  | EV_MAJOR_MARK_OPPORTUNISTIC -> "major_mark_opportunistic"
  | EV_MAJOR_SLICE -> "major_slice"
  | EV_MINOR_CLEAR -> "minor_clear"
  | EV_MINOR_FINALIZERS_OLDIFY -> "minor_finalizers_oldify"
  | EV_MINOR_GLOBAL_ROOTS -> "minor_global_roots"
  | EV_MINOR_LEAVE_BARRIER -> "minor_leave_barrier"
  | EV_STW_API_BARRIER -> "stw_api_barrier"
  | EV_STW_HANDLER -> "stw_handler"
  | EV_STW_LEADER -> "stw_leader"
  | EV_MAJOR_FINISH_SWEEPING -> "major_finish_sweeping"
  | EV_MINOR_FINALIZERS_ADMIN -> "minor_finalizers_admin"
  | EV_MINOR_REMEMBERED_SET -> "minor_remembered_set"
  | EV_MINOR_REMEMBERED_SET_PROMOTE -> "minor_remembered_set_promote"
  | EV_MINOR_LOCAL_ROOTS_PROMOTE -> "minor_local_roots_promote"
  | EV_DOMAIN_CONDITION_WAIT -> "domain_condition_wait"
  | EV_MAJOR_FINISH_CYCLE -> "major_finish_cycle"
  | EV_DOMAIN_RESIZE_HEAP_RESERVATION -> "domain_resize_heap_reservation"

let lifecycle_name lifecycle =
  match lifecycle with
    EV_RING_START -> "ring_start"
  | EV_RING_STOP -> "ring_stop"
  | EV_RING_PAUSE -> "ring_pause"
  | EV_RING_RESUME -> "ring_resume"
  | EV_FORK_PARENT -> "fork_parent"
  | EV_FORK_CHILD -> "fork_child"
  | EV_DOMAIN_SPAWN -> "domain_spawn"
  | EV_DOMAIN_TERMINATE -> "domain_terminate"

type cursor

module Timestamp = struct
  type t = int64

  let to_int64 t =
    t
end

module Callbacks = struct
    (* these record callbacks are only called from C code in the runtime
       so we suppress the unused field warning *)
    type[@warning "-unused-field"] t = {
      runtime_begin: (Domain.id -> Timestamp.t -> runtime_phase -> unit) option;
      runtime_end: (Domain.id -> Timestamp.t -> runtime_phase -> unit) option;
      runtime_counter: (Domain.id -> Timestamp.t -> runtime_counter
                        -> int -> unit) option;
      alloc: (Domain.id -> Timestamp.t -> int array -> unit) option;
      lifecycle: (Domain.id -> Timestamp.t -> lifecycle
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
