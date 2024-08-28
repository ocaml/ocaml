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

external runtime_events_are_active : unit -> bool
  = "caml_ml_runtime_events_are_active" [@@noalloc]

type runtime_counter =
| EV_C_FORCE_MINOR_ALLOC_SMALL
| EV_C_FORCE_MINOR_MAKE_VECT
| EV_C_FORCE_MINOR_SET_MINOR_HEAP_SIZE
| EV_C_FORCE_MINOR_MEMPROF
| EV_C_MINOR_PROMOTED
| EV_C_MINOR_ALLOCATED
| EV_C_REQUEST_MAJOR_ALLOC_SHR
| EV_C_REQUEST_MAJOR_ADJUST_GC_SPEED
| EV_C_REQUEST_MINOR_REALLOC_REF_TABLE
| EV_C_REQUEST_MINOR_REALLOC_EPHE_REF_TABLE
| EV_C_REQUEST_MINOR_REALLOC_CUSTOM_TABLE
| EV_C_MAJOR_HEAP_POOL_WORDS
| EV_C_MAJOR_HEAP_POOL_LIVE_WORDS
| EV_C_MAJOR_HEAP_LARGE_WORDS
| EV_C_MAJOR_HEAP_POOL_FRAG_WORDS
| EV_C_MAJOR_HEAP_POOL_LIVE_BLOCKS
| EV_C_MAJOR_HEAP_LARGE_BLOCKS
| EV_C_MAJOR_HEAP_WORDS
| EV_C_MAJOR_ALLOCATED_WORDS
| EV_C_MAJOR_ALLOCATED_WORK
| EV_C_MAJOR_DEPENDENT_WORK
| EV_C_MAJOR_EXTRA_WORK
| EV_C_MAJOR_WORK_COUNTER
| EV_C_MAJOR_ALLOC_COUNTER
| EV_C_MAJOR_SLICE_TARGET
| EV_C_MAJOR_SLICE_BUDGET

type runtime_phase =
| EV_EXPLICIT_GC_SET
| EV_EXPLICIT_GC_STAT
| EV_EXPLICIT_GC_MINOR
| EV_EXPLICIT_GC_MAJOR
| EV_EXPLICIT_GC_FULL_MAJOR
| EV_EXPLICIT_GC_COMPACT
| EV_MAJOR
| EV_MAJOR_SWEEP
| EV_MAJOR_MARK_ROOTS
| EV_MAJOR_MEMPROF_ROOTS
| EV_MAJOR_MARK
| EV_MINOR
| EV_MINOR_LOCAL_ROOTS
| EV_MINOR_MEMPROF_ROOTS
| EV_MINOR_MEMPROF_CLEAN
| EV_MINOR_FINALIZED
| EV_EXPLICIT_GC_MAJOR_SLICE
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
| EV_MAJOR_MEMPROF_CLEAN
| EV_MINOR_FINALIZERS_ADMIN
| EV_MINOR_REMEMBERED_SET
| EV_MINOR_REMEMBERED_SET_PROMOTE
| EV_MINOR_LOCAL_ROOTS_PROMOTE
| EV_DOMAIN_CONDITION_WAIT
| EV_DOMAIN_RESIZE_HEAP_RESERVATION
| EV_COMPACT
| EV_COMPACT_EVACUATE
| EV_COMPACT_FORWARD
| EV_COMPACT_RELEASE
| EV_EMPTY_MINOR

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
  | EV_C_FORCE_MINOR_ALLOC_SMALL -> "force_minor_alloc_small"
  | EV_C_FORCE_MINOR_MAKE_VECT -> "force_minor_make_vect"
  | EV_C_FORCE_MINOR_SET_MINOR_HEAP_SIZE -> "force_minor_set_minor_heap_size"
  | EV_C_FORCE_MINOR_MEMPROF -> "force_minor_memprof"
  | EV_C_MINOR_PROMOTED -> "minor_promoted"
  | EV_C_MINOR_ALLOCATED -> "minor_allocated"
  | EV_C_REQUEST_MAJOR_ALLOC_SHR -> "request_major_alloc_shr"
  | EV_C_REQUEST_MAJOR_ADJUST_GC_SPEED -> "request_major_adjust_gc_speed"
  | EV_C_REQUEST_MINOR_REALLOC_REF_TABLE -> "request_minor_realloc_ref_table"
  | EV_C_REQUEST_MINOR_REALLOC_EPHE_REF_TABLE ->
      "request_minor_realloc_ephe_ref_table"
  | EV_C_REQUEST_MINOR_REALLOC_CUSTOM_TABLE ->
      "request_minor_realloc_custom_table"
  | EV_C_MAJOR_HEAP_POOL_WORDS ->
      "major_heap_pool_words"
  | EV_C_MAJOR_HEAP_POOL_LIVE_WORDS ->
      "major_heap_pool_live_words"
  | EV_C_MAJOR_HEAP_LARGE_WORDS ->
      "major_heap_large_words"
  | EV_C_MAJOR_HEAP_POOL_FRAG_WORDS ->
      "major_heap_pool_frag_words"
  | EV_C_MAJOR_HEAP_POOL_LIVE_BLOCKS ->
      "major_heap_pool_live_blocks"
  | EV_C_MAJOR_HEAP_LARGE_BLOCKS ->
      "major_heap_large_blocks"
  | EV_C_MAJOR_HEAP_WORDS ->
      "major_heap_words"
  | EV_C_MAJOR_ALLOCATED_WORDS ->
      "major_allocated_words"
  | EV_C_MAJOR_ALLOCATED_WORK ->
      "major_allocated_work"
  | EV_C_MAJOR_DEPENDENT_WORK ->
      "major_dependent_work"
  | EV_C_MAJOR_EXTRA_WORK ->
      "major_extra_work"
  | EV_C_MAJOR_WORK_COUNTER ->
      "major_work_counter"
  | EV_C_MAJOR_ALLOC_COUNTER ->
      "major_alloc_counter"
  | EV_C_MAJOR_SLICE_TARGET ->
      "major_slice_target"
  | EV_C_MAJOR_SLICE_BUDGET ->
      "major_slice_budget"


let runtime_phase_name phase =
  match phase with
  | EV_EXPLICIT_GC_SET -> "explicit_gc_set"
  | EV_EXPLICIT_GC_STAT -> "explicit_gc_stat"
  | EV_EXPLICIT_GC_MINOR -> "explicit_gc_minor"
  | EV_EXPLICIT_GC_MAJOR -> "explicit_gc_major"
  | EV_EXPLICIT_GC_FULL_MAJOR -> "explicit_gc_full_major"
  | EV_EXPLICIT_GC_COMPACT -> "explicit_gc_compact"
  | EV_MAJOR -> "major"
    | EV_MAJOR_SWEEP -> "major_sweep"
  | EV_MAJOR_MARK_ROOTS -> "major_mark_roots"
  | EV_MAJOR_MEMPROF_ROOTS -> "major_memprof_roots"
  | EV_MAJOR_MARK -> "major_mark"
  | EV_MINOR -> "minor"
  | EV_MINOR_LOCAL_ROOTS -> "minor_local_roots"
  | EV_MINOR_MEMPROF_ROOTS -> "minor_memprof_roots"
  | EV_MINOR_MEMPROF_CLEAN -> "minor_memprof_clean"
  | EV_MINOR_FINALIZED -> "minor_finalized"
  | EV_EXPLICIT_GC_MAJOR_SLICE -> "explicit_gc_major_slice"
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
  | EV_MAJOR_MEMPROF_CLEAN -> "major_memprof_clean"
  | EV_MINOR_FINALIZERS_ADMIN -> "minor_finalizers_admin"
  | EV_MINOR_REMEMBERED_SET -> "minor_remembered_set"
  | EV_MINOR_REMEMBERED_SET_PROMOTE -> "minor_remembered_set_promote"
  | EV_MINOR_LOCAL_ROOTS_PROMOTE -> "minor_local_roots_promote"
  | EV_DOMAIN_CONDITION_WAIT -> "domain_condition_wait"
  | EV_MAJOR_FINISH_CYCLE -> "major_finish_cycle"
  | EV_DOMAIN_RESIZE_HEAP_RESERVATION -> "domain_resize_heap_reservation"
  | EV_COMPACT -> "compaction"
  | EV_COMPACT_EVACUATE -> "compaction_evacuate"
  | EV_COMPACT_FORWARD -> "compaction_forward"
  | EV_COMPACT_RELEASE -> "compaction_release"
  | EV_EMPTY_MINOR -> "empty_minor"

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

module Type = struct

  (* the data structure is primarily managed in C *)
  type[@warning "-unused-field"] 'a custom = {
    serialize: bytes -> 'a -> int;
    deserialize: bytes -> int -> 'a;
    (* id is used for the callback table *)
    id: int;
  }

  type span = Begin | End

  type 'a t =
  | Unit : unit t
  | Int : int t
  | Span : span t
  | Custom : 'a custom -> 'a t

  let unit = Unit

  let span = Span

  let int = Int

  let next_id = Atomic.make 3

  let register ~encode ~decode =
    let id = Atomic.fetch_and_add next_id 1 in
    Custom { serialize = encode; deserialize = decode; id; }

  let id: type a. a t -> int = function
    | Unit -> 0
    | Int -> 1
    | Span -> 2
    | Custom {id; _} -> id

end

module User = struct
  type tag = ..

  (* the UNK tag is used when an unknown event of a known type (unit, int, span)
     is received *)
  type tag += UNK : tag

  (* the data structure is primarily managed in C *)
  type [@warning "-unused-field"] 'a t = {
    id: int;
    name: string;
    typ: 'a Type.t;
    tag: tag option;
  }

  external user_register : string -> tag option -> 'a Type.t -> 'a t
    = "caml_runtime_events_user_register"
  external user_write : bytes -> 'a t -> 'a -> unit
    = "caml_runtime_events_user_write"

  let register name tag typ = user_register name (Some tag) typ

  let with_write_buffer =
    (* [caml_runtime_events_user_write] needs a write buffer in which
       the user-provided serializer will write its data. The user may
       want to write a lot of custom events fast, so we want to cache
       the write buffer across calls.

       To be safe for multi-domain programs, we use domain-local
       storage for the write buffer. To accommodate for multi-threaded
       programs (without depending on the Thread module), we store
       a list of caches for each domain. This might leak a bit of
       memory: the number of buffers for a domain is equal to the
       maximum number of threads that requested a buffer concurrently,
       and we never free those buffers. *)
    let create_buffer () = Bytes.create 1024 in
    let write_buffer_cache = Domain.DLS.new_key (fun () -> ref []) in
    let pop_or_create buffers =
      (* intended to be thread-safe *)
      (* begin atomic *)
      match !buffers with
      | [] ->
          (* end atomic *)
          create_buffer ()
      | b::bs ->
          buffers := bs;
          (* end atomic *)
          b
    in
    let[@poll error] compare_and_set r old_val new_val =
      if !r == old_val then (r := new_val; true)
      else false
    in
    let rec push buffers buf =
      (* intended to be thread-safe *)
      let old_buffers = !buffers in
      let new_buffers = buf :: old_buffers in
      (* retry if !buffers changed under our feet: *)
      if compare_and_set buffers old_buffers new_buffers
      then ()
      else push buffers buf
    in
    fun consumer ->
      let buffers = Domain.DLS.get write_buffer_cache in
      let buf = pop_or_create buffers in
      Fun.protect ~finally:(fun () -> push buffers buf)
        (fun () -> consumer buf)

  let write (type a) (event : a t) (value : a) =
    if runtime_events_are_active () then
    (* only custom events need a write buffer *)
    match event.typ with
    | Type.Custom _ ->
        with_write_buffer (fun buf -> user_write buf event value)
    | Type.Unit | Type.Int | Type.Span ->
        user_write Bytes.empty event value

  let name ev = ev.name

  let tag ev = Option.value ~default:UNK ev.tag
end

module Callbacks = struct

  type 'a callback = int -> Timestamp.t -> 'a User.t -> 'a -> unit
  (* Callbacks are bound to a specific event type *)
  type any_callback = U : 'a callback -> any_callback

  (* these record callbacks are only called from C code in the runtime
      so we suppress the unused field warning *)
  type[@warning "-unused-field"] t = {
    runtime_begin: (int -> Timestamp.t -> runtime_phase -> unit) option;
    runtime_end: (int -> Timestamp.t -> runtime_phase -> unit) option;
    runtime_counter: (int -> Timestamp.t -> runtime_counter
                      -> int -> unit) option;
    alloc: (int -> Timestamp.t -> int array -> unit) option;
    lifecycle: (int -> Timestamp.t -> lifecycle
                -> int option -> unit) option;
    lost_events: (int -> int -> unit) option;
    (* user event callbacks is an array containing at each indice [i] a list
        of functions to call when an event of type id [i] happen *)
    user_events: any_callback list array;
  }

  let create ?runtime_begin ?runtime_end ?runtime_counter ?alloc ?lifecycle
             ?lost_events () =
    { runtime_begin; runtime_end; runtime_counter;
        alloc; lifecycle; lost_events; user_events = Array.make 1 [] }


  (* returns an array that is sufficiently large to contain a value of given
     index *)
  let fit_or_grow array index =
    let size = Array.length array in
    if index < size then
      (* array is large enough *)
      array
    else
      (* array is too small. we resize it by finding the power of two that is
         big enough to contain the index *)
      let rec find_new_size sz =
        if index < sz then
          sz
        else
          find_new_size (2 * sz)
      in
      let new_size = find_new_size size in
      let new_array = Array.make new_size [] in
      Array.blit array 0 new_array 0 size;
      new_array

  let add_user_event ty callback t =
    let id = Type.id ty in
    let user_events = fit_or_grow t.user_events id in
    user_events.(id) <- U callback :: user_events.(id);
    {t with user_events}

end

external start : unit -> unit = "caml_ml_runtime_events_start"
external pause : unit -> unit = "caml_ml_runtime_events_pause"
external resume : unit -> unit = "caml_ml_runtime_events_resume"
external path : unit -> string option = "caml_ml_runtime_events_path"

external create_cursor : (string * int) option -> cursor
                                        = "caml_ml_runtime_events_create_cursor"
external free_cursor : cursor -> unit = "caml_ml_runtime_events_free_cursor"
external read_poll : cursor -> Callbacks.t -> int option -> int
                                        = "caml_ml_runtime_events_read_poll"
