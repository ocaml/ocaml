(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt             *)
(*            Jacques-Henri Jourdan, projet Gallium, INRIA Paris          *)
(*                                                                        *)
(*   Copyright 1996-2016 Institut National de Recherche en Informatique   *)
(*     et en Automatique.                                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Memory management control and statistics; finalised values. *)

type stat =
  { minor_words : float;
    (** Number of words allocated in the minor heap since
       the program was started. *)

    promoted_words : float;
    (** Number of words allocated in the minor heap that
       survived a minor collection and were moved to the major heap
       since the program was started. *)

    major_words : float;
    (** Number of words allocated in the major heap, including
       the promoted words, since the program was started. *)

    minor_collections : int;
    (** Number of minor collections since the program was started. *)

    major_collections : int;
    (** Number of major collection cycles completed since the program
        was started. *)

    heap_words : int;
    (** Total size of the major heap, in words. *)

    heap_chunks : int;
    (** Number of contiguous pieces of memory that make up the major heap.
        This metric is currently not available in OCaml 5: the field value is
        always [0]. *)

    live_words : int;
    (** Number of words of live data in the major heap, including the header
       words.

       Note that "live" words refers to every word in the major heap that isn't
       currently known to be collectable, which includes words that have become
       unreachable by the program after the start of the previous gc cycle.
       It is typically much simpler and more predictable to call
       {!Gc.full_major} (or {!Gc.compact}) then computing gc stats, as then
       "live" words has the simple meaning of "reachable by the program". One
       caveat is that a single call to {!Gc.full_major} will not reclaim values
       that have a finaliser from {!Gc.finalise} (this does not apply to
       {!Gc.finalise_last}). If this caveat matters, simply call
       {!Gc.full_major} twice instead of once.
     *)

    live_blocks : int;
    (** Number of live blocks in the major heap.

        See [live_words] for a caveat about what "live" means. *)

    free_words : int;
    (** Number of words in the free list. *)

    free_blocks : int;
    (** Number of blocks in the free list.
        This metric is currently not available in OCaml 5: the field value is
        always [0]. *)

    largest_free : int;
    (** Size (in words) of the largest block in the free list.
        This metric is currently not available in OCaml 5: the field value
        is always [0]. *)

    fragments : int;
    (** Number of wasted words due to fragmentation.  These are
       1-words free blocks placed between two live blocks.  They
       are not available for allocation. *)

    compactions : int;
    (** Number of heap compactions since the program was started. *)

    top_heap_words : int;
    (** Maximum size reached by the major heap, in words. *)

    stack_size: int;
    (** Current size of the stack, in words.
        This metric is currently not available in OCaml 5: the field value is
        always [0].
        @since 3.12 *)

    forced_major_collections: int;
    (** Number of forced full major collections completed since the program
        was started.
        @since 4.12 *)
}
(** The memory management counters are returned in a [stat] record. These
   counters give values for the whole program.

   The total amount of memory allocated by the program since it was started
   is (in words) [minor_words + major_words - promoted_words].  Multiply by
   the word size (4 on a 32-bit machine, 8 on a 64-bit machine) to get
   the number of bytes.
*)

type control =
  { minor_heap_size : int;
    (** The size (in words) of the minor heap.  Changing
       this parameter will trigger a minor collection. The total size of the
       minor heap used by this program is the sum of the heap sizes of the
       active domains. Default: 256k. *)

    major_heap_increment : int;
    (** How much to add to the major heap when increasing it. If this
        number is less than or equal to 1000, it is a percentage of
        the current heap size (i.e. setting it to 100 will double the heap
        size at each increase). If it is more than 1000, it is a fixed
        number of words that will be added to the heap.

        This field is currently not available in OCaml 5: the field value is
        always [0]. *)

    space_overhead : int;
    (** The major GC speed is computed from this parameter.
       This is the memory that will be "wasted" because the GC does not
       immediately collect unreachable blocks.  It is expressed as a
       percentage of the memory used for live data.
       The GC will work more (use more CPU time and collect
       blocks more eagerly) if [space_overhead] is smaller.
       Default: 120. *)

    verbose : int;
    (** This value controls the GC messages on standard error output.
       It is a sum of some of the following flags, to print messages
       on the corresponding events:
       - [0x0001] Start and end of major GC cycle.
       - [0x0002] Minor collection and major GC slice.
       - [0x0004] Growing and shrinking of the heap.
       - [0x0008] Resizing of stacks and memory manager tables.
       - [0x0010] Heap compaction.
       - [0x0020] Change of GC parameters.
       - [0x0040] Computation of major GC slice size.
       - [0x0080] Calling of finalisation functions.
       - [0x0100] Bytecode executable and shared library search at start-up.
       - [0x0200] Computation of compaction-triggering condition.
       - [0x0400] Output GC statistics at program exit.
       - [0x0800] GC debugging messages.
       - [0x1000] Address space reservation changes.
       Default: 0. *)

    max_overhead : int;
    (** Heap compaction is triggered when the estimated amount
       of "wasted" memory is more than [max_overhead] percent of the
       amount of live data.  If [max_overhead] is set to 0, heap
       compaction is triggered at the end of each major GC cycle
       (this setting is intended for testing purposes only).
       If [max_overhead >= 1000000], compaction is never triggered.

       This field is currently not available in OCaml 5: the field value is
       always [0]. *)

    stack_limit : int;
    (** The maximum size of the fiber stacks (in words).
       Default: 128M. *)

    allocation_policy : int;
    (** The policy used for allocating in the major heap.

        This field is currently not available in OCaml 5: the field value is
        always [0].

        Prior to OCaml 5.0, possible values were 0, 1 and 2.

        - 0 was the next-fit policy

        - 1 was the first-fit policy (since OCaml 3.11)

        - 2 was the best-fit policy (since OCaml 4.10)

        @since 3.11 *)

    window_size : int;
    (** The size of the window used by the major GC for smoothing
        out variations in its workload. This is an integer between
        1 and 50.
        @since 4.03

        This field is currently not available in OCaml 5: the field value is
        always [0]. *)

    custom_major_ratio : int;
    (** Target ratio of floating garbage to major heap size for
        out-of-heap memory held by custom values located in the major
        heap. The GC speed is adjusted to try to use this much memory
        for dead values that are not yet collected. Expressed as a
        percentage of major heap size. The default value keeps the
        out-of-heap floating garbage about the same size as the
        in-heap overhead.
        Note: this only applies to values allocated with
        [caml_alloc_custom_mem] (e.g. bigarrays).
        Default: 44.
        @since 4.08 *)

    custom_minor_ratio : int;
    (** Bound on floating garbage for out-of-heap memory held by
        custom values in the minor heap. A minor GC is triggered when
        this much memory is held by custom values located in the minor
        heap. Expressed as a percentage of minor heap size.
        Note: this only applies to values allocated with
        [caml_alloc_custom_mem] (e.g. bigarrays).
        Default: 100.
        @since 4.08 *)

    custom_minor_max_size : int;
    (** Maximum amount of out-of-heap memory for each custom value
        allocated in the minor heap. Custom values that hold more
        than this many bytes are allocated on the major heap.
        Note: this only applies to values allocated with
        [caml_alloc_custom_mem] (e.g. bigarrays).
        Default: 70000 bytes.
        @since 4.08 *)
  }
(** The GC parameters are given as a [control] record.  Note that
    these parameters can also be initialised by setting the
    OCAMLRUNPARAM environment variable.  See the documentation of
    [ocamlrun]. *)

external stat : unit -> stat = "caml_gc_stat"
(** Return the current values of the memory management counters in a
    [stat] record that represents the program's total memory stats.
    The [heap_chunks], [free_blocks], [largest_free], and [stack_size] metrics
    are currently not available in OCaml 5: their returned field values are
    therefore [0].
    This function causes a full major collection. *)

external quick_stat : unit -> stat = "caml_gc_quick_stat"
(** Returns a record with the current values of the memory management counters
    like [stat]. Unlike [stat], [quick_stat] does not perform a full major
    collection, and hence, is much faster. However, [quick_stat] reports the
    counters sampled at the last minor collection or at the end of the last
    major collection cycle (whichever is the latest). Hence, the memory stats
    returned by [quick_stat] are not instantaneously accurate. *)

external counters : unit -> float * float * float = "caml_gc_counters"
(** Return [(minor_words, promoted_words, major_words)] for the current
    domain or potentially previous domains.  This function is as fast as
    [quick_stat]. *)

external minor_words : unit -> (float [@unboxed])
  = "caml_gc_minor_words" "caml_gc_minor_words_unboxed"
(** Number of words allocated in the minor heap by this domain or potentially
    previous domains. This number is accurate in byte-code programs, but
    only an approximation in programs compiled to native code.

    In native code this function does not allocate.

    @since 4.04 *)

external get : unit -> control = "caml_gc_get"
[@@alert unsynchronized_access
    "GC parameters are a mutable global state."
]
(** Return the current values of the GC parameters in a [control] record.

    The [major_heap_increment], [max_overhead], [allocation_policy], and
    [window_size] fields are currently not available in OCaml 5: their returned
    field values are therefore [0]. *)

external set : control -> unit = "caml_gc_set"
[@@alert unsynchronized_access
    "GC parameters are a mutable global state."
]
(** [set r] changes the GC parameters according to the [control] record [r].
    The normal usage is: [Gc.set { (Gc.get()) with Gc.verbose = 0x00d }]

    The [major_heap_increment], [max_overhead], [allocation_policy], and
    [window_size] fields are currently not available in OCaml 5: setting them
    therefore has no effect. *)

external minor : unit -> unit = "caml_gc_minor"
(** Trigger a minor collection. *)

external major_slice : int -> int = "caml_gc_major_slice"
(** [major_slice n]
    Do a minor collection and a slice of major collection. [n] is the
    size of the slice: the GC will do enough work to free (on average)
    [n] words of memory. If [n] = 0, the GC will try to do enough work
    to ensure that the next automatic slice has no work to do.
    This function returns an unspecified integer (currently: 0). *)

external major : unit -> unit = "caml_gc_major"
(** Do a minor collection and finish the current major collection cycle. *)

external full_major : unit -> unit = "caml_gc_full_major"
(** Do a minor collection, finish the current major collection cycle,
   and perform a complete new cycle.  This will collect all currently
   unreachable blocks. *)

external compact : unit -> unit = "caml_gc_compaction"
(** Perform a full major collection and compact the heap.  Note that heap
   compaction is a lengthy operation. *)

val print_stat : out_channel -> unit
(** Print the current values of the memory management counters (in
   human-readable form) of the total program into the channel argument. *)

val allocated_bytes : unit -> float
(** Return the number of bytes allocated by this domain and potentially
   a previous domain. It is returned as a [float] to avoid overflow problems
   with [int] on 32-bit machines. *)

external get_minor_free : unit -> int = "caml_get_minor_free"
(** Return the current size of the free space inside the minor heap of this
   domain.

    @since 4.03 *)

val finalise : ('a -> unit) -> 'a -> unit
(** [finalise f v] registers [f] as a finalisation function for [v].
   [v] must be heap-allocated.  [f] will be called with [v] as
   argument at some point between the first time [v] becomes unreachable
   (including through weak pointers) and the time [v] is collected by
   the GC. Several functions can
   be registered for the same value, or even several instances of the
   same function.  Each instance will be called once (or never,
   if the program terminates before [v] becomes unreachable).

   The GC will call the finalisation functions in the order of
   deallocation.  When several values become unreachable at the
   same time (i.e. during the same GC cycle), the finalisation
   functions will be called in the reverse order of the corresponding
   calls to [finalise].  If [finalise] is called in the same order
   as the values are allocated, that means each value is finalised
   before the values it depends upon.  Of course, this becomes
   false if additional dependencies are introduced by assignments.

   In the presence of multiple OCaml threads it should be assumed that
   any particular finaliser may be executed in any of the threads.

   Anything reachable from the closure of finalisation functions
   is considered reachable, so the following code will not work
   as expected:
   - [ let v = ... in Gc.finalise (fun _ -> ...v...) v ]

   Instead you should make sure that [v] is not in the closure of
   the finalisation function by writing:
   - [ let f = fun x -> ...  let v = ... in Gc.finalise f v ]


   The [f] function can use all features of OCaml, including
   assignments that make the value reachable again.  It can also
   loop forever (in this case, the other
   finalisation functions will not be called during the execution of f,
   unless it calls [finalise_release]).
   It can call [finalise] on [v] or other values to register other
   functions or even itself.  It can raise an exception; in this case
   the exception will interrupt whatever the program was doing when
   the function was called.


   [finalise] will raise [Invalid_argument] if [v] is not
   guaranteed to be heap-allocated.  Some examples of values that are not
   heap-allocated are integers, constant constructors, booleans,
   the empty array, the empty list, the unit value.  The exact list
   of what is heap-allocated or not is implementation-dependent.
   Some constant values can be heap-allocated but never deallocated
   during the lifetime of the program, for example a list of integer
   constants; this is also implementation-dependent.
   Note that values of types [float] are sometimes allocated and
   sometimes not, so finalising them is unsafe, and [finalise] will
   also raise [Invalid_argument] for them. Values of type ['a Lazy.t]
   (for any ['a]) are like [float] in this respect, except that the
   compiler sometimes optimizes them in a way that prevents [finalise]
   from detecting them. In this case, it will not raise
   [Invalid_argument], but you should still avoid calling [finalise]
   on lazy values.


   The results of calling {!String.make}, {!Bytes.make}, {!Bytes.create},
   {!Array.make}, and {!val:Stdlib.ref} are guaranteed to be
   heap-allocated and non-constant except when the length argument is [0].
*)

val finalise_last : (unit -> unit) -> 'a -> unit
(** same as {!finalise} except the value is not given as argument. So
    you can't use the given value for the computation of the
    finalisation function. The benefit is that the function is called
    after the value is unreachable for the last time instead of the
    first time. So contrary to {!finalise} the value will never be
    reachable again or used again. In particular every weak pointer
    and ephemeron that contained this value as key or data is unset
    before running the finalisation function. Moreover the finalisation
    functions attached with {!finalise} are always called before the
    finalisation functions attached with {!finalise_last}.

    @since 4.04
*)

val finalise_release : unit -> unit
(** A finalisation function may call [finalise_release] to tell the
    GC that it can launch the next finalisation function without waiting
    for the current one to return. *)

type alarm
(** An alarm is a piece of data that calls a user function at the end of
   major GC cycle.  The following functions are provided to create
   and delete alarms. *)

val create_alarm : (unit -> unit) -> alarm
(** [create_alarm f] will arrange for [f] to be called at the end of
   major GC cycles, not caused by [f] itself, starting with the
   current cycle or the next one. [f] will run on the same domain that
   created the alarm, until the domain exits or [delete_alarm] is
   called. A value of type [alarm] is returned that you can use to
   call [delete_alarm].

   It is not guaranteed that the Gc alarm runs at the end of every major
   GC cycle, but it is guaranteed that it will run eventually.

   As an example, here is a crude way to interrupt a function if the
   memory consumption of the program exceeds a given [limit] in MB,
   suitable for use in the toplevel:

   {[
let run_with_memory_limit (limit : int) (f : unit -> 'a) : 'a =
  let limit_memory () =
    let mem = Gc.(quick_stat ()).heap_words in
    if mem / (1024 * 1024) > limit / (Sys.word_size / 8) then
      raise Out_of_memory
  in
  let alarm = Gc.create_alarm limit_memory in
  Fun.protect f ~finally:(fun () -> Gc.delete_alarm alarm ; Gc.compact ())
   ]}

*)

val delete_alarm : alarm -> unit
(** [delete_alarm a] will stop the calls to the function associated
   to [a]. Calling [delete_alarm a] again has no effect. *)

val eventlog_pause : unit -> unit
[@@ocaml.deprecated "Use Runtime_events.pause instead."]

val eventlog_resume : unit -> unit
[@@ocaml.deprecated "Use Runtime_events.resume instead."]

(** [Memprof] is a profiling engine which randomly samples allocated
   memory words. Every allocated word has a probability of being
   sampled equal to a configurable sampling rate. Once a block is
   sampled, it becomes tracked. A tracked block triggers a
   user-defined callback as soon as it is allocated, promoted or
   deallocated.

   Since blocks are composed of several words, a block can potentially
   be sampled several times. If a block is sampled several times, then
   each of the callbacks is called once for each event of this block:
   the multiplicity is given in the [n_samples] field of the
   [allocation] structure.

   This engine makes it possible to implement a low-overhead memory
   profiler as an OCaml library.

   Note: this API is EXPERIMENTAL. It may change without prior
   notice. *)
module Memprof :
  sig
    type t
    (** the type of a profile *)

    type allocation_source = Normal | Marshal | Custom
    type allocation = private
      { n_samples : int;
        (** The number of samples in this block (>= 1). *)

        size : int;
        (** The size of the block, in words, excluding the header. *)

        source : allocation_source;
        (** The cause of the allocation. *)

        callstack : Printexc.raw_backtrace
        (** The callstack for the allocation. *)
      }
    (** The type of metadata associated with allocations. This is the
       type of records passed to the callback triggered by the
       sampling of an allocation. *)

    type ('minor, 'major) tracker = {
      alloc_minor: allocation -> 'minor option;
      alloc_major: allocation -> 'major option;
      promote: 'minor -> 'major option;
      dealloc_minor: 'minor -> unit;
      dealloc_major: 'major -> unit;
    }
    (**
       A [('minor, 'major) tracker] describes how memprof should track
       sampled blocks over their lifetime, keeping a user-defined piece
       of metadata for each of them: ['minor] is the type of metadata
       to keep for minor blocks, and ['major] the type of metadata
       for major blocks.

       The member functions in a [tracker] are called callbacks.

       If an allocation or promotion callback raises an exception or
       returns [None], memprof stops tracking the corresponding block.
       *)

    val null_tracker: ('minor, 'major) tracker
    (** Default callbacks simply return [None] or [()] *)

    val start :
      sampling_rate:float ->
      ?callstack_size:int ->
      ('minor, 'major) tracker ->
      t
    (** Start a profile with the given parameters. Raises an exception
       if a profile is already sampling in the current domain.

       Sampling begins immediately. The parameter [sampling_rate] is
       the sampling rate in samples per word (including headers).
       Usually, with cheap callbacks, a rate of 1e-4 has no visible
       effect on performance, and 1e-3 causes the program to run a few
       percent slower. 0.0 <= sampling_rate <= 1.0.

       The parameter [callstack_size] is the length of the callstack
       recorded at every sample. Its default is [max_int].

       The parameter [tracker] determines how to track sampled blocks
       over their lifetime in the minor and major heap.

       Sampling and running callbacks are temporarily disabled on the
       current thread when calling a callback, so callbacks do not
       need to be re-entrant if the program is single-threaded and
       single-domain. However, if threads or multiple domains are
       used, it is possible that several callbacks will run in
       parallel. In this case, callback functions must be re-entrant.

       Note that a callback may be postponed slightly after the actual
       event. The callstack passed to an allocation callback always
       accurately reflects the allocation, but the program state may
       have evolved between the allocation and the call to the
       callback.

       If a new thread or domain is created when the current domain is
       sampling for a profile, the child thread or domain joins that
       profile (using the same [sampling_rate], [callstack_size], and
       [tracker] callbacks).

       An allocation callback is always run by the thread which
       allocated the block. If the thread exits or the profile is
       stopped before the callback is called, the allocation callback
       is not called and the block is not tracked.

       Each subsequent callback is generally run by the domain which
       allocated the block. If the domain terminates or the profile is
       stopped before the callback is called, the callback may be run
       by a different domain.

       Different domains may sample for different profiles
       simultaneously.  *)

    val stop : unit -> unit
    (** Stop sampling for the current profile. Fails if no profile is
       sampling in the current domain. Stops sampling in all threads
       and domains sharing the profile.

       Promotion and deallocation callbacks from a profile may run
       after [stop] is called, until [discard] is applied to the
       profile.

       A profile is implicitly stopped (but not discarded) if all
       domains and threads sampling for it are terminated.
       *)

    val discard : t -> unit
    (** Discards all profiling state for a stopped profile, which
       prevents any more callbacks for it. Raises an exception if
       called on a profile which has not been stopped.
       *)
end
