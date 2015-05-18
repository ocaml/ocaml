(* additional GC functions *)
(* These are in a separate file because we want to do our testing
   on code that doesn't like changes to the Gc module's interface.
   Ultimately, these new functions will be in Gc.
*)

external get_minor_free : unit -> int = "caml_get_minor_free" "noalloc"
(** Return the current size of the free space inside the minor heap. *)

external get_bucket : int -> int = "caml_get_major_bucket" "noalloc"
(** [get_bucket n] returns the current size of the [n]-th future bucket
    of the GC smoothing system. The unit is one millionth of a full GC.
    Raise [Invalid_argument] if [n] is negative, return 0 if n is larger
    than the smoothing window.
*)

external get_credit : unit -> int = "caml_get_major_credit" "noalloc"
(** [get_bucket n] returns the current size of the "work done in advance"
    counter of the GC smoothing system. The unit is one millionth of a
    full GC.
*)

type control =
  { mutable minor_heap_size : int;
    (** The size (in words) of the minor heap.  Changing
       this parameter will trigger a minor collection.  Default: 256k. *)

    mutable major_heap_increment : int;
    (** How much to add to the major heap when increasing it. If this
        number is less than or equal to 1000, it is a percentage of
        the current heap size (i.e. setting it to 100 will double the heap
        size at each increase). If it is more than 1000, it is a fixed
        number of words that will be added to the heap. Default: 15. *)

    mutable space_overhead : int;
    (** The major GC speed is computed from this parameter.
       This is the memory that will be "wasted" because the GC does not
       immediatly collect unreachable blocks.  It is expressed as a
       percentage of the memory used for live data.
       The GC will work more (use more CPU time and collect
       blocks more eagerly) if [space_overhead] is smaller.
       Default: 80. *)

    mutable verbose : int;
    (** This value controls the GC messages on standard error output.
       It is a sum of some of the following flags, to print messages
       on the corresponding events:
       - [0x001] Start of major GC cycle.
       - [0x002] Minor collection and major GC slice.
       - [0x004] Growing and shrinking of the heap.
       - [0x008] Resizing of stacks and memory manager tables.
       - [0x010] Heap compaction.
       - [0x020] Change of GC parameters.
       - [0x040] Computation of major GC slice size.
       - [0x080] Calling of finalisation functions.
       - [0x100] Bytecode executable search at start-up.
       - [0x200] Computation of compaction triggering condition.
       Default: 0. *)

    mutable max_overhead : int;
    (** Heap compaction is triggered when the estimated amount
       of "wasted" memory is more than [max_overhead] percent of the
       amount of live data.  If [max_overhead] is set to 0, heap
       compaction is triggered at the end of each major GC cycle
       (this setting is intended for testing purposes only).
       If [max_overhead >= 1000000], compaction is never triggered.
       If compaction is permanently disabled, it is strongly suggested
       to set [allocation_policy] to 1.
       Default: 500. *)

    mutable stack_limit : int;
    (** The maximum size of the stack (in words).  This is only
       relevant to the byte-code runtime, as the native code runtime
       uses the operating system's stack.  Default: 1024k. *)

    mutable allocation_policy : int;
    (** The policy used for allocating in the heap.  Possible
        values are 0 and 1.  0 is the next-fit policy, which is
        quite fast but can result in fragmentation.  1 is the
        first-fit policy, which can be slower in some cases but
        can be better for programs with fragmentation problems.
        Default: 0. @since 3.11.0 *)

    window_size : int;
    (** The size of the window used by the major GC for smoothing
        out variations in its workload. This is an integer between
        1 and 50.
        Default: 1. @since 4.03.0 *)
}

external get : unit -> control = "caml_gc_get"
(** Return the current values of the GC parameters in a [control] record. *)

external set : control -> unit = "caml_gc_set"
(** [set r] changes the GC parameters according to the [control] record [r].
   The normal usage is: [Gc.set { (Gc.get()) with Gc.verbose = 0x00d }] *)

external huge_fallback_count : unit -> int = "caml_gc_huge_fallback_count"
(** Return the number of times we tried to map huge pages and had to fall
    back to small pages. *)
