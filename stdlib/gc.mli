(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Gc]: memory management control and statistics *)

type stat = {
  minor_words : int;
  promoted_words : int;
  major_words : int;
  minor_collections : int;
  major_collections : int;
  heap_words : int;
  heap_chunks : int;
  live_words : int;
  live_blocks : int;
  free_words : int;
  free_blocks : int;
  largest_free : int;
  fragments : int;
  compactions : int
}
  (* The memory management counters are returned in a [stat] record.
     All the numbers are computed since the start of the program.
     The fields of this record are:
-     [minor_words]  Number of words allocated in the minor heap.
-     [promoted_words] Number of words allocated in the minor heap that
             survived a minor collection and were moved to the major heap.
-     [major_words]  Number of words allocated in the major heap, including
             the promoted words.
-     [minor_collections]  Number of minor collections.
-     [major_collections]  Number of major collection cycles, not counting
             the current cycle.
-     [heap_words]  Total number of words in the major heap.
-     [heap_chunks]  Number of times the major heap size was increased.
-     [live_words]  Number of words of live data in the major heap, including
             the header words.
-     [live_blocks]  Number of live objects in the major heap.
-     [free_words]  Number of words in the free list.
-     [free_blocks]  Number of objects in the free list.
-     [largest_free]  Size (in words) of the largest object in the free list.
-     [fragments]  Number of wasted words due to fragmentation.  These are
             1-words free blocks placed between two live objects.  They
             cannot be inserted in the free list, thus they are not available
             for allocation.
-     [compactions]  Number of heap compactions.

     The total amount of memory allocated by the program is (in words)
     [minor_words + major_words - promoted_words].  Multiply by
     the word size (4 on a 32-bit machine, 8 on a 64-bit machine) to get
     the number of bytes.
  *)

type control = {
  mutable minor_heap_size : int;
  mutable major_heap_increment : int;
  mutable space_overhead : int;
  mutable verbose : bool;
  mutable max_overhead : int;
  mutable stack_limit : int
}

  (* The GC parameters are given as a [control] record.  The fields are:
-     [minor_heap_size]  The size (in words) of the minor heap.  Changing
             this parameter will trigger a minor collection.  Default: 32k.
-     [major_heap_increment]  The minimum number of words to add to the
             major heap when increasing it.  Default: 62k.
-     [space_overhead]  The major GC speed is computed from this parameter.
             This is the memory that will be "wasted" because the GC does not
             immediatly collect unreachable objects.  It is expressed as a
             percentage of the memory used for live data.
             The GC will work more (use more CPU time and collect
             objects more eagerly) if [space_overhead] is smaller.
             The computation of the GC speed assumes that the amount
             of live data is constant.  Default: 42.
-     [max_overhead]  Heap compaction is triggered when the estimated amount
             of free memory is more than [max_overhead] percent of the amount
             of live data.  If [max_overhead] is set to 0, heap
             compaction is triggered at the end of each major GC cycle
             (this setting is intended for testing purposes only).
	     If [max_overhead >= 1000000], compaction is never triggered.
             Default: 1000000.
-     [verbose]  This flag controls the GC messages on standard error output.
             Default: false.
-     [stack_limit]  The maximum size of the stack (in words).  This is only
             relevant to the byte-code runtime, as the native code runtime
             uses the operating system's stack.  Default: 256k.
  *)

external stat : unit -> stat = "gc_stat"
  (* Return the current values of the memory management counters in a
     [stat] record. *)
val print_stat : out_channel -> unit
  (* Print the current values of the memory management counters (in
     human-readable form) into the channel argument. *)
external get : unit -> control = "gc_get"
  (* Return the current values of the GC parameters in a [control] record. *)
external set : control -> unit = "gc_set"
  (* [set r] changes the GC parameters according to the [control] record [r].
     The normal usage is:
     [
       let r = Gc.get () in    (* Get the current parameters. *)
         r.verbose <- true;    (* Change some of them. *)
         Gc.set r              (* Set the new values. *)
     ]
  *)
external minor : unit -> unit = "gc_minor"
  (* Trigger a minor collection. *)
external major : unit -> unit = "gc_major"
  (* Finish the current major collection cycle. *)
external full_major : unit -> unit = "gc_full_major"
  (* Finish the current major collection cycle and perform a complete
     new cycle.  This will collect all currently unreachable objects. *)
external compact : unit -> unit = "gc_compaction";;
  (* Perform a full major collection and compact the heap.  Note that heap
     compaction is a lengthy operation. *)
