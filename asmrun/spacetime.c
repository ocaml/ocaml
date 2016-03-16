/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Mark Shinwell and Leo White, Jane Street Europe             */
/*                                                                        */
/*   Copyright 2013--2016, Jane Street Group, LLC                         */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <math.h>
#include <sys/resource.h>

#include "caml/alloc.h"
#include "caml/backtrace_prim.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/intext.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "caml/sys.h"
#include "spacetime.h"
#include "stack.h"

#ifdef WITH_SPACETIME

#ifdef HAS_LIBUNWIND
#include "libunwind.h"
#endif

#pragma GCC optimize ("-O0")

static void debug_printf(const char* format, ...)
{
}

extern value caml_spacetime_debug(value);

/* The following structures must match the type definitions in the
   [Spacetime] module. */

typedef struct {
  /* (GC header here.) */
  value minor_words;
  value promoted_words;
  value major_words;
  value minor_collections;
  value major_collections;
  value heap_words;
  value heap_chunks;
  value compactions;
  value top_heap_words;
} gc_stats;

typedef struct {
  value profinfo;
  value num_blocks;
  value num_words_including_headers;
} snapshot_entry;

typedef struct {
  /* (GC header here.) */
  snapshot_entry entries[0];
} snapshot_entries;

typedef struct {
  /* (GC header here.) */
  value time;  /* Cf. [Sys.time]. */
  value gc_stats;
  value entries;
  value num_blocks_in_minor_heap;
  value num_blocks_in_major_heap;
  value num_blocks_in_minor_heap_with_profinfo;
  value num_blocks_in_major_heap_with_profinfo;
} snapshot;

static const uintnat profinfo_none = (uintnat) 0;
static const uintnat profinfo_overflow = (uintnat) 1;
uintnat caml_spacetime_profinfo = (uintnat) 2;

static value allocate_outside_heap_with_tag(mlsize_t size_in_bytes, tag_t tag)
{
  /* CR mshinwell: this function should live somewhere else */
  header_t* block;

  assert(size_in_bytes % sizeof(value) == 0);
  block = caml_stat_alloc(sizeof(header_t) + size_in_bytes);
  *block = Make_header(size_in_bytes / sizeof(value), tag, Caml_black);
  return (value) &block[1];
}

static value allocate_outside_heap(mlsize_t size_in_bytes)
{
  assert(size_in_bytes > 0);
  return allocate_outside_heap_with_tag(size_in_bytes, 0);
}

static value take_gc_stats(void)
{
  value v_stats;
  gc_stats* stats;

  v_stats = allocate_outside_heap(sizeof(gc_stats));
  stats = (gc_stats*) v_stats;

  stats->minor_words = Val_long(caml_stat_minor_words);
  stats->promoted_words = Val_long(caml_stat_promoted_words);
  stats->major_words =
    Val_long(((uintnat) caml_stat_major_words)
             + ((uintnat) caml_allocated_words));
  stats->minor_collections = Val_long(caml_stat_minor_collections);
  stats->major_collections = Val_long(caml_stat_major_collections);
  stats->heap_words = Val_long(caml_stat_heap_wsz / sizeof(value));
  stats->heap_chunks = Val_long(caml_stat_heap_chunks);
  stats->compactions = Val_long(caml_stat_compactions);
  stats->top_heap_words = Val_long(caml_stat_top_heap_wsz / sizeof(value));

  return v_stats;
}

typedef struct {
  uintnat num_blocks;
  uintnat num_words_including_headers;
} raw_snapshot_entry;

CAMLprim value caml_spacetime_take_heap_snapshot(void)
{
  value v_snapshot;
  snapshot* heap_snapshot;
  value v_entries;
  snapshot_entries* entries;
  char* chunk;
  value gc_stats;
  uintnat index;
  uintnat target_index;
  value v_time;
  double time;
  uintnat profinfo;
  uintnat num_distinct_profinfos;
  value* ptr;
  /* Fixed size buffer to avoid needing a hash table: */
  static raw_snapshot_entry* raw_entries = NULL;
  uintnat num_blocks_in_minor_heap = 0;
  uintnat num_blocks_in_minor_heap_with_profinfo = 0;
  uintnat num_blocks_in_major_heap = 0;
  uintnat num_blocks_in_major_heap_with_profinfo = 0;

  time = caml_sys_time_unboxed(Val_unit);
  gc_stats = take_gc_stats();

  if (raw_entries == NULL) {
    size_t size = (PROFINFO_MASK + 1) * sizeof(raw_snapshot_entry);
    raw_entries = caml_stat_alloc(size);
    memset(raw_entries, '\0', size);
  }

  num_distinct_profinfos = 0;

  /* Scan the minor heap. */
  assert(((uintnat) caml_young_ptr) % sizeof(value) == 0);
  ptr = (value*) caml_young_ptr;
  assert(ptr >= (value*) caml_young_start);
  while (ptr < (value*) caml_young_end) {
    header_t hd;
    value value_in_minor_heap;

    ptr++;
    value_in_minor_heap = (value) ptr;
    assert(Is_young(value_in_minor_heap));
    assert(Is_block(value_in_minor_heap));

    hd = Hd_val(value_in_minor_heap);

    /* We do not expect the value to be promoted, since this function
       should not be called during a minor collection. */
    assert(hd != 0);

    profinfo = Profinfo_hd(hd);

    num_blocks_in_minor_heap++;
    if (profinfo >= caml_profinfo_lowest && profinfo <= PROFINFO_MASK) {
      num_blocks_in_minor_heap_with_profinfo++;
      assert (raw_entries[profinfo].num_blocks >= 0);
      if (raw_entries[profinfo].num_blocks == 0) {
        num_distinct_profinfos++;
      }
      raw_entries[profinfo].num_blocks++;
      raw_entries[profinfo].num_words_including_headers +=
        Whsize_val(value_in_minor_heap);
    }

    ptr += Wosize_val(value_in_minor_heap);
  }

  /* Scan the major heap. */
  chunk = caml_heap_start;
  while (chunk != NULL) {
    char* hp;
    char* limit;

    hp = chunk;
    limit = chunk + Chunk_size (chunk);

    while (hp < limit) {
      header_t hd = Hd_hp (hp);
      switch (Color_hd(hd)) {
        case Caml_blue:
          break;

        default:
          profinfo = Profinfo_hd(hd);
          num_blocks_in_major_heap++;
          if (profinfo >= caml_profinfo_lowest && profinfo <= PROFINFO_MASK) {
            num_blocks_in_major_heap_with_profinfo++;
            assert (raw_entries[profinfo].num_blocks >= 0);
            if (raw_entries[profinfo].num_blocks == 0) {
              num_distinct_profinfos++;
            }
            raw_entries[profinfo].num_blocks++;
            raw_entries[profinfo].num_words_including_headers +=
              Whsize_hd(hd);
          }
          break;
      }
      hp += Bhsize_hd (hd);
      Assert (hp <= limit);
    }

    chunk = Chunk_next (chunk);
  }

  if(num_distinct_profinfos > 0) {
    v_entries = allocate_outside_heap(
      num_distinct_profinfos*sizeof(snapshot_entry));
    entries = (snapshot_entries*) v_entries;
    target_index = 0;
    for (index = caml_profinfo_lowest; index <= PROFINFO_MASK; index++) {
      assert(raw_entries[index].num_blocks >= 0);
      if (raw_entries[index].num_blocks > 0) {
        assert(target_index < num_distinct_profinfos);
        entries->entries[target_index].profinfo = Val_long(index);
        entries->entries[target_index].num_blocks
          = Val_long(raw_entries[index].num_blocks);
        entries->entries[target_index].num_words_including_headers
          = Val_long(raw_entries[index].num_words_including_headers);
        target_index++;
      }
    }
  } else {
    v_entries = Atom(0);
  }

  assert(sizeof(double) == sizeof(value));
  v_time = allocate_outside_heap_with_tag(sizeof(double), Double_tag);
  Double_field(v_time, 0) = time;

  v_snapshot = allocate_outside_heap(sizeof(snapshot));
  heap_snapshot = (snapshot*) v_snapshot;

  heap_snapshot->time = v_time;
  heap_snapshot->gc_stats = gc_stats;
  heap_snapshot->entries = v_entries;
  heap_snapshot->num_blocks_in_minor_heap =
    Val_long(num_blocks_in_minor_heap);
  heap_snapshot->num_blocks_in_major_heap =
    Val_long(num_blocks_in_major_heap);
  heap_snapshot->num_blocks_in_minor_heap_with_profinfo
    = Val_long(num_blocks_in_minor_heap_with_profinfo);
  heap_snapshot->num_blocks_in_major_heap_with_profinfo
    = Val_long(num_blocks_in_major_heap_with_profinfo);

  return v_snapshot;
}

CAMLprim value caml_spacetime_free_heap_snapshot(value v_snapshot)
{
  snapshot* heap_snapshot = (snapshot*) v_snapshot;
  caml_stat_free(Hp_val(heap_snapshot->time));
  caml_stat_free(Hp_val(heap_snapshot->gc_stats));
  if (Wosize_val(heap_snapshot->entries) > 0) {
    caml_stat_free(Hp_val(heap_snapshot->entries));
  }
  caml_stat_free(Hp_val(v_snapshot));
  return Val_unit;
}

CAMLprim value caml_spacetime_marshal_heap_snapshot
      (value v_channel, value v_snapshot)
{
  caml_extern_allow_out_of_heap = 1;
  caml_output_value(v_channel, v_snapshot, Val_long(0));
  caml_extern_allow_out_of_heap = 0;

  return Val_unit;
}

CAMLprim value
caml_spacetime_num_frame_descriptors(value unit)
{
  assert(unit == Val_unit);

  if (caml_frame_descriptors == NULL) {
    caml_init_frame_descriptors();
  }

  return Val_long(caml_frame_descriptors_mask + 1);
}

CAMLprim value
caml_spacetime_get_frame_descriptor(value v_index)
{
  uintnat index;
  value v_result;
  frame_descr* descr;

  assert(!Is_block(v_index));
  index = Long_val(v_index);
  if (index > caml_frame_descriptors_mask) {
    caml_failwith("caml_spacetime_get_frametable: bad index");
  }

  if (caml_frame_descriptors == NULL) {
    caml_init_frame_descriptors();
  }
  
  descr = caml_frame_descriptors[index];

  if (descr == NULL) {
    return Val_long(0 /* None */);
  }

  v_result = caml_alloc_small(1, 1 /* Some */);
  Field(v_result, 0) = caml_val_raw_backtrace_slot((backtrace_slot) descr);

  return v_result;
}

CAMLprim value
caml_spacetime_return_address_of_frame_descriptor(value v_descr)
{
  frame_descr* descr;

  descr = (frame_descr*) caml_raw_backtrace_slot_val(v_descr);
  assert(descr != NULL);

  return caml_copy_int64(descr->retaddr);
}

CAMLprim value
caml_forget_where_values_were_allocated (value v_unit)
{

  assert(v_unit == Val_unit);

  /* CR mshinwell: This function should traverse the minor heap. */
#if 0
  char* chunk;
  caml_minor_collection();

  chunk = caml_heap_start;

  while (chunk != NULL) {
    char* hp;
    char* limit;

    hp = chunk;
    limit = chunk + Chunk_size (chunk);

    while (hp < limit) {
      header_t hd = Hd_hp (hp);
      Hd_hp (hp) = Make_header (Wosize_hd(hd), Tag_hd(hd), Color_hd(hd));
      hp += Bhsize_hd (hd);
      Assert (hp <= limit);
    }

    chunk = Chunk_next (chunk);
  }
#endif

  return v_unit;
}

static value caml_spacetime_trie_root = Val_unit;
value* caml_spacetime_trie_node_ptr = &caml_spacetime_trie_root;

static value caml_spacetime_finaliser_trie_root_main_thread = Val_unit;
value* caml_spacetime_finaliser_trie_root
  = &caml_spacetime_finaliser_trie_root_main_thread;

value caml_spacetime_use_override_profinfo = Val_false;
uintnat caml_spacetime_override_profinfo;

void caml_spacetime_initialize (void)
{
}

CAMLprim value caml_spacetime_trie_is_initialized (value v_unit)
{
  return (caml_spacetime_trie_root == Val_unit) ? Val_false : Val_true;
}

CAMLprim value caml_spacetime_get_trie_root (value v_unit)
{
  return caml_spacetime_trie_root;
}

CAMLprim value caml_spacetime_do_not_override_profinfo (value v_unit)
{
  v_unit = v_unit;
  caml_spacetime_use_override_profinfo = Val_false;
  return Val_unit;
}

CAMLprim value caml_spacetime_set_override_profinfo (value v_override)
{
  uintnat override = (uintnat) Long_val (v_override);
  if (override == profinfo_none
      || override == profinfo_overflow
      || override > PROFINFO_MASK) {
    return Val_false;
  }
  caml_spacetime_use_override_profinfo = Val_true;
  caml_spacetime_override_profinfo = override;
  return Val_true;
}

CAMLprim value caml_spacetime_get_profinfo (value v)
{
  return Val_long(Profinfo_val(v));
}

CAMLprim value caml_spacetime_profinfo_none (value v_unit)
{
  return Val_long(profinfo_none);
}

CAMLprim value caml_spacetime_profinfo_overflow (value v_unit)
{
  return Val_long(profinfo_overflow);
}

static int pc_inside_c_node_matches(c_node* node, void* pc)
{
  return Decode_c_node_pc(node->pc) == pc;
}

static value allocate_uninitialized_ocaml_node(int size_including_header)
{
  void* node;
  assert(size_including_header >= 3);
  node = caml_stat_alloc(sizeof(uintnat) * size_including_header);
  /* We don't currently rely on [uintnat] alignment, but we do need some
     alignment, so just be sure. */
  assert (((uintnat) node) % sizeof(uintnat) == 0);
  debug_printf("allocate ocaml node: Val_hp=%p\n", (void*) Val_hp(node));
  return Val_hp(node);
}

static value find_tail_node(value node, void* callee)
{
  /* Search the tail chain within [node] (which corresponds to an invocation
     of a caller of [callee]) to determine whether it contains a tail node
     corresponding to [callee].  Returns any such node, or [Val_unit] if no
     such node exists. */

  value starting_node;
  value pc;
  value found = Val_unit;

  debug_printf("find_tail_node with callee %p\n", callee);
  starting_node = node;
  pc = Encode_node_pc(callee);

  do {
    assert(Is_ocaml_node(node));
    debug_printf("find_tail_node comparing %p with %p\n",
      (void*) Decode_node_pc(Node_pc(node)), (void*) callee);
    if (Node_pc(node) == pc) {
      found = node;
    }
    else {
      node = Tail_link(node);
    }
  } while (found == Val_unit && starting_node != node);

  debug_printf("find_tail_node returns value pointer %p\n", (void*) found);

  return found;
}

CAMLprim value caml_spacetime_check_node(
      value node, void* pc)
{
  assert(Is_ocaml_node(node));
  if (Decode_node_pc(Node_pc(node)) != pc) {
    debug_printf("check_node failure: OCaml node %p should have PC %p but has %p\n",
      (void*) node, pc, Decode_node_pc(Node_pc(node)));
    assert(0);
  }
  return Val_unit;
}

CAMLprim value caml_spacetime_allocate_node(
      int size_including_header, void* pc, value* node_hole)
{
  int field;
  value node;
  value caller_node = Val_unit;

  node = *node_hole;
  /* The node hole should either contain [Val_unit], indicating that this
     function was not tail called and we have not been to this point in the
     trie before; or it should contain a value encoded using
     [Encoded_tail_caller_node] that points at the node of a caller
     that tail called the current function.  (Such a value is necessary to
     be able to find the start of the caller's node, and hence its tail
     chain, so we as a tail-called callee can link ourselves in.) */
  assert(Is_tail_caller_node_encoded(node));

  if (node != Val_unit) {
    value tail_node;
    /* The callee was tail called.  Find whether there already exists a node
       for it in the tail call chain within the caller's node.  The caller's
       node must always be an OCaml node. */
debug_printf("allocating node for callee that was tail called.  Identifying PC of callee=%p.\n",pc);
    caller_node = Decode_tail_caller_node(node);
    tail_node = find_tail_node(caller_node, pc);
    if (tail_node != Val_unit) {
      /* This tail calling sequence has happened before; just fill the hole
         with the existing node and return. */
      *node_hole = tail_node;
debug_printf("tail calling sequence has happened before; node=%p\n",(void*)tail_node);
      return 0;  /* indicates an existing node was returned */
    }
else {
debug_printf("tail calling sequence has not happened before\n");
}
  }

  node = allocate_uninitialized_ocaml_node(size_including_header);
  Hd_val(node) =
    Make_header(size_including_header - 1, OCaml_node_tag, Caml_black);
  assert((((uintnat) pc) % 1) == 0);
  Node_pc(node) = Encode_node_pc(pc);
  /* If the callee was tail called, then the tail link field will link this
     new node into an existing tail chain.  Otherwise, it is initialized with
     the empty tail chain, i.e. the one pointing directly at [node]. */
  if (caller_node == Val_unit) {
    Tail_link(node) = node;
  }
  else {
debug_printf("doing tail link\n");
    Tail_link(node) = Tail_link(caller_node);
    Tail_link(caller_node) = node;
  }

  /* The callee node pointers for direct tail call points are
     initialized from code emitted by the OCaml compiler.  This is done to
     avoid having to pass this function a description of which nodes are
     direct tail call points.  (We cannot just count them and put them at the
     beginning of the node because we need the indexes of elements within the
     node during instruction selection before we have found all call points.)
     This is now also used for assertion checking in
     [caml_spacetime_caml_garbage_collection].
  */

  for (field = Node_num_header_words; field < size_including_header - 1;
       field++) {
    Field(node, field) = Val_unit;
  }

  *node_hole = node;

  return 1;  /* indicates a new node was created */
}

static c_node* allocate_c_node(void)
{
  c_node* node;

  node = (c_node*) malloc(sizeof(c_node));
  if (!node) {
    abort();
  }

  assert((sizeof(c_node) % sizeof(uintnat)) == 0);
  node->gc_header =
    Make_header(sizeof(c_node)/sizeof(uintnat) - 1, C_node_tag, Caml_black);
  node->data.callee_node = Val_unit;
  node->next = Val_unit;

debug_printf("allocate_c_node returns %p\n", (void*) node);

  return node;
}

CAMLprim value* caml_spacetime_indirect_node_hole_ptr
      (void* callee, value* node_hole, value caller_node)
{
  /* Find the address of the node hole for an indirect call to [callee].
     If [caller_node] is not [Val_unit], it is a pointer to the caller's
     node, and indicates that this is a tail call site. */

  c_node* c_node;
  int found = 0;
/*
  debug_printf("caml_spacetime_indirect_node_hole_ptr: node hole=%p on entry\n",  (void*) node_hole);

*/

  /* On entry, the node hole pointer is over the call site address slot,
     so we must advance it to reach the linked list slot. */
  node_hole++;
/*
  debug_printf("indirect node hole ptr for callee %p starting at %p contains %p\n",
    callee, (void*) node_hole, *(void**) node_hole);
*/
  while (!found && *node_hole != Val_unit) {
/*
    debug_printf("loop iteration; *node_hole=%p\n", *(void**) node_hole);
*/
    assert(((uintnat) *node_hole) % sizeof(value) == 0);
    c_node = caml_spacetime_c_node_of_stored_pointer(*node_hole);
    assert(c_node != NULL);
    switch (caml_spacetime_classify_c_node(c_node)) {
      case CALL:
        if (pc_inside_c_node_matches(c_node, callee)) {
          found = 1;
        }
        else {
          node_hole = &c_node->next;
        }
        break;

      case ALLOCATION:
        fprintf(stderr, "Node at %p wrongly marked as ALLOCATION\n", c_node);
        abort();

      default:
        assert(0);
    }
  }

  if (!found) {
    assert(*node_hole == Val_unit);
    c_node = allocate_c_node();
    c_node->pc = Encode_c_node_pc_for_call(callee);

    if (caller_node != Val_unit) {
      /* This is a tail call site.
         Perform the initialization equivalent to that emitted by
         [Spacetime.code_for_function_prologue] for direct tail call
         sites. */

      c_node->data.callee_node = Encode_tail_caller_node(caller_node);
    }

    *node_hole = caml_spacetime_stored_pointer_of_c_node(c_node);
    assert(((uintnat) *node_hole) % sizeof(value) == 0);
  }

  assert(*node_hole != Val_unit);
/*
  debug_printf("indirect node hole ptr for callee %p starting at %p is %p\n",
    callee, (void*) node_hole,
    (void*) &(c_node->data.callee_node));
*/
  return &(c_node->data.callee_node);
}

/* Some notes on why caml_call_gc doesn't need a distinguished node.
   (Remember that thread switches are irrelevant here because each thread
   has its own trie.)

   caml_call_gc only invokes OCaml functions in the following circumstances:
   1. running an OCaml finaliser;
   2. executing an OCaml signal handler.
   Both of these are done on the finaliser trie.  Furthermore, both of
   these invocations start via caml_callback; the code in this file for
   handling that (caml_spacetime_c_to_ocaml) correctly copes with that by
   attaching a single "caml_start_program" node that can cope with any
   number of indirect OCaml calls from that point.

   caml_call_gc may also invoke C functions that cause allocation.  All of
   these (assuming libunwind support is present) will cause a chain of
   c_node structures to be attached to the trie, starting at the node hole
   passed to caml_call_gc from OCaml code.  These structures are extensible
   and can thus accommodate any number of C backtraces leading from
   caml_call_gc.
*/

/* CR mshinwell: check caml_stash_backtrace */

static void* find_trie_node_from_libunwind(int for_allocation)
{
#ifdef HAS_LIBUNWIND
  /* Given that [caml_last_return_address] is the most recent call site in
     OCaml code, and that we are now in C (or other) code called from that
     site, obtain a backtrace using libunwind and graft the most recent
     portion (everything back to but not including [caml_last_return_address])
     onto the trie.  See the important comment below regarding the fact that
     call site, and not callee, addresses are recorded during this process.

     If [for_allocation] is non-zero, the final node recorded will be for
     an allocation, and the returned pointer is to the allocation node.
     Otherwise, no node is recorded for the innermost frame, and the
     returned pointer is a pointer to the *node hole* where a node for that
     frame should be attached.
  */

  unw_cursor_t cur;
  unw_context_t ctx;
  int ret;
  int stop;
  int innermost_frame;
  int frame;
  struct ext_table frames;
  value* node_hole;
  c_node* node = NULL;

  caml_ext_table_init(&frames, 42);

  unw_getcontext(&ctx);
  unw_init_local(&cur, &ctx);

  stop = 0;
  while (!stop && (ret = unw_step(&cur)) > 0) {
    unw_word_t ip;
    unw_get_reg(&cur, UNW_REG_IP, &ip);
    if (caml_last_return_address == (uintnat) ip) {
      stop = 1;
    }
    else {
      caml_ext_table_add(&frames, (void*) ip);
      debug_printf("pc=%p\n", (void*)ip);
    }
  }

  node_hole = caml_spacetime_trie_node_ptr;
  debug_printf("*** find_trie_node_from_libunwind: starting at %p\n",
    (void*) *node_hole);
  /* Note that if [node_hole] is filled, then it must point to a C node,
     since it is not possible for there to be a call point in an OCaml
     function that sometimes calls C and sometimes calls OCaml. */

  /* We always need to ignore the frames for:
      #0  find_trie_node_from_libunwind
      #1  graft_c_backtrace_onto_trie
      #2  caml_spacetime_c_to_ocaml
     Further, if this is not an allocation point, we should not create the
     node for the current C function that triggered us (i.e. frame #3). */
  innermost_frame = for_allocation ? 2 : 3;

  /* CR mshinwell: we need to do something if there aren't enough frames
     from libunwind.  This will dereference NULL lower down at the moment
     if it's an allocation. */
  if (frames.size - 1 < innermost_frame) {
    fprintf(stderr, "*** insufficiently many frames from libunwind\n");
  }

  for (frame = frames.size - 1; frame >= innermost_frame; frame--) {
    c_node_type expected_type;
    void* pc = frames.contents[frame];
    assert (pc != (void*) caml_last_return_address);

    debug_printf("frames.contents[%d]=%p\n", frame, pc);

    if (!for_allocation) {
      expected_type = CALL;
    }
    else {
      expected_type = (frame > innermost_frame ? CALL : ALLOCATION);
    }

    if (*node_hole == Val_unit) {
      node = allocate_c_node();
      debug_printf("making new node %p\n", node);
      /* Note: for CALL nodes, the PC is the program counter at each call
         site.  We do not store program counter addresses of the start of
         callees, unlike for OCaml nodes.  This means that some trie nodes
         will become conflated.  These can be split during post-processing by
         working out which function each call site was in. */
      node->pc = (expected_type == CALL ? Encode_c_node_pc_for_call(pc)
        : Encode_c_node_pc_for_alloc_point(pc));
      *node_hole = caml_spacetime_stored_pointer_of_c_node(node);
    }
    else {
      c_node* prev;
      int found = 0;

      node = caml_spacetime_c_node_of_stored_pointer_not_null(*node_hole);
      debug_printf("using existing node %p (size %lld)\n", (void*) node,
        (unsigned long long) Wosize_val(*node_hole));
      assert(node != NULL);
      assert(node->next == Val_unit
        || (((uintnat) (node->next)) % sizeof(value) == 0));

      prev = NULL;

      while (!found && node != NULL) {
        debug_printf("...linked list entry pc=%p: ", (void*) node->pc);
        if (caml_spacetime_classify_c_node(node) == expected_type
            && pc_inside_c_node_matches(node, pc)) {
          debug_printf("found\n");
          found = 1;
        }
        else {
          debug_printf("doesn't match\n");
          prev = node;
          node = caml_spacetime_c_node_of_stored_pointer(node->next);
        }
      }
      if (!found) {
        assert(prev != NULL);
        node = allocate_c_node();
        node->pc = (expected_type == CALL ? Encode_c_node_pc_for_call(pc)
          : Encode_c_node_pc_for_alloc_point(pc));
        prev->next = caml_spacetime_stored_pointer_of_c_node(node);
      }
    }

    assert(node != NULL);

    assert(caml_spacetime_classify_c_node(node) == expected_type);
    assert(pc_inside_c_node_matches(node, pc));
    node_hole = &node->data.callee_node;

    debug_printf("find_trie_node, frame=%d, ra=%p\n", frame, pc);
  }

  if (for_allocation) {
    assert(caml_spacetime_classify_c_node(node) == ALLOCATION);
    assert(caml_spacetime_c_node_of_stored_pointer(node->next) != node);
  }

  assert(node->next != (value) NULL);

  debug_printf("find_trie_node_from_libunwind returns %p\n",
    for_allocation ? (void*) node : (void*) node_hole);

  return for_allocation ? (void*) node : (void*) node_hole;
#else
  return NULL;
#endif
}

static c_node* graft_backtrace_onto_trie_for_allocation(void)
{
  return (c_node*) find_trie_node_from_libunwind(1);
}

static void graft_c_backtrace_onto_trie(void)
{
  /* Update the trie with the current backtrace, as far back as
     [caml_last_return_address], and leave the node hole pointer at
     the correct place for attachment of a [caml_start_program] node. */

#ifdef HAS_LIBUNWIND
  /* CR mshinwell: handle NULL return */
  caml_spacetime_trie_node_ptr
    = (value*) find_trie_node_from_libunwind(0);
#endif
}

void caml_spacetime_c_to_ocaml(void* ocaml_entry_point,
      void* identifying_pc_for_caml_start_program)
{
  /* Called in [caml_start_program] and [caml_callback*] when we are about
     to cross from C into OCaml.  [ocaml_entry_point] is the branch target.
     This situation is handled by ensuring the presence of a new OCaml node
     for the callback veneer; the node contains a single indirect call point
     which accumulates the [ocaml_entry_point]s. */

  value node;

  debug_printf("c_to_ocaml for ocaml callee at %p, c_s_p identifying pc=%p\n",
    ocaml_entry_point, identifying_pc_for_caml_start_program);
  fflush(stdout);

  graft_c_backtrace_onto_trie();

  if (*caml_spacetime_trie_node_ptr == Val_unit) {
    uintnat size_including_header;

    size_including_header =
      1 /* GC header */ + Node_num_header_words + Indirect_num_fields;

    node = allocate_uninitialized_ocaml_node(size_including_header);
    Hd_val(node) =
      Make_header(size_including_header - 1, OCaml_node_tag, Caml_black);
    assert((((uintnat) identifying_pc_for_caml_start_program) % 1) == 0);
    Node_pc(node) = Encode_node_pc(identifying_pc_for_caml_start_program);
    Tail_link(node) = node;
    Indirect_pc_call_site(node, Node_num_header_words) =
      Encode_call_point_pc(identifying_pc_for_caml_start_program);
    Indirect_pc_linked_list(node, Node_num_header_words) = Val_unit;
    *caml_spacetime_trie_node_ptr = node;
  }
  else {
    node = *caml_spacetime_trie_node_ptr;
    /* If there is a node here already, it should never be an initialized
       (but as yet unused) tail call point, since calls from OCaml into C
       are never tail calls (and no C -> C call is marked as tail). */
    assert(!Is_tail_caller_node_encoded(node));
  }

  assert(Is_ocaml_node(node));
  if (Decode_node_pc(Node_pc(node)) != identifying_pc_for_caml_start_program) {
    printf("Indirect node for C -> OCaml has wrong PC %p (expected %p)\n",
      Decode_node_pc(Node_pc(node)),
      identifying_pc_for_caml_start_program);
  }
  assert(Decode_node_pc(Node_pc(node))
    == identifying_pc_for_caml_start_program);
  assert(Tail_link(node) == node);
  assert(Wosize_val(node) == Node_num_header_words + Indirect_num_fields);

  /* Search the node to find the node hole corresponding to the indirect
     call to the OCaml function. */
  caml_spacetime_trie_node_ptr =
    caml_spacetime_indirect_node_hole_ptr(
      ocaml_entry_point,
      &Indirect_pc_call_site(node, Node_num_header_words),
      Val_unit);
  assert(*caml_spacetime_trie_node_ptr == Val_unit
    || Is_ocaml_node(*caml_spacetime_trie_node_ptr));

  debug_printf("c_to_ocaml has moved the node ptr to %p\n",
    (void*) caml_spacetime_trie_node_ptr);
  fflush(stdout);
}

extern void caml_garbage_collection(void);  /* signals_asm.c */

void caml_spacetime_caml_garbage_collection(void)
{
  /* Called upon entry to [caml_garbage_collection].
     Since [caml_call_gc] points cannot easily be instrumented by
     [Spacetime_profiling] (the calls are created too late in the
     compiler pipeline), we have to manually find the correct place in the
     current OCaml node before [caml_garbage_collection] can continue.

     On entry we expect:
     1. [caml_allocation_trie_node_ptr] to point to the first of the three
        fields of a direct call point inside an OCaml node (which is
        arranged by code in asmcomp/amd64/emit.mlp); and
     2. [caml_gc_regs] to point at the usual GC register array on the stack
        with the return address immediately after (at a higher address) than
        such array.
  */

  value call_site;
  value encoded_caml_garbage_collection;
  value node;

  /* We need to skip over 13 registers to fish out our return address.
     See asmrun/amd64.S:caml_call_gc. */
  call_site = Encode_call_point_pc(*(((uint64_t*) caml_gc_regs) + 13));

  /* See point 1 above. */
  node = (value) caml_spacetime_trie_node_ptr;

  /* The callee is constant. */
  encoded_caml_garbage_collection =
    Encode_call_point_pc(&caml_garbage_collection);

  /* If the call site and callee have been set, we don't check whether there
     is any child node, since it's possible there might not actually be one
     (e.g. if no allocation or callbacks performed in
     [caml_garbage_collection]). */
  assert((Direct_pc_call_site(node, 0) == Val_unit
      && Direct_pc_callee(node, 0) == Val_unit
      && Direct_callee_node(node, 0) == Val_unit)
    || (Direct_pc_call_site(node, 0) == call_site
      && Direct_pc_callee(node, 0) == encoded_caml_garbage_collection));

  /* Initialize the direct call point within the node. */
  Direct_pc_call_site(node, 0) = call_site;
  Direct_pc_callee(node, 0) = encoded_caml_garbage_collection;

  /* Set the trie node hole pointer correctly so that when e.g. an
     allocation occurs from within [caml_garbage_collection] the resulting
     nodes are attached correctly. */
  caml_spacetime_trie_node_ptr = &Direct_callee_node(node, 0);
}

static uintnat generate_profinfo(void)
{
  uintnat profinfo;

  if (caml_spacetime_use_override_profinfo == Val_true) {
    return caml_spacetime_override_profinfo;
  }

  profinfo = caml_spacetime_profinfo++;
  if (caml_spacetime_profinfo > PROFINFO_MASK) {
    /* Profiling counter overflow. */
    profinfo = profinfo_overflow;
  }

  return profinfo;
}

CAMLprim uintnat caml_spacetime_generate_profinfo (void* pc,
    void* profinfo_words)
{
  value node;
  uintnat offset;
  uintnat profinfo = generate_profinfo();

  /* [node] isn't really a node; it points into the middle of
     one---specifically to the "profinfo" word of an allocation point pair of
     words  It's done like this to avoid re-calculating the place in the node
     (which already has to be done in the OCaml-generated code run before
     this function). */
  node = (value) (((uintnat*) profinfo_words) - 1);
  offset = 0;

  assert(Alloc_point_pc(node, offset) == Val_unit);
  assert(Alloc_point_profinfo(node, offset) == Val_unit);

  Alloc_point_pc(node, offset) = Encode_alloc_point_pc(pc);
  Alloc_point_profinfo(node, offset) = Encode_alloc_point_profinfo(profinfo);

  debug_printf("*** generate_profinfo PC=%p returning 0x%llx\n", pc,
    (unsigned long long) profinfo);
  fflush(stdout);

  return profinfo << PROFINFO_SHIFT;
}

uintnat caml_spacetime_my_profinfo (void)
{
  /* Return the profinfo value that should be written into a value's header
     during an allocation from C.  This may necessitate extending the trie
     with information obtained from libunwind. */

  c_node* node;
  uint64_t profinfo;

  node = graft_backtrace_onto_trie_for_allocation ();
  profinfo = generate_profinfo();
  if (node != NULL) {
    node->data.profinfo = Val_long(profinfo);
  }

  assert(profinfo <= PROFINFO_MASK);
  return profinfo;  /* N.B. not shifted by PROFINFO_SHIFT */
}

/* List of tries corresponding to threads that have been created. */

/* CR-soon mshinwell: just include the main trie in this list. */

typedef struct per_thread {
  value* trie_node_root;
  value* finaliser_trie_node_root;
  struct per_thread* next;
} per_thread;

static per_thread* per_threads = NULL;
static int num_per_threads = 0;

void caml_spacetime_register_thread(
  value* trie_node_root, value* finaliser_trie_node_root)
{
  per_thread* thr;

  thr = (per_thread*) malloc(sizeof(per_thread));
  if (thr == NULL) {
    fprintf(stderr, "Out of memory while registering thread for profiling\n");
    abort();
  }
  thr->next = per_threads;
  per_threads = thr;

  thr->trie_node_root = trie_node_root;
  thr->finaliser_trie_node_root = finaliser_trie_node_root;

  /* CR mshinwell: record thread ID (and for the main thread too) */

  num_per_threads++;
}

CAMLprim value caml_spacetime_marshal_trie (value v_channel)
{
  /* Marshal both the main and finaliser tries, for all threads that have
     been created, to an [out_channel].  This can be done by using the
     extern.c code as usual, since the trie looks like standard OCaml values;
     but we must allow it to traverse outside the heap. */

  int num_marshalled = 0;
  per_thread* thr = per_threads;

  caml_output_value(v_channel, Val_long(num_per_threads + 1), Val_long(0));

  caml_extern_allow_out_of_heap = 1;
  caml_output_value(v_channel, caml_spacetime_trie_root, Val_long(0));
  caml_output_value(v_channel,
    caml_spacetime_finaliser_trie_root_main_thread, Val_long(0));
  while (thr != NULL) {
    caml_output_value(v_channel, *(thr->trie_node_root), Val_long(0));
    caml_output_value(v_channel, *(thr->finaliser_trie_node_root),
      Val_long(0));
    thr = thr->next;
    num_marshalled++;
  }
  caml_extern_allow_out_of_heap = 0;
  assert(num_marshalled == num_per_threads);

  return Val_unit;
}

static void print_tail_chain(value node)
{
  value starting_node;

  assert(Is_ocaml_node(node));
  starting_node = node;

  if (Tail_link(node) == node) {
    printf("Tail chain is empty.\n");
  }
  else {
    printf("Tail chain:\n");
    do {
      node = Tail_link(node);
      printf("  Node %p (identifying PC=%p)\n", (void*) node,
        Decode_node_pc(Node_pc(node)));
    } while (node != starting_node);
  }
}

static void print_node_header(value node)
{
  uintnat index;

  if (node == Val_unit) {
    printf("(Uninitialized node)\n");
  }
  else {
    printf("Node %p: tag %d, size %d\n",
      (void*) node, Tag_val(node), (int) Wosize_val(node));
    assert(Tag_val(node) == 0 || Tag_val(node) == 1);
    if (Is_ocaml_node(node)) {
      printf("Identifying PC=%p\n", Decode_node_pc(Node_pc(node)));
      print_tail_chain(node);
    }

    /* Sanity check: there should never be actual NULL pointers in these
       values.  [Val_unit] is used instead, so we can marshal. */
    for (index = 0; index < Wosize_val(node); index++) {
      assert(Field(node, index) != (value) 0);
    }
  }
}

static void print_trie_node(value node, int inside_indirect_node)
{
  print_node_header(node);
  if (node == Val_unit) {
    return;
  }

  if (Color_val(node) != Caml_black) {
    printf("Node %p visited before\n", (void*) node);
  }
  else {
    int field;
    int alloc_point;
    int direct_call_point;
    int indirect_call_point;

    alloc_point = 0;
    direct_call_point = 0;
    indirect_call_point = 0;

    Hd_val(node) = Whitehd_hd(Hd_val(node));

    /* CR mshinwell: remove some of the hard-coded offsets below and use the
       macros */

    if (Is_ocaml_node(node)) {
      for (field = Node_num_header_words; field < Wosize_val(node); field++) {
        value entry;

        entry = Field(node, field);

        /* Even though indirect call points have a different size from
           direct call points and allocation points, it is still safe to just
           skip until we don't see [Val_unit] any more. */
        if (entry == Val_unit || entry == (value) 3) {
          continue;
        }

        /* We may now be in the middle of an uninitialized direct call point
           for a tail call.  This can be detected by seeing if the pointer
           is an encoded pointer to the current node. */
        if (entry == Encode_tail_caller_node(node)) {
          /* The pointer should be the third in a group of three words. */
          assert (field >= Node_num_header_words + 2);
          printf("(Reached uninitialized tail call point.)\n");
          continue;
        }

        /* At this point we should have an encoded program counter value.
           First distinguish between:
           (a) a direct or an indirect call point;
           (b) an allocation point.
        */
        switch (Call_or_allocation_point(node, field)) {
          case CALL: {
            /* Determine whether this is a direct or an indirect call
               point by examining the second word in the group.  This will be
               an immediate encoded PC value for a direct call point, but a
               pointer for an indirect call point.  It should never be
               [Val_unit] in either case. */
            value second_word;
            assert(field < Wosize_val(node) - 1);
            /* CR mshinwell: this assumes that the list coincides with
               the callee slot... */
            second_word = Indirect_pc_linked_list(node, field);
            assert(second_word != Val_unit);
            /* CR mshinwell: consider using a macro */
            if (Is_block(second_word)) {
              /* This is an indirect call point. */
              int i = indirect_call_point;
              printf("Indirect call point %d: %p calls...\n",
                indirect_call_point,
                Decode_call_point_pc(Indirect_pc_call_site(node, field)));
              assert(!Is_ocaml_node(second_word));
              print_trie_node(second_word, 1);
              field++;
              printf("Indirect call point %d ends.\n", i);
            }
            else {
              /* This is a direct call point. */
              value child;
              int i = direct_call_point;
              assert(field < Wosize_val(node) - 2);
              child = Direct_callee_node(node, field);
              printf("Direct call point %d: %p calls %p, ",
                direct_call_point,
                Decode_call_point_pc(Direct_pc_call_site(node, field)),
                Decode_call_point_pc(Direct_pc_callee(node, field)));
              if (child == Val_unit) {
                printf("callee was not instrumented\n");
              } else if (Is_tail_caller_node_encoded(child)) {
                /* XXX not sure this should happen, since it's always
                   OCaml -> OCaml */
                printf("tail-called callee was not instrumented\n");
              } else {
                printf("child node=%p\n", (void*) child);
              }
              direct_call_point++;
              if (child != Val_unit
                  && !Is_tail_caller_node_encoded(child)) {
                print_trie_node(child, 0);
              }
              field += 2;
              printf("Direct call point %d ends\n", i);
            }
            break;
          }

          case ALLOCATION:
            assert(field < Wosize_val(node) - 1);
            printf("Allocation point %d: pc=%p, profinfo=%lld\n", alloc_point,
              Decode_alloc_point_pc(Alloc_point_pc(node, field)),
              (unsigned long long)
                Decode_alloc_point_profinfo(Alloc_point_profinfo(node, field)));
            alloc_point++;
            field++;
            break;

          default:
            assert(0);
        }
      }
    } else {
      c_node* c_node = caml_spacetime_c_node_of_stored_pointer(node);
      assert (c_node != NULL);
      while (c_node != NULL) {
        assert(c_node->next != (value) 0);
        printf("(Debug: about to classify node %p)\n", (void*) c_node);
        switch (caml_spacetime_classify_c_node(c_node)) {
          case CALL:
            printf("%s %p: child node=%p\n",
              inside_indirect_node ? "..." : "Call site in non-OCaml code ",
              (void*) (c_node->pc >> 2),
              (void*) c_node->data.callee_node);
            if (Is_tail_caller_node_encoded(c_node->data.callee_node)) {
              printf("(Unused indirect tail point--uninstrumented callee)\n");
            }
            else {
              print_trie_node(c_node->data.callee_node, 0);
            }
            break;

          case ALLOCATION:
            assert(!Is_block(c_node->data.profinfo));
            printf("Allocation point in non-OCaml code at %p: profinfo=%lld\n",
              (void*) (c_node->pc >> 2),
              (unsigned long long) Long_val(c_node->data.profinfo));
            break;

          default:
            abort();
        }
        printf("(Debug: before 'about to classify node %p' with %p)\n",
          (void*) (caml_spacetime_c_node_of_stored_pointer(c_node->next)),
          (void*) c_node);
        c_node = caml_spacetime_c_node_of_stored_pointer(c_node->next);
      }
    }
    printf("End of node %p\n", (void*) node);
  }
}

static void mark_trie_node_black(value node)
{
  int field;

  if (node == Val_unit) {
    return;
  }

  if (Color_val(node) == Caml_black) {
    return;
  }
  Hd_val(node) = Blackhd_hd(Hd_val(node));

  if (Is_ocaml_node(node)) {
    for (field = Node_num_header_words; field < Wosize_val(node); field++) {
      value entry;

      entry = Field(node, field);

      if (entry == Val_unit || entry == (value) 3) {
        continue;
      }

      if (entry == Encode_tail_caller_node(node)) {
        continue;
      }

      switch (Call_or_allocation_point(node, field)) {
        case CALL: {
          value second_word;
          second_word = Indirect_pc_linked_list(node, field);
          if (Is_block(second_word)) {
            assert(!Is_ocaml_node(second_word));
            mark_trie_node_black(second_word);
            field++;
          }
          else {
            value child;
            child = Direct_callee_node(node, field);
            if (child != Val_unit) {
              mark_trie_node_black(child);
            }
            field += 2;
          }
          break;
        }

        case ALLOCATION:
          field++;
          break;

        default:
          assert(0);
      }
    }
  } else {
    c_node* c_node = caml_spacetime_c_node_of_stored_pointer(node);
    assert (c_node != NULL);
    while (c_node != NULL) {
      switch (caml_spacetime_classify_c_node(c_node)) {
        case CALL:
          if (!Is_tail_caller_node_encoded(c_node->data.callee_node)) {
            mark_trie_node_black(c_node->data.callee_node);
          }
          break;

        default:
          break;
      }
      c_node = caml_spacetime_c_node_of_stored_pointer(c_node->next);
    }
  }
}

CAMLprim value caml_spacetime_debug(value v_unit)
{
  value trie_node = caml_spacetime_trie_root;

  printf("---------------------------------------------------------------\n");
  if (trie_node == Val_unit) {
    printf("Spacetime trie is empty\n");
  }
  else {
    print_trie_node(trie_node, 0);
    printf("End of trie dump.  Marking trie black.\n");
    mark_trie_node_black(trie_node);
    printf("Done.\n");
  }
  printf("---------------------------------------------------------------\n");

  fflush(stdout);

  return Val_unit;
}

#else

CAMLprim value caml_spacetime_take_heap_snapshot()
{
  caml_failwith("Spacetime profiling not enabled");
  assert(0);  /* unreachable */
}

CAMLprim value caml_spacetime_marshal_heap_snapshot()
{
  caml_failwith("Spacetime profiling not enabled");
  assert(0);  /* unreachable */
}

CAMLprim value caml_spacetime_free_heap_snapshot()
{
  caml_failwith("Spacetime profiling not enabled");
  assert(0);  /* unreachable */
}

CAMLprim value caml_spacetime_get_profinfo()
{
  caml_failwith("Spacetime profiling not enabled");
  assert(0);  /* unreachable */
}

CAMLprim value caml_spacetime_profinfo_none()
{
  caml_failwith("Spacetime profiling not enabled");
  assert(0);  /* unreachable */
}

CAMLprim value caml_spacetime_debug()
{
  caml_failwith("Spacetime profiling not enabled");
  assert(0);  /* unreachable */
}

CAMLprim value
caml_forget_where_values_were_allocated ()
{
  caml_failwith("Spacetime profiling not enabled");
  assert(0);  /* unreachable */
}

CAMLprim value
caml_spacetime_num_frame_descriptors ()
{
  caml_failwith("Spacetime profiling not enabled");
  assert(0);  /* unreachable */
}

CAMLprim value
caml_spacetime_get_frame_descriptor ()
{
  caml_failwith("Spacetime profiling not enabled");
  assert(0);  /* unreachable */
}

CAMLprim value
caml_spacetime_return_address_of_frame_descriptor ()
{
  caml_failwith("Spacetime profiling not enabled");
  assert(0);  /* unreachable */
}

CAMLprim value
caml_spacetime_marshal_trie ()
{
  caml_failwith("Spacetime profiling not enabled");
  assert(0);  /* unreachable */
}

#endif
