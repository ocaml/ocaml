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

/* CR mshinwell: remove pragma and rename assert -> Assert */

#pragma GCC optimize ("O0")

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <math.h>

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

/* We force "noinline" in certain places to be sure we know how many
   frames there will be on the stack. */
#define NOINLINE __attribute__((noinline))

#ifdef HAS_LIBUNWIND
#include "libunwind.h"
#endif

extern value caml_spacetime_debug(value);

typedef struct per_thread {
  value* trie_node_root;
  value* finaliser_trie_node_root;
  struct per_thread* next;
} per_thread;

/* List of tries corresponding to threads that have been created. */
/* CR-soon mshinwell: just include the main trie in this list. */
static per_thread* per_threads = NULL;
static int num_per_threads = 0;

static uintnat caml_spacetime_profinfo = (uintnat) 0;

static value caml_spacetime_trie_root = Val_unit;
value* caml_spacetime_trie_node_ptr = &caml_spacetime_trie_root;

static value caml_spacetime_finaliser_trie_root_main_thread = Val_unit;
value* caml_spacetime_finaliser_trie_root
  = &caml_spacetime_finaliser_trie_root_main_thread;

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

void caml_spacetime_register_dynamic_library(
  const char* filename, void* address_of_code_begin)
{
  /* CR mshinwell: implement this */
}

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

  starting_node = node;
  pc = Encode_node_pc(callee);

  do {
    assert(Is_ocaml_node(node));
    if (Node_pc(node) == pc) {
      found = node;
    }
    else {
      node = Tail_link(node);
    }
  } while (found == Val_unit && starting_node != node);

  return found;
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
    caller_node = Decode_tail_caller_node(node);
    tail_node = find_tail_node(caller_node, pc);
    if (tail_node != Val_unit) {
      /* This tail calling sequence has happened before; just fill the hole
         with the existing node and return. */
      *node_hole = tail_node;
      return 0;  /* indicates an existing node was returned */
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

  /* CR-soon mshinwell: consider using a different allocator */
  node = (c_node*) malloc(sizeof(c_node));
  if (!node) {
    abort();
  }

  assert((sizeof(c_node) % sizeof(uintnat)) == 0);
  node->gc_header =
    Make_header(sizeof(c_node)/sizeof(uintnat) - 1, C_node_tag, Caml_black);
  node->data.callee_node = Val_unit;
  node->next = Val_unit;

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

  /* On entry, the node hole pointer is over the call site address slot,
     so we must advance it to reach the linked list slot. */
  node_hole++;

  while (!found && *node_hole != Val_unit) {
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

static NOINLINE void* find_trie_node_from_libunwind(int for_allocation)
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

  ret = unw_getcontext(&ctx);
  if (ret != UNW_ESUCCESS) {
    return NULL;
  }

  ret = unw_init_local(&cur, &ctx);
  if (ret != UNW_ESUCCESS) {
    return NULL;
  }

  stop = 0;
  while (!stop && (ret = unw_step(&cur)) > 0) {
    unw_word_t ip;
    unw_get_reg(&cur, UNW_REG_IP, &ip);
    if (caml_last_return_address == (uintnat) ip) {
      stop = 1;
    }
    else {
      caml_ext_table_add(&frames, (void*) ip);
    }
  }

  /* We always need to ignore the frames for:
      #0  find_trie_node_from_libunwind
      #1  graft_c_backtrace_onto_trie
      #2  caml_spacetime_c_to_ocaml
     Further, if this is not an allocation point, we should not create the
     node for the current C function that triggered us (i.e. frame #3). */
  innermost_frame = for_allocation ? 2 : 3;

  if (frames.size - 1 < innermost_frame) {
    /* Insufficiently many frames (maybe no frames) returned from
       libunwind; just don't do anything. */
    return NULL;
  }

  node_hole = caml_spacetime_trie_node_ptr;
  /* Note that if [node_hole] is filled, then it must point to a C node,
     since it is not possible for there to be a call point in an OCaml
     function that sometimes calls C and sometimes calls OCaml. */

  for (frame = frames.size - 1; frame >= innermost_frame; frame--) {
    c_node_type expected_type;
    void* pc = frames.contents[frame];
    assert (pc != (void*) caml_last_return_address);

    if (!for_allocation) {
      expected_type = CALL;
    }
    else {
      expected_type = (frame > innermost_frame ? CALL : ALLOCATION);
    }

    if (*node_hole == Val_unit) {
      node = allocate_c_node();
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
      assert(node != NULL);
      assert(node->next == Val_unit
        || (((uintnat) (node->next)) % sizeof(value) == 0));

      prev = NULL;

      while (!found && node != NULL) {
        if (caml_spacetime_classify_c_node(node) == expected_type
            && pc_inside_c_node_matches(node, pc)) {
          found = 1;
        }
        else {
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
  }

  if (for_allocation) {
    assert(caml_spacetime_classify_c_node(node) == ALLOCATION);
    assert(caml_spacetime_c_node_of_stored_pointer(node->next) != node);
  }

  assert(node->next != (value) NULL);

  return for_allocation ? (void*) node : (void*) node_hole;
#else
  return NULL;
#endif
}

static NOINLINE c_node* graft_backtrace_onto_trie_for_allocation(void)
{
  return (c_node*) find_trie_node_from_libunwind(1);
}

static NOINLINE void graft_c_backtrace_onto_trie(void)
{
  /* Update the trie with the current backtrace, as far back as
     [caml_last_return_address], and leave the node hole pointer at
     the correct place for attachment of a [caml_start_program] node. */

#ifdef HAS_LIBUNWIND
  value* node;

  node = (value*) find_trie_node_from_libunwind(0);
  if (node != NULL) {
    caml_spacetime_trie_node_ptr = node;
  }
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
}

extern void caml_garbage_collection(void);  /* signals_asm.c */
extern void caml_array_bound_error(void);  /* fail.c */

static void ocaml_to_c_call_site_without_instrumentation(
  void* call_site_ptr,
  void(* callee_ptr)(void))
{
  /* See comment on [caml_spacetime_caml_garbage_collection] below. */

  value call_site;
  value callee;
  value node;

  /* We need to skip over 13 registers to fish out our return address.
     See asmrun/amd64.S:caml_call_gc. */
  call_site = Encode_call_point_pc(call_site_ptr);

  /* See point 1 above. */
  node = (value) caml_spacetime_trie_node_ptr;
  assert ((node % sizeof(value)) == 0);

  callee = Encode_call_point_pc(callee_ptr);

  /* If the call site and callee have been set, we don't check whether there
     is any child node, since it's possible there might not actually be one
     (e.g. if no allocation or callbacks performed in
     [caml_garbage_collection]). */
  assert((Direct_pc_call_site(node, 0) == Val_unit
      && Direct_pc_callee(node, 0) == Val_unit
      && Direct_callee_node(node, 0) == Val_unit)
    || (Direct_pc_call_site(node, 0) == call_site
      && Direct_pc_callee(node, 0) == callee));

  /* Initialize the direct call point within the node. */
  Direct_pc_call_site(node, 0) = call_site;
  Direct_pc_callee(node, 0) = callee;

  /* Set the trie node hole pointer correctly so that when e.g. an
     allocation occurs from within [caml_garbage_collection] the resulting
     nodes are attached correctly. */
  caml_spacetime_trie_node_ptr = &Direct_callee_node(node, 0);
}

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

  void* call_site;
  void(* callee)();

  /* We need to skip over 13 registers to fish out our return address.
     See asmrun/amd64.S:caml_call_gc. */
  /* CR mshinwell: this should be target-dependent */
  call_site = (void*) *(((uint64_t*) caml_gc_regs) + 13);
  callee = &caml_garbage_collection;

  ocaml_to_c_call_site_without_instrumentation(call_site, callee);
}

void caml_spacetime_caml_ml_array_bound_error(void)
{
  /* Like [caml_spacetime_caml_ml_array_bound_error], but for array bounds
     check failures.  This one is called via [caml_c_call] so we can use
     [caml_last_return_address] to find the original call site in OCaml
     code. */

  void* call_site;
  void(* callee)();

  /* CR mshinwell: it looks like caml_last_return_address won't be correct
     here, we need to look at the stack */

  call_site = (void*) caml_last_return_address;
  callee = &caml_array_bound_error;

  ocaml_to_c_call_site_without_instrumentation(call_site, callee);
}

CAMLprim uintnat caml_spacetime_generate_profinfo (void* pc,
    void* profinfo_words)
{
  /* Called from code that creates a value's header inside an OCaml
     function. */

  value node;
  uintnat offset;
  uintnat profinfo;

  caml_spacetime_profinfo++;
  if (caml_spacetime_profinfo > PROFINFO_MASK) {
    /* Profiling counter overflow. */
    caml_spacetime_profinfo = PROFINFO_MASK;
  }
  profinfo = caml_spacetime_profinfo;

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

  return profinfo << PROFINFO_SHIFT;
}

uintnat caml_spacetime_my_profinfo (void)
{
  /* Return the profinfo value that should be written into a value's header
     during an allocation from C.  This may necessitate extending the trie
     with information obtained from libunwind. */

  c_node* node;
  uint64_t profinfo;

  caml_spacetime_profinfo++;
  if (caml_spacetime_profinfo > PROFINFO_MASK) {
    /* Profiling counter overflow. */
    caml_spacetime_profinfo = PROFINFO_MASK;
  }
  profinfo = caml_spacetime_profinfo;

  node = graft_backtrace_onto_trie_for_allocation ();
  if (node != NULL) {
    node->data.profinfo = Val_long(profinfo);
  }

  assert(profinfo <= PROFINFO_MASK);
  return profinfo;  /* N.B. not shifted by PROFINFO_SHIFT */
}

#else

CAMLprim value
caml_spacetime_marshal_trie ()
{
  caml_failwith("Spacetime profiling not enabled");
  assert(0);  /* unreachable */
}

#endif
