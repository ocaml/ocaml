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

#ifndef CAML_SPACETIME_H
#define CAML_SPACETIME_H

#include "caml/io.h"

/* Runtime support for Spacetime profiling.
 * This header file is not intended for the casual user.
 *
 * The implementation is split into three files:
 *   1. spacetime.c: core management of the instrumentation;
 *   2. spacetime_snapshot.c: the taking of heap snapshots;
 *   3. spacetime_offline.c: functions that are also used when examining
 *      saved profiling data.
 */

typedef enum {
  CALL,
  ALLOCATION
} c_node_type;

/* All pointers between nodes point at the word immediately after the
   GC headers, and everything is traversable using the normal OCaml rules.

   On entry to an OCaml function:
   If the node hole pointer register has the bottom bit set, then the function
   is being tail called or called from a self-recursive call site:
   - If the node hole is empty, the callee must create a new node and link
     it into the tail chain.  The node hole pointer will point at the tail
     chain.
   - Otherwise the node should be used as normal.
   Otherwise (not a tail call):
   - If the node hole is empty, the callee must create a new node, but the
     tail chain is untouched.
   - Otherwise the node should be used as normal.
*/

/* Classification of nodes (OCaml or C) with corresponding GC tags. */
#define OCaml_node_tag 0
#define C_node_tag 1
#define Is_ocaml_node(node) (Is_block(node) && Tag_val(node) == OCaml_node_tag)
#define Is_c_node(node) (Is_block(node) && Tag_val(node) == C_node_tag)

/* The header words are:
   1. The node program counter.
   2. The tail link. */
#define Node_num_header_words 2

/* The "node program counter" at the start of an OCaml node. */
#define Node_pc(node) (Field(node, 0))
#define Encode_node_pc(pc) (((value) pc) | 1)
#define Decode_node_pc(encoded_pc) ((void*) (encoded_pc & ~1))

/* The circular linked list of tail-called functions within OCaml nodes. */
#define Tail_link(node) (Field(node, 1))

/* The convention for pointers from OCaml nodes to other nodes.  There are
   two special cases:
   1. [Val_unit] means "uninitialized", and further, that this is not a
      tail call point.  (Tail call points are pre-initialized, as in case 2.)
   2. If the bottom bit is set, and the value is not [Val_unit], this is a
      tail call point. */
#define Encode_tail_caller_node(node) ((node) | 1)
#define Decode_tail_caller_node(node) ((node) & ~1)
#define Is_tail_caller_node_encoded(node) (((node) & 1) == 1)

/* Allocation points within OCaml nodes.
   The "profinfo" value is stored shifted. */
#define Encode_alloc_point_profinfo(profinfo) (profinfo | 1)
#define Decode_alloc_point_profinfo(profinfo) (profinfo & ~((uintnat) 1))
#define Alloc_point_profinfo(node, offset) (Field(node, offset))

/* Direct call points (tail or non-tail) within OCaml nodes.
   They just hold a pointer to the child node.  The call site and callee are
   both recorded in the shape. */
#define Direct_num_fields 1
#define Direct_callee_node(node,offset) (Field(node, offset))
#define Encode_call_point_pc(pc) (((value) pc) | 1)
#define Decode_call_point_pc(pc) ((void*) (((value) pc) & ~((uintnat) 1)))

/* Indirect call points (tail or non-tail) within OCaml nodes.
   They hold a linked list of (PC upon entry to the callee, pointer to
   child node) pairs.  The linked list is encoded using C nodes and should
   be thought of as part of the OCaml node itself. */
#define Indirect_num_fields 1
#define Indirect_pc_linked_list(node,offset) (Field(node, offset))

/* Encodings of the program counter value within a C node. */
#define Encode_c_node_pc_for_call(pc) ((((value) pc) << 2) | 3)
#define Encode_c_node_pc_for_alloc_point(pc) ((((value) pc) << 2) | 1)
#define Decode_c_node_pc(pc) ((void*) ((pc) >> 2))

typedef struct {
  uintnat gc_header;
  uintnat pc;           /* always has bit 0 set.  Bit 1 set => CALL. */
  union {
    value callee_node;  /* for CALL */
    value profinfo;   /* for ALLOCATION (encoded with [Val_long])*/
  } data;
  value next;           /* [Val_unit] for the end of the list */
} c_node; /* CR mshinwell: rename to dynamic_node */

typedef struct shape_table {
  uint64_t* table;
  struct shape_table* next;
} shape_table;

extern uint64_t** caml_spacetime_static_shape_tables;
extern shape_table* caml_spacetime_dynamic_shape_tables;

typedef struct ext_table* spacetime_unwind_info_cache;

extern value* caml_spacetime_trie_node_ptr;
extern value* caml_spacetime_finaliser_trie_root;

extern void caml_spacetime_initialize(void);
extern uintnat caml_spacetime_my_profinfo(spacetime_unwind_info_cache*);
extern void caml_spacetime_register_dynamic_library(const char*, void*);
extern c_node_type caml_spacetime_classify_c_node(c_node* node);
extern c_node* caml_spacetime_c_node_of_stored_pointer(value);
extern c_node* caml_spacetime_c_node_of_stored_pointer_not_null(value);
extern value caml_spacetime_stored_pointer_of_c_node(c_node* node);
extern void caml_spacetime_register_thread(value*, value*);
extern void caml_spacetime_register_shapes(void*);
extern value caml_spacetime_timestamp(void);
extern value caml_spacetime_frame_table(void);
extern value caml_spacetime_shape_table(void);
extern void caml_spacetime_save_snapshot (struct channel *chan);
extern void caml_spacetime_automatic_snapshot(void);
extern void caml_spacetime_automatic_save(void);

#if defined(SYS_mingw64) || defined(SYS_cygwin)
#include <intrin.h>
#pragma intrinsic(_ReturnAddress)
#define DIRECTLY_CALLED_FROM_OCAML \
  (_ReturnAddress() == caml_last_return_address)
#else
#define DIRECTLY_CALLED_FROM_OCAML \
  (__builtin_return_address(0) == caml_last_return_address)
#endif

#endif
