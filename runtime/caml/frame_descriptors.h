/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                   Tom Kelly, OCaml Labs Consultancy                    */
/*                 Stephen Dolan, University of Cambridge                 */
/*                                                                        */
/*   Copyright 2019 Indian Institute of Technology, Madras                */
/*   Copyright 2021 OCaml Labs Consultancy Ltd                            */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_FRAME_DESCRIPTORS_H
#define CAML_FRAME_DESCRIPTORS_H

#ifdef CAML_INTERNALS

#include <stdbool.h>
#include "config.h"

/* The compiler generates a "frame descriptor" for every potential
 * return address. Each loaded module has a block of memory, the
 * "frame table", consisting of these frame descriptors
 * concatenated. Each frame descriptor includes:
 *
 * - frame_return_to_C(): Whether the return is to C from OCaml, in
 *   which case there is no actual stack frame, GC roots, allocation
 *   sizes, or debug info.  See caml_system$frametable in the various
 *   architecture-specific OCaml/C interfaces.
 *
 * - frame_size(): The stack frame size, in bytes. All stack frames
 *   are word-aligned so we also store information in the bottom two
 *   bits:
 *
 * - frame_has_allocs(): Whether it is the return address of a call
 *   into the garbage collector, and if so the sizes of all objects to
 *   be allocated by this call. Each size is stored reduced by 1, so
 *   that a single byte can record sizes (wosize) from 1 to 256 words.
 *
 * - frame_has_debug(): Whether we have debug information for this
 *   stack frame, and if so the "debuginfo" (source location) of this
 *   return address. (If frame_has_allocs(), this is an array of
 *   debuginfo, one for each of the set of allocations performed by
 *   this GC entry).
 *
 * - the register or stack frame offset of every "live" value,
 *   which should be scanned by the garbage collector if a GC is
 *   performed at this point.
 */

#define FRAME_DESCRIPTOR_DEBUG 1
#define FRAME_DESCRIPTOR_ALLOC 2
#define FRAME_DESCRIPTOR_FLAGS 3
#define FRAME_RETURN_TO_C 0xFFFF

typedef struct {
  uintnat retaddr;
  uint16_t frame_data; /* frame size and various flags */
  uint16_t num_live;
  uint16_t live_ofs[/* num_live */]; /* flexible array member */
  /*
    If frame_has_allocs(), alloc lengths follow:
        uint8_t num_allocs;
        uint8_t alloc[num_allocs];

    If frame_has_debug(), debug info follows (32-bit aligned):
        uint32_t debug_info[frame_has_allocs() ? num_allocs : 1];

    Debug info is stored as a relative offset, in bytes, from the
    debug_info itself to a debuginfo structure. */
} frame_descr;

Caml_inline bool frame_return_to_C(frame_descr *d) {
  return d->frame_data == 0xFFFF;
}

Caml_inline uint16_t frame_size(frame_descr *d) {
  return d->frame_data &~ FRAME_DESCRIPTOR_FLAGS;
}

Caml_inline bool frame_has_allocs(frame_descr *d) {
  return (d->frame_data & FRAME_DESCRIPTOR_ALLOC) != 0;
}

Caml_inline bool frame_has_debug(frame_descr *d) {
  return (d->frame_data & FRAME_DESCRIPTOR_DEBUG) != 0;
}

/* Allocation lengths are encoded reduced by one, so values 0-255 mean
 * sizes 1-256 words. */

#define Wosize_encoded_alloc_len(n) ((uintnat)(n) + 1)

/* Used to compute offsets in frame tables.
   ty must have power-of-2 size */
#define Align_to(p, ty) \
  (void*)(((uintnat)(p) + sizeof(ty) - 1) & ~(sizeof(ty) - 1))

#define Hash_retaddr(addr, mask)                          \
  (((uintnat)(addr) >> 3) & (mask))

void caml_init_frame_descriptors(void);

void caml_register_frametables(void **tables, int ntables);
void caml_register_frametable(void *table);

/* Create copies of the frametables and register them in the runtime.
   It writes back the pointers of the new copies of the frametables.
   Calling 'caml_unregister_frametable(s)' on these copies is safe
   and will free the allocated memory. */
void caml_copy_and_register_frametables(void **table, int *sizes, int ntables);
void* caml_copy_and_register_frametable(void *table, int size);

/* The unregistered frametables can still be in use after calling
   this function. Thus, you should not free their memory.
   Note: it may reorder the content of the array 'tables'.
   This can be called from a custom block finalizer. */
void caml_unregister_frametables(void **tables, int ntables);
void caml_unregister_frametable(void *table);

/* a linked list of frametables */
typedef struct caml_frametable_list {
  intnat* frametable;
  struct caml_frametable_list *next;
} caml_frametable_list;

/* a hashtable of frame descriptors */
typedef struct caml_frame_descrs caml_frame_descrs;

caml_frame_descrs* caml_get_frame_descrs(void);

/* Find the current table of frame descriptors.
   The resulting structure is only valid until the next GC */
frame_descr* caml_find_frame_descr(caml_frame_descrs *fds, uintnat pc);


/* Returns the next frame descriptor (or NULL if none is available),
   and updates *pc and *sp to point to the following one.  */
frame_descr *caml_next_frame_descriptor
    (caml_frame_descrs * fds, uintnat * pc, char ** sp,
     struct stack_info* stack);

#endif /* CAML_INTERNALS */

#endif /* CAML_FRAME_DESCRIPTORS_H */
