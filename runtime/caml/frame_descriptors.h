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

#include "config.h"

/* The compiler generates a "frame descriptor" for every potential
 * return address. Each loaded module has a block of memory consisting
 * of these frame descriptors concatenated. Each frame descriptor
 * includes:
 *
 * - the stack frame size, in bytes;
 *
 * - whether it is the return address of a call into the garbage collector,
 *   and if so the sizes of all objects to be allocated by this call;
 *
 * - whether we have debug information for this stack frame, and if so
 *   the "debuginfo" (source location) of this return address. (For
 *   GC return addresses, this is an array of debuginfo, one for each
 *   of the set of allocations performed by this GC entry).
 *
 * - the register or stack frame offset of every "live" value,
 *   which should be scanned by the garbage collector if a GC is
 *   performed at this point.
 */

typedef struct {
  uintnat retaddr;
  uint16_t frame_size;
  uint16_t num_live;
  uint16_t live_ofs[1 /* num_live */];
  /*
    If FRAME_HAS_ALLOC, alloc lengths follow:
        uint8_t num_allocs;
        uint8_t alloc[num_allocs];
    Each alloc length has an offset of 1, giving sizes 1-256.

    If FRAME_HAS_DEBUG, debug info follows (32-bit aligned):
        uint32_t debug_info[FRAME_HAS_ALLOC ? num_allocs : 1];

    Debug info is stored as a relative offset, in bytes, from the
    debug_info itself to a debuginfo structure. */
} frame_descr;

/* Two bits in the frame_size field are used to indicate whether the
 * allocation sizes and debug info are present: */

#define FRAME_DESCRIPTOR_DEBUG 1
#define FRAME_DESCRIPTOR_ALLOC 2
#define FRAME_SIZE(fd) ((fd)->frame_size &~ 3)
#define FRAME_HAS_ALLOC(fd) ((fd)->frame_size & FRAME_DESCRIPTOR_ALLOC)
#define FRAME_HAS_DEBUG(fd) ((fd)->frame_size & FRAME_DESCRIPTOR_DEBUG)

/* If frame_size is 0xFFFF, this stack frame has no debug info or
 * allocation sizes but is the special entry frame at the top of each
 * ML stack chunk. */

#define FRAME_CHUNK_TOP(fd) ((fd)->frame_size == 0xFFFF)

/* Allocation lengths are encoded as 0-255, giving sizes 1-256 */

#define Wosize_encoded_alloc_len(n) ((uintnat)(n) + 1)

/* Used to compute offsets in frame tables.
   ty must have power-of-2 size */
#define Align_to(p, ty) \
  (void*)(((uintnat)(p) + sizeof(ty) - 1) & -sizeof(ty))

#define Hash_retaddr(addr, mask)                          \
  (((uintnat)(addr) >> 3) & (mask))

void caml_init_frame_descriptors(void);
void caml_register_frametables(void **tables, int ntables);

/* a linked list of frametables */
typedef struct caml_frametable_list {
  intnat* frametable;
  struct caml_frametable_list *next;
} caml_frametable_list;

/* a hashtable of frame descriptors */
typedef struct {
  int num_descr;
  int mask;
  frame_descr** descriptors;
  caml_frametable_list *frametables;
} caml_frame_descrs;
/* Let us call 'capacity' the length of the descriptors array.

   We maintain the following invariants:
     capacity = mask + 1
     capacity = 0 || Is_power_of_2(capacity)
     num_desc <= 2 * num_descr <= capacity

   For an extensible array we would maintain
      num_desc <= capacity,
    but this is a linear-problem hash table, we need to ensure that
    free slots are frequent enough, so we use a twice-larger capacity:
      num_desc * 2 <= capacity

   We keep the list of frametables that was used to build the hashtable.
   We use it when rebuilding the table after resizing.

   Some frame tables in the list may have been unregistered after the
   hashtable was built, so in general [num_descrs] is an over-approximation
   of the true number of frame descriptors in the [list].
*/

caml_frame_descrs caml_get_frame_descrs(void);

/* Find the current table of frame descriptors.
   The resulting structure is only valid until the next GC */
frame_descr* caml_find_frame_descr(caml_frame_descrs fds, uintnat pc);


frame_descr * caml_next_frame_descriptor
    (caml_frame_descrs fds, uintnat * pc, char ** sp, struct stack_info* stack);

#endif /* CAML_INTERNALS */

#endif /* CAML_FRAME_DESCRIPTORS_H */
