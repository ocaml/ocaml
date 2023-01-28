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

#define Hash_retaddr(addr, mask)                          \
  (((uintnat)(addr) >> 3) & (mask))

/* Structure of frame descriptors */

typedef struct {
  uintnat retaddr;
  unsigned short frame_size;
  unsigned short num_live;
  unsigned short live_ofs[1 /* num_live */];
  /*
    If frame_size & 1, then debug info follows:
  uint32_t debug_info_offset;
    Debug info is stored as a relative offset to a debuginfo structure. */
} frame_descr;

/* Allocation lengths are encoded as 0-255, giving sizes 1-256 */
#define Wosize_encoded_alloc_len(n) ((uintnat)(n) + 1)

/* Used to compute offsets in frame tables.
   ty must have power-of-2 size */
#define Align_to(p, ty) \
  (void*)(((uintnat)(p) + sizeof(ty) - 1) & -sizeof(ty))


void caml_init_frame_descriptors(void);
void caml_register_frametables(void **tables, int ntables);

typedef struct {
  int num_descr;
  int mask;
  frame_descr** descriptors;
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
*/

caml_frame_descrs caml_get_frame_descrs(void);

/* Find the current table of frame descriptors.
   The resulting structure is only valid until the next GC */
frame_descr* caml_find_frame_descr(caml_frame_descrs fds, uintnat pc);


frame_descr * caml_next_frame_descriptor
    (caml_frame_descrs fds, uintnat * pc, char ** sp, struct stack_info* stack);

#endif /* CAML_INTERNALS */

#endif /* CAML_FRAME_DESCRIPTORS_H */
