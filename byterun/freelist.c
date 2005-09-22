/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include "config.h"
#include "freelist.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "misc.h"
#include "mlvalues.h"

/* The free-list is kept sorted by increasing addresses.
   This makes the merging of adjacent free blocks possible.
   (See [caml_fl_merge_block].)
*/

typedef struct {
  char *next_bp;   /* Pointer to the first byte of the next block. */
} block;

/* The sentinel can be located anywhere in memory, but it must not be
   adjacent to any heap object. */
static struct {
  value filler1; /* Make sure the sentinel is never adjacent to any block. */
  header_t h;
  value first_bp;
  value filler2; /* Make sure the sentinel is never adjacent to any block. */
} sentinel = {0, Make_header (0, 0, Caml_blue), 0, 0};

#define Fl_head ((char *) (&(sentinel.first_bp)))
static char *fl_prev = Fl_head;  /* Current allocation pointer. */
static char *fl_last = NULL;     /* Last block in the list.  Only valid
                                 just after [caml_fl_allocate] returns NULL. */
char *caml_fl_merge = Fl_head;   /* Current insertion pointer.  Managed
                                    jointly with [sweep_slice]. */
asize_t caml_fl_cur_size = 0;    /* Number of words in the free list,
                                    including headers but not fragments. */

#define Next(b) (((block *) (b))->next_bp)

#ifdef DEBUG
static void fl_check (void)
{
  char *cur, *prev;
  int prev_found = 0, merge_found = 0;
  uintnat size_found = 0;

  prev = Fl_head;
  cur = Next (prev);
  while (cur != NULL){
    size_found += Whsize_bp (cur);
    Assert (Is_in_heap (cur));
    if (cur == fl_prev) prev_found = 1;
    if (cur == caml_fl_merge) merge_found = 1;
    prev = cur;
    cur = Next (prev);
  }
  Assert (prev_found || fl_prev == Fl_head);
  Assert (merge_found || caml_fl_merge == Fl_head);
  Assert (size_found == caml_fl_cur_size);
}
#endif

/* [allocate_block] is called by [caml_fl_allocate].  Given a suitable free
   block and the desired size, it allocates a new block from the free
   block.  There are three cases:
   0. The free block has the desired size.  Detach the block from the
      free-list and return it.
   1. The free block is 1 word longer than the desired size.  Detach
      the block from the free list.  The remaining word cannot be linked:
      turn it into an empty block (header only), and return the rest.
   2. The free block is big enough.  Split it in two and return the right
      block.
   In all cases, the allocated block is right-justified in the free block:
   it is located in the high-address words of the free block.  This way,
   the linking of the free-list does not change in case 2.
*/
static char *allocate_block (mlsize_t wh_sz, char *prev, char *cur)
{
  header_t h = Hd_bp (cur);
                                             Assert (Whsize_hd (h) >= wh_sz);
  if (Wosize_hd (h) < wh_sz + 1){                        /* Cases 0 and 1. */
    caml_fl_cur_size -= Whsize_hd (h);
    Next (prev) = Next (cur);
                    Assert (Is_in_heap (Next (prev)) || Next (prev) == NULL);
    if (caml_fl_merge == cur) caml_fl_merge = prev;
#ifdef DEBUG
    fl_last = NULL;
#endif
      /* In case 1, the following creates the empty block correctly.
         In case 0, it gives an invalid header to the block.  The function
         calling [caml_fl_allocate] will overwrite it. */
    Hd_op (cur) = Make_header (0, 0, Caml_white);
  }else{                                                        /* Case 2. */
    caml_fl_cur_size -= wh_sz;
    Hd_op (cur) = Make_header (Wosize_hd (h) - wh_sz, 0, Caml_blue);
  }
  fl_prev = prev;
  return cur + Bosize_hd (h) - Bsize_wsize (wh_sz);
}  

/* [caml_fl_allocate] does not set the header of the newly allocated block.
   The calling function must do it before any GC function gets called.
   [caml_fl_allocate] returns a head pointer.
*/
char *caml_fl_allocate (mlsize_t wo_sz)
{
  char *cur, *prev;
                                  Assert (sizeof (char *) == sizeof (value));
                                  Assert (fl_prev != NULL);
                                  Assert (wo_sz >= 1);
    /* Search from [fl_prev] to the end of the list. */
  prev = fl_prev;
  cur = Next (prev);
  while (cur != NULL){                             Assert (Is_in_heap (cur));
    if (Wosize_bp (cur) >= wo_sz){
      return allocate_block (Whsize_wosize (wo_sz), prev, cur);
    }
    prev = cur;
    cur = Next (prev);
  }
  fl_last = prev;
    /* Search from the start of the list to [fl_prev]. */
  prev = Fl_head;
  cur = Next (prev);
  while (prev != fl_prev){
    if (Wosize_bp (cur) >= wo_sz){
      return allocate_block (Whsize_wosize (wo_sz), prev, cur);
    }
    prev = cur;
    cur = Next (prev);
  }
    /* No suitable block was found. */
  return NULL;
}

static char *last_fragment;

void caml_fl_init_merge (void)
{
  last_fragment = NULL;
  caml_fl_merge = Fl_head;
#ifdef DEBUG
  fl_check ();
#endif
}

/* This is called by caml_compact_heap. */
void caml_fl_reset (void)
{
  Next (Fl_head) = 0;
  fl_prev = Fl_head;
  caml_fl_cur_size = 0;
  caml_fl_init_merge ();
}

/* [caml_fl_merge_block] returns the head pointer of the next block after [bp],
   because merging blocks may change the size of [bp]. */
char *caml_fl_merge_block (char *bp)
{
  char *prev, *cur, *adj;
  header_t hd = Hd_bp (bp);
  mlsize_t prev_wosz;

  caml_fl_cur_size += Whsize_hd (hd);
  
#ifdef DEBUG
  {
    mlsize_t i;
    for (i = 0; i < Wosize_hd (hd); i++){
      Field (Val_bp (bp), i) = Debug_free_major;
    }
  }
#endif
  prev = caml_fl_merge;
  cur = Next (prev);
  /* The sweep code makes sure that this is the right place to insert
     this block: */
  Assert (prev < bp || prev == Fl_head);
  Assert (cur > bp || cur == NULL);

  /* If [last_fragment] and [bp] are adjacent, merge them. */
  if (last_fragment == Hp_bp (bp)){
    mlsize_t bp_whsz = Whsize_bp (bp);
    if (bp_whsz <= Max_wosize){
      hd = Make_header (bp_whsz, 0, Caml_white);
      bp = last_fragment;
      Hd_bp (bp) = hd;
      caml_fl_cur_size += Whsize_wosize (0);
    }
  }

  /* If [bp] and [cur] are adjacent, remove [cur] from the free-list
     and merge them. */
  adj = bp + Bosize_hd (hd);
  if (adj == Hp_bp (cur)){
    char *next_cur = Next (cur);
    mlsize_t cur_whsz = Whsize_bp (cur);

    if (Wosize_hd (hd) + cur_whsz <= Max_wosize){
      Next (prev) = next_cur;
      if (fl_prev == cur) fl_prev = prev;
      hd = Make_header (Wosize_hd (hd) + cur_whsz, 0, Caml_blue);
      Hd_bp (bp) = hd;
      adj = bp + Bosize_hd (hd);
#ifdef DEBUG
      fl_last = NULL;
      Next (cur) = (char *) Debug_free_major;
      Hd_bp (cur) = Debug_free_major;
#endif
      cur = next_cur;
    }
  }
  /* If [prev] and [bp] are adjacent merge them, else insert [bp] into
     the free-list if it is big enough. */
  prev_wosz = Wosize_bp (prev);
  if (prev + Bsize_wsize (prev_wosz) == Hp_bp (bp)
      && prev_wosz + Whsize_hd (hd) < Max_wosize){
    Hd_bp (prev) = Make_header (prev_wosz + Whsize_hd (hd), 0,Caml_blue);
#ifdef DEBUG
    Hd_bp (bp) = Debug_free_major;
#endif
    Assert (caml_fl_merge == prev);
  }else if (Wosize_hd (hd) != 0){
    Hd_bp (bp) = Bluehd_hd (hd);
    Next (bp) = cur;
    Next (prev) = bp;
    caml_fl_merge = bp;
  }else{
    /* This is a fragment.  Leave it in white but remember it for eventual
       merging with the next block. */
    last_fragment = bp;
    caml_fl_cur_size -= Whsize_wosize (0);
  }
  return adj;
}

/* This is a heap extension.  We have to insert it in the right place
   in the free-list.
   [caml_fl_add_block] can only be called right after a call to
   [caml_fl_allocate] that returned NULL.
   Most of the heap extensions are expected to be at the end of the
   free list.  (This depends on the implementation of [malloc].)
*/
void caml_fl_add_block (char *bp)
{
                                                   Assert (fl_last != NULL);
                                            Assert (Next (fl_last) == NULL);
#ifdef DEBUG
  {
    mlsize_t i;
    for (i = 0; i < Wosize_bp (bp); i++){
      Field (Val_bp (bp), i) = Debug_free_major;
    }
  }
#endif

  caml_fl_cur_size += Whsize_bp (bp);

  if (bp > fl_last){
    Next (fl_last) = bp;
    Next (bp) = NULL;
  }else{
    char *cur, *prev;

    prev = Fl_head;
    cur = Next (prev);
    while (cur != NULL && cur < bp){   Assert (prev < bp || prev == Fl_head);
      prev = cur;
      cur = Next (prev);
    }                                  Assert (prev < bp || prev == Fl_head);
                                            Assert (cur > bp || cur == NULL);
    Next (bp) = cur;
    Next (prev) = bp;
    /* When inserting a block between [caml_fl_merge] and [caml_gc_sweep_hp],
       we must advance [caml_fl_merge] to the new block, so that [caml_fl_merge]
       is always the last free-list block before [caml_gc_sweep_hp]. */
    if (prev == caml_fl_merge && bp <= caml_gc_sweep_hp) caml_fl_merge = bp;
  }
}

/* Cut a block of memory into Max_wosize pieces, give them headers,
   and optionally merge them into the free list.
   arguments:
   p: pointer to the first word of the block
   size: size of the block (in words)
   do_merge: 1 -> do merge; 0 -> do not merge
*/
void caml_make_free_blocks (value *p, mlsize_t size, int do_merge)
{
  mlsize_t sz;

  while (size > 0){
    if (size > Whsize_wosize (Max_wosize)){
      sz = Whsize_wosize (Max_wosize);
    }else{
      sz = size;
    }
    *(header_t *)p = Make_header (Wosize_whsize (sz), 0, Caml_white);
    if (do_merge) caml_fl_merge_block (Bp_hp (p));
    size -= sz;
    p += sz;
  }
}
