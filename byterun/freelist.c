/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
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
   (See [fl_merge_block].)
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
} sentinel = {0, Make_header (0, 0, Blue), 0, 0};

#define Fl_head ((char *) (&(sentinel.first_bp)))
static char *fl_prev = Fl_head;  /* Current allocation pointer. */
static char *fl_last = NULL;     /* Last block in the list.  Only valid
                                    just after fl_allocate returned NULL. */
char *fl_merge = Fl_head;        /* Current insertion pointer.  Managed
                                    jointly with [sweep_slice]. */
asize_t fl_cur_size = 0;         /* How many free words were added since
                                    the latest fl_init_merge. */

#define Next(b) (((block *) (b))->next_bp)

#ifdef DEBUG
void fl_check ()
{
  char *cur, *prev;
  int prev_found = 0, merge_found = 0;

  prev = Fl_head;
  cur = Next (prev);
  while (cur != NULL){
    Assert (Is_in_heap (cur));
    if (cur == fl_prev) prev_found = 1;
    if (cur == fl_merge) merge_found = 1;
    prev = cur;
    cur = Next (prev);
  }
  Assert (prev_found || fl_prev == Fl_head);
  Assert (merge_found || fl_merge == Fl_head);
}
#endif

/* [allocate_block] is called by [fl_allocate].  Given a suitable free
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
static char *allocate_block (wh_sz, prev, cur)
    mlsize_t wh_sz;
    char *prev, *cur;
{
  header_t h = Hd_bp (cur);
                                             Assert (Whsize_hd (h) >= wh_sz);
  if (Wosize_hd (h) < wh_sz + 1){                        /* Cases 0 and 1. */
    Next (prev) = Next (cur);
                    Assert (Is_in_heap (Next (prev)) || Next (prev) == NULL);
    if (fl_merge == cur) fl_merge = prev;
#ifdef DEBUG
    fl_last = NULL;
#endif
      /* In case 1, the following creates the empty block correctly.
         In case 0, it gives an invalid header to the block.  The function
         calling [fl_allocate] will overwrite it. */
    Hd_op (cur) = Make_header (0, 0, White);
  }else{                                                        /* Case 2. */
    Hd_op (cur) = Make_header (Wosize_hd (h) - wh_sz, 0, Blue);
  }
  fl_prev = prev;
  return cur + Bosize_hd (h) - Bsize_wsize (wh_sz);
}  

/* [fl_allocate] does not set the header of the newly allocated block.
   The calling function must do it before any GC function gets called.
   [fl_allocate] returns a head pointer.
*/
char *fl_allocate (wo_sz)
     mlsize_t wo_sz;
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

void fl_init_merge ()
{
  last_fragment = NULL;
  fl_merge = Fl_head;
  fl_cur_size = 0;
#ifdef DEBUG
  fl_check ();
#endif
}

/* This is called by compact_heap. */
void fl_reset ()
{
  Next (Fl_head) = 0;
  fl_prev = Fl_head;
  fl_init_merge ();
}

/* [fl_merge_block] returns the head pointer of the next block after [bp],
   because merging blocks may change the size of [bp]. */
char *fl_merge_block (bp)
    char *bp;
{
  char *prev, *cur, *adj;
  header_t hd = Hd_bp (bp);

  fl_cur_size += Whsize_hd (hd);
  
#ifdef DEBUG
  {
    mlsize_t i;
    for (i = 0; i < Wosize_hd (hd); i++){
      Field (Val_bp (bp), i) = not_random ();
    }
  }
#endif
  prev = fl_merge;
  cur = Next (prev);
  /* The sweep code makes sure that this is the right place to insert
     this block: */
  Assert (prev < bp || prev == Fl_head);
  Assert (cur > bp || cur == NULL);

  /* If [last_fragment] and [bp] are adjacent, merge them. */
  if (last_fragment == Hp_bp (bp)){
    hd = Make_header (Whsize_bp (bp), 0, White);
    bp = last_fragment;
    Hd_bp (bp) = hd;
    fl_cur_size += Whsize_wosize (0);
  }

  /* If [bp] and [cur] are adjacent, remove [cur] from the free-list
     and merge them. */
  adj = bp + Bosize_hd (hd);
  if (adj == Hp_bp (cur)){
    char *next_cur = Next (cur);
    long cur_whsz = Whsize_bp (cur);

    Next (prev) = next_cur;
    if (fl_prev == cur) fl_prev = prev;
    hd = Make_header (Wosize_hd (hd) + cur_whsz, 0, Blue);
    Hd_bp (bp) = hd;
    adj = bp + Bosize_hd (hd);
#ifdef DEBUG
    fl_last = NULL;
    Next (cur) = (char *) not_random ();
    Hd_bp (cur) = not_random ();
#endif
    cur = next_cur;
  }
  /* If [prev] and [bp] are adjacent merge them, else insert [bp] into
     the free-list if it is big enough. */
  if (prev + Bosize_bp (prev) == Hp_bp (bp)){
    Hd_bp (prev) = Make_header (Wosize_bp (prev) + Whsize_hd (hd), 0, Blue);
#ifdef DEBUG
    Hd_bp (bp) = not_random ();
#endif
    Assert (fl_merge == prev);
  }else if (Wosize_hd (hd) != 0){
    Hd_bp (bp) = Bluehd_hd (hd);
    Next (bp) = cur;
    Next (prev) = bp;
    fl_merge = bp;
  }else{
    /* This is a fragment.  Leave it in white but remember it for eventual
       merging with the next block. */
    last_fragment = bp;
  }
  return adj;
}

/* This is a heap extension.  We have to insert it in the right place
   in the free-list.
   [fl_add_block] can only be called just after a call to [fl_allocate]
   that returned NULL.
   Most of the heap extensions are expected to be at the end of the
   free list.  (This depends on the implementation of [malloc].)
*/
void fl_add_block (bp)
     char *bp;
{
                                                   Assert (fl_last != NULL);
                                            Assert (Next (fl_last) == NULL);
#ifdef DEBUG
  {
    mlsize_t i;
    for (i = 0; i < Wosize_bp (bp); i++){
      Field (Val_bp (bp), i) = not_random ();
    }
  }
#endif
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
    /* When inserting a block between fl_merge and gc_sweep_hp, we must
       advance fl_merge to the new block, so that fl_merge is always the
       last free-list block before gc_sweep_hp. */
    if (prev == fl_merge && bp <= gc_sweep_hp) fl_merge = bp;
  }
}
