/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <string.h>

#include "caml/address_class.h"
#include "caml/config.h"
#include "caml/finalise.h"
#include "caml/freelist.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/weak.h"
#include "caml/compact.h"
#include "caml/memprof.h"
#include "caml/eventlog.h"

extern uintnat caml_percent_free;                   /* major_gc.c */
extern void caml_shrink_heap (char *);              /* memory.c */

/* Colors

   We use the GC's color bits in the following way:

   - White words are headers of live blocks except for 0, which is a
     fragment.
   - Blue words are headers of free blocks.
   - Black words are headers of out-of-heap "blocks".
   - Gray words are the encoding of pointers in inverted lists.

   Encoded pointers:
   Pointers always have their two low-order bits clear. We make use of
   this to encode pointers by shifting bits 2-9 to 0-7:
   ...XXXyyyyyyyy00 becomes ...XXX01yyyyyyyy
   Note that 01 corresponds to the "gray" color of the GC, so we can now
   mix pointers and headers because there are no gray headers anywhere in
   the heap (or outside) when we start a compaction (which must be done at
   the end of a sweep phase).
*/

typedef uintnat word;

#define eptr(p) \
  (((word) (p) & ~0x3FF) | ((((word) p) & 0x3FF) >> 2) | Caml_gray)
#define dptr(p) ((word *) (((word) (p) & ~0x3FF) | ((((word) p) & 0xFF) << 2)))

static void invert_pointer_at (word *p)
{
  word q = *p;
  header_t h;

  CAMLassert (((uintnat) p & 3) == 0);

  if (Is_block (q) && Is_in_value_area (q)){
    h = Hd_val (q);
    switch (Color_hd (h)){
    case Caml_white:
      if (Tag_hd (h) == Infix_tag){
        value realvalue = (value) q - Infix_offset_val (q);
        if (Is_black_val (realvalue)) break;
      }
      /* FALL THROUGH */
    case Caml_gray:
      CAMLassert (Is_in_heap (q));
      /* [q] points to some inverted list, insert it. */
      *p = h;
      Hd_val (q) = eptr (p);
      break;
    case Caml_black:
      /* [q] points to an out-of-heap value. Leave it alone. */
      break;
    default: /* Caml_blue */
      /* We found a pointer to a free block. This cannot happen. */
      CAMLassert (0);
      break;
    }
  }
}

void caml_invert_root (value v, value *p)
{
#ifdef NO_NAKED_POINTERS
  /* Note: this assertion will become tautological and should be removed when
     we finally get rid of the page table in NNP mode.
  */
  CAMLassert (Is_long (*p) || Is_in_heap (*p) || Is_black_val (*p)
              || Tag_val (*p) == Infix_tag);
#endif
  invert_pointer_at ((word *) p);
}

static char *compact_fl;

static void init_compact_allocate (void)
{
  char *ch = caml_heap_start;
  while (ch != NULL){
    Chunk_alloc (ch) = 0;
    ch = Chunk_next (ch);
  }
  compact_fl = caml_heap_start;
}

/* [size] is a number of bytes and includes the header size */
static char *compact_allocate (mlsize_t size)
{
  char *chunk, *adr;

  while (Chunk_size(compact_fl) - Chunk_alloc(compact_fl) < Bhsize_wosize(1)){
    compact_fl = Chunk_next (compact_fl);
    CAMLassert (compact_fl != NULL);
  }
  chunk = compact_fl;
  while (Chunk_size (chunk) - Chunk_alloc (chunk) < size){
    chunk = Chunk_next (chunk);
    CAMLassert (chunk != NULL);
  }
  adr = chunk + Chunk_alloc (chunk);
  Chunk_alloc (chunk) += size;
  return adr;
}

static void do_compaction (intnat new_allocation_policy)
{
  char *ch, *chend;
  CAMLassert (caml_gc_phase == Phase_idle);
  caml_gc_message (0x10, "Compacting heap...\n");

#ifdef DEBUG
  caml_heap_check ();
#endif

  /* Make sure the heap is in the right state for compaction:
     - all free blocks are blue
     - all other blocks are white and contain valid pointers
  */
  caml_fl_reset_and_switch_policy (new_allocation_policy);

  /* First pass: removed in 4.12 thanks to the new closure representation. */


  /* Second pass: invert pointers.
     Don't forget roots and weak pointers.
     This is a mark-like pass. */
  {
    caml_do_roots (caml_invert_root, 1);
    /* The values to be finalised are not roots but should still be inverted */
    caml_final_invert_finalisable_values ();
    /* Idem for memprof tracked blocks */
    caml_memprof_invert_tracked ();

    ch = caml_heap_start;
    while (ch != NULL){
      word *p = (word *) ch;
      chend = ch + Chunk_size (ch);

      while ((char *) p < chend){
        word q = *p;
        mlsize_t wosz, i, first_field;
        tag_t t;

        while (Is_gray_hd (q)) q = * dptr (q);
        wosz = Wosize_hd (q);
        if (Is_white_hd (q)){
          t = Tag_hd (q);
          CAMLassert (t != Infix_tag);
          if (t < No_scan_tag){
            value v = Val_hp (p);
            if (t == Closure_tag){
              first_field = Start_env_closinfo (Closinfo_val (v));
            }else{
              first_field = 0;
            }
            for (i = first_field; i < wosz; i++){
              invert_pointer_at ((word *) &Field (v,i));
            }
          }
        }
        p += Whsize_wosize (wosz);
      }
      ch = Chunk_next (ch);
    }
    /* Invert weak pointers. */
    {
      value *pp = &caml_ephe_list_head;
      value p;
      word q;
      size_t sz, i;

      while (1){
        p = *pp;
        if (p == (value) NULL) break;
        q = Hd_val (p);
        while (Is_gray_hd (q)) q = * dptr (q);
        CAMLassert (Is_white_hd (q));
        sz = Wosize_hd (q);
        for (i = 1; i < sz; i++){
          if (Field (p,i) != caml_ephe_none){
            invert_pointer_at ((word *) &(Field (p,i)));
          }
        }
        invert_pointer_at ((word *) pp);
        pp = &Field (p, 0);
      }
    }
  }


  /* Third pass: reallocate virtually; revert pointers.
     This is a sweep-like pass. */
  {
    init_compact_allocate ();
    ch = caml_heap_start;
    while (ch != NULL){
      word *p = (word *) ch;

      chend = ch + Chunk_size (ch);
      while ((char *) p < chend){
        header_t h = Hd_hp (p);
        size_t sz;

        while (Is_gray_hd (h)) h = * dptr (h);
        sz = Whsize_hd (h);

        CAMLassert (!Is_black_hd (h));
        CAMLassert (!Is_gray_hd (h));
        if (h != 0 && Is_white_hd (h)){
          word q;
          tag_t t;
          char *newadr;

          t = Tag_hd (h);
          CAMLassert (t != Infix_tag);

          newadr = compact_allocate (Bsize_wsize (sz));
          q = *p;
          while (Is_gray_hd (q)){
            word *pp = dptr (q);
            q = *pp;
            *pp = (word) Val_hp (newadr);
          }
          CAMLassert (q == h);
          *p = q;

          if (t == Closure_tag){
            /* Revert the infix pointers to this block. */
            mlsize_t i, startenv;
            value v;

            v = Val_hp (p);
            startenv = Start_env_closinfo (Closinfo_val (v));
            i = 0;
            while (1){
              int arity = Arity_closinfo (Field (v, i+1));
              i += 2 + (arity != 0 && arity != 1);
              if (i >= startenv) break;

              /* Revert the inverted list for infix header at offset [i]. */
              q = Field (v, i);
              while (Is_gray_hd (q)){
                word *pp = dptr (q);
                q = *pp;
                *pp = (word) Val_hp ((header_t *) &Field (Val_hp (newadr), i));
              }
              CAMLassert (Tag_hd (q) == Infix_tag);
              Field (v, i) = q;
              ++i;
            }
          }
        }
        p += sz;
      }
      ch = Chunk_next (ch);
    }
  }


  /* Fourth pass: reallocate and move objects.
     Use the exact same allocation algorithm as pass 3. */
  {
    init_compact_allocate ();
    ch = caml_heap_start;
    while (ch != NULL){
      word *p = (word *) ch;

      chend = ch + Chunk_size (ch);
      while ((char *) p < chend){
        word q = *p;
        if (q != 0 && Is_white_hd (q)){
          size_t sz = Bhsize_hd (q);
          char *newadr = compact_allocate (sz);
          memmove (newadr, p, sz);
          p += Wsize_bsize (sz);
        }else{
          CAMLassert (q == 0 || Is_blue_hd (q));
          p += Whsize_hd (q);
        }
      }
      ch = Chunk_next (ch);
    }
  }

  /* Shrink the heap if needed. */
  {
    /* Find the amount of live data and the unshrinkable free space. */
    asize_t live = 0;
    asize_t free = 0;
    asize_t wanted;

    ch = caml_heap_start;
    while (ch != NULL){
      if (Chunk_alloc (ch) != 0){
        live += Wsize_bsize (Chunk_alloc (ch));
        free += Wsize_bsize (Chunk_size (ch) - Chunk_alloc (ch));
      }
      ch = Chunk_next (ch);
    }

    /* Add up the empty chunks until there are enough, then remove the
       other empty chunks. */
    wanted = caml_percent_free * (live / 100 + 1);
    ch = caml_heap_start;
    while (ch != NULL){
      char *next_chunk = Chunk_next (ch);  /* Chunk_next (ch) will be erased */

      if (Chunk_alloc (ch) == 0){
        if (free < wanted){
          free += Wsize_bsize (Chunk_size (ch));
        }else{
          caml_shrink_heap (ch);
        }
      }
      ch = next_chunk;
    }
  }

  /* Rebuild the free list. This is the right time for a change of
     allocation policy, since we are rebuilding the allocator's data
     structures from scratch. */
  {
    ch = caml_heap_start;
    caml_fl_init_merge ();
    while (ch != NULL){
      if (Chunk_size (ch) > Chunk_alloc (ch)){
        caml_make_free_blocks ((value *) (ch + Chunk_alloc (ch)),
                               Wsize_bsize (Chunk_size(ch)-Chunk_alloc(ch)), 1,
                               Caml_white);
      }
      ch = Chunk_next (ch);
    }
  }
  ++ Caml_state->stat_compactions;

  caml_shrink_mark_stack();

  caml_gc_message (0x10, "done.\n");
}

uintnat caml_percent_max;  /* used in gc_ctrl.c and memory.c */

void caml_compact_heap (intnat new_allocation_policy)
{
  uintnat target_wsz, live;

  CAMLassert (Caml_state->young_ptr == Caml_state->young_alloc_end);
  CAMLassert (Caml_state->ref_table->ptr ==
              Caml_state->ref_table->base);
  CAMLassert (Caml_state->ephe_ref_table->ptr ==
              Caml_state->ephe_ref_table->base);
  CAMLassert (Caml_state->custom_table->ptr ==
              Caml_state->custom_table->base);

  CAML_EV_BEGIN(EV_COMPACT_MAIN);
  do_compaction (new_allocation_policy);
  CAML_EV_END(EV_COMPACT_MAIN);
  /* Compaction may fail to shrink the heap to a reasonable size
     because it deals in complete chunks: if a very large chunk
     is at the beginning of the heap, everything gets moved to
     it and it is not freed.

     In that case, we allocate a new chunk of the desired heap
     size, chain it at the beginning of the heap (thus pretending
     its address is smaller), and launch a second compaction.
     This will move all data to this new chunk and free the
     very large chunk.

     See PR#5389
  */
  /* We compute:
     freewords = caml_fl_cur_wsz                   (exact)
     heapwords = Wsize_bsize (caml_heap_size)      (exact)
     live = heapwords - freewords
     wanted = caml_percent_free * (live / 100 + 1) (same as in do_compaction)
     target_wsz = live + wanted
     We add one page to make sure a small difference in counting sizes
     won't make [do_compaction] keep the second block (and break all sorts
     of invariants).

     We recompact if target_wsz < heap_size / 2
  */
  live = Caml_state->stat_heap_wsz - caml_fl_cur_wsz;
  target_wsz = live + caml_percent_free * (live / 100 + 1)
                 + Wsize_bsize (Page_size);
  target_wsz = caml_clip_heap_chunk_wsz (target_wsz);

#ifdef HAS_HUGE_PAGES
  if (caml_use_huge_pages
      && Bsize_wsize (Caml_state->stat_heap_wsz) <= HUGE_PAGE_SIZE)
    return;
#endif

  if (target_wsz < Caml_state->stat_heap_wsz / 2){
    /* Recompact. */
    char *chunk;

    caml_gc_message (0x10, "Recompacting heap (target=%"
                     ARCH_INTNAT_PRINTF_FORMAT "uk words)\n",
                     target_wsz / 1024);

    chunk = caml_alloc_for_heap (Bsize_wsize (target_wsz));
    if (chunk == NULL) return;
    /* PR#5757: we need to make the new blocks blue, or they won't be
       recognized as free by the recompaction. */
    caml_make_free_blocks ((value *) chunk,
                           Wsize_bsize (Chunk_size (chunk)), 0, Caml_blue);
    if (caml_page_table_add (In_heap, chunk, chunk + Chunk_size (chunk)) != 0){
      caml_free_for_heap (chunk);
      return;
    }
    Chunk_next (chunk) = caml_heap_start;
    caml_heap_start = chunk;
    ++ Caml_state->stat_heap_chunks;
    Caml_state->stat_heap_wsz += Wsize_bsize (Chunk_size (chunk));
    if (Caml_state->stat_heap_wsz > Caml_state->stat_top_heap_wsz){
      Caml_state->stat_top_heap_wsz = Caml_state->stat_heap_wsz;
    }
    CAML_EV_BEGIN(EV_COMPACT_RECOMPACT);
    do_compaction (-1);
    CAMLassert (Caml_state->stat_heap_chunks == 1);
    CAMLassert (Chunk_next (caml_heap_start) == NULL);
    CAMLassert (Caml_state->stat_heap_wsz == Wsize_bsize (Chunk_size (chunk)));
    CAML_EV_END(EV_COMPACT_RECOMPACT);
  }
}

void caml_compact_heap_maybe (double previous_overhead)
{
  CAMLassert (caml_gc_phase == Phase_idle);
  if (caml_percent_max >= 1000000) return;
  if (Caml_state->stat_major_collections < 3) return;
  if (Caml_state->stat_heap_wsz <= 2 * caml_clip_heap_chunk_wsz (0)) return;

#ifdef HAS_HUGE_PAGES
  if (caml_use_huge_pages
      && Bsize_wsize (Caml_state->stat_heap_wsz) <= HUGE_PAGE_SIZE)
    return;
#endif

  if (previous_overhead >= caml_percent_max){
    double current_overhead;

    caml_gc_message (0x200, "Automatic compaction triggered.\n");
    caml_empty_minor_heap ();  /* minor heap must be empty for compaction */
    caml_gc_message
      (0x1, "Finishing major GC cycle (triggered by compaction)\n");
    caml_finish_major_cycle ();
    ++ Caml_state->stat_forced_major_collections;

    /* Note: There is no floating garbage because we just did a complete
       major cycle*/
    current_overhead =
      100.0 * caml_fl_cur_wsz / (Caml_state->stat_heap_wsz - caml_fl_cur_wsz);
    caml_gc_message (0x200, "Current overhead: %"
                            ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                     (uintnat) current_overhead);
    if (current_overhead >= caml_percent_max)
      caml_compact_heap (-1);
    else
      caml_gc_message (0x200, "Automatic compaction aborted.\n");
  }
}
