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

extern uintnat caml_percent_free;                   /* major_gc.c */
extern void caml_shrink_heap (char *);              /* memory.c */

/* Encoded headers: the color is stored in the 2 least significant bits.
   (For pointer inversion, we need to distinguish headers from pointers.)
   s is a Wosize, t is a tag, and c is a color (a two-bit number)

   For the purpose of compaction, "colors" are:
   0: pointers (direct or inverted)
   1: integer or (unencoded) infix header
   2: inverted pointer for infix header
   3: integer or encoded (noninfix) header

  XXX Should be fixed:
  XXX The above assumes that all roots are aligned on a 4-byte boundary,
  XXX which is not always guaranteed by C.
  XXX (see [caml_register_global_roots])
  XXX Should be able to fix it to only assume 2-byte alignment.
*/
#ifdef WITH_PROFINFO
#define Make_ehd(s,t,c,p) \
  (((s) << 10) | (t) << 2 | (c) | ((p) << PROFINFO_SHIFT))
#else
#define Make_ehd(s,t,c,p) (((s) << 10) | (t) << 2 | (c))
#endif
#define Whsize_ehd(h) Whsize_hd (h)
#define Wosize_ehd(h) Wosize_hd (h)
#define Tag_ehd(h) (((h) >> 2) & 0xFF)
#ifdef WITH_PROFINFO
#define Profinfo_ehd(hd) Profinfo_hd(hd)
#endif
#define Ecolor(w) ((w) & 3)

typedef uintnat word;

static void invert_pointer_at (word *p)
{
  word q = *p;
  CAMLassert (Ecolor ((intnat) p) == 0);

  /* Use Ecolor (q) == 0 instead of Is_block (q) because q could be an
     inverted pointer for an infix header (with Ecolor == 2). */
  if (Ecolor (q) == 0 && Is_in_heap (q)){
    switch (Ecolor (Hd_val (q))){
    case 0:
    case 3: /* Pointer or header: insert in inverted list. */
      *p = Hd_val (q);
      Hd_val (q) = (header_t) p;
      break;
    case 1: /* Infix header: make inverted infix list. */
      /* Double inversion: the last of the inverted infix list points to
         the next infix header in this block.  The last of the last list
         contains the original block header. */
      {
        /* This block as a value. */
        value val = (value) q - Infix_offset_val (q);
        /* Get the block header. */
        word *hp = (word *) Hp_val (val);

        while (Ecolor (*hp) == 0) hp = (word *) *hp;
        CAMLassert (Ecolor (*hp) == 3);
        if (Tag_ehd (*hp) == Closure_tag){
          /* This is the first infix found in this block. */
          /* Save original header. */
          *p = *hp;
          /* Link inverted infix list. */
          Hd_val (q) = (header_t) ((word) p | 2);
          /* Change block header's tag to Infix_tag, and change its size
             to point to the infix list. */
          *hp = Make_ehd (Wosize_bhsize (q - val), Infix_tag, 3, (uintnat) 0);
        }else{
          CAMLassert (Tag_ehd (*hp) == Infix_tag);
          /* Point the last of this infix list to the current first infix
             list of the block. */
          *p = (word) &Field (val, Wosize_ehd (*hp)) | 1;
          /* Point the head of this infix list to the above. */
          Hd_val (q) = (header_t) ((word) p | 2);
          /* Change block header's size to point to this infix list. */
          *hp = Make_ehd (Wosize_bhsize (q - val), Infix_tag, 3, (uintnat) 0);
        }
      }
      break;
    case 2: /* Inverted infix list: insert. */
      *p = Hd_val (q);
      Hd_val (q) = (header_t) ((word) p | 2);
      break;
    }
  }
}

void caml_invert_root (value v, value *p)
{
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

  while (Chunk_size (compact_fl) - Chunk_alloc (compact_fl) <= Bhsize_wosize (3)
         && Chunk_size (Chunk_next (compact_fl))
            - Chunk_alloc (Chunk_next (compact_fl))
            <= Bhsize_wosize (3)){
    compact_fl = Chunk_next (compact_fl);
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

static void do_compaction (void)
{
  char *ch, *chend;
  CAMLassert (caml_gc_phase == Phase_idle);
  caml_gc_message (0x10, "Compacting heap...\n");

#ifdef DEBUG
  caml_heap_check ();
#endif

  /* First pass: encode all noninfix headers. */
  {
    ch = caml_heap_start;
    while (ch != NULL){
      header_t *p = (header_t *) ch;

      chend = ch + Chunk_size (ch);
      while ((char *) p < chend){
        header_t hd = Hd_hp (p);
        mlsize_t sz = Wosize_hd (hd);

        if (Is_blue_hd (hd)){
          /* Free object.  Give it a string tag. */
          Hd_hp (p) = Make_ehd (sz, String_tag, 3, (uintnat) 0);
        }else{
          CAMLassert (Is_white_hd (hd));
          /* Live object.  Keep its tag. */
          Hd_hp (p) = Make_ehd (sz, Tag_hd (hd), 3, Profinfo_hd (hd));
        }
        p += Whsize_wosize (sz);
      }
      ch = Chunk_next (ch);
    }
  }


  /* Second pass: invert pointers.
     Link infix headers in each block in an inverted list of inverted lists.
     Don't forget roots and weak pointers. */
  {
    /* Invert roots first because the threads library needs some heap
       data structures to find its roots.  Fortunately, it doesn't need
       the headers (see above). */
    caml_do_roots (caml_invert_root, 1);
    /* The values to be finalised are not roots but should still be inverted */
    caml_final_invert_finalisable_values ();

    ch = caml_heap_start;
    while (ch != NULL){
      word *p = (word *) ch;
      chend = ch + Chunk_size (ch);

      while ((char *) p < chend){
        word q = *p;
        size_t sz, i;
        tag_t t;
        word *infixes;

        while (Ecolor (q) == 0) q = * (word *) q;
        sz = Whsize_ehd (q);
        t = Tag_ehd (q);

        if (t == Infix_tag){
          /* Get the original header of this block. */
          infixes = p + sz;
          q = *infixes;
          while (Ecolor (q) != 3) q = * (word *) (q & ~(uintnat)3);
          sz = Whsize_ehd (q);
          t = Tag_ehd (q);
        }

        if (t < No_scan_tag){
          for (i = 1; i < sz; i++) invert_pointer_at (&(p[i]));
        }
        p += sz;
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
        while (Ecolor (q) == 0) q = * (word *) q;
        sz = Wosize_ehd (q);
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


  /* Third pass: reallocate virtually; revert pointers; decode headers.
     Rebuild infix headers. */
  {
    init_compact_allocate ();
    ch = caml_heap_start;
    while (ch != NULL){
      word *p = (word *) ch;

      chend = ch + Chunk_size (ch);
      while ((char *) p < chend){
        word q = *p;

        if (Ecolor (q) == 0 || Tag_ehd (q) == Infix_tag){
          /* There were (normal or infix) pointers to this block. */
          size_t sz;
          tag_t t;
          char *newadr;
#ifdef WITH_PROFINFO
          uintnat profinfo;
#endif
          word *infixes = NULL;

          while (Ecolor (q) == 0) q = * (word *) q;
          sz = Whsize_ehd (q);
          t = Tag_ehd (q);
#ifdef WITH_PROFINFO
          profinfo = Profinfo_ehd (q);
#endif
          if (t == Infix_tag){
            /* Get the original header of this block. */
            infixes = p + sz;
            q = *infixes;
            CAMLassert (Ecolor (q) == 2);
            while (Ecolor (q) != 3) q = * (word *) (q & ~(uintnat)3);
            sz = Whsize_ehd (q);
            t = Tag_ehd (q);
          }

          newadr = compact_allocate (Bsize_wsize (sz));
          q = *p;
          while (Ecolor (q) == 0){
            word next = * (word *) q;
            * (word *) q = (word) Val_hp (newadr);
            q = next;
          }
          *p = Make_header_with_profinfo (Wosize_whsize (sz), t, Caml_white,
            profinfo);

          if (infixes != NULL){
            /* Rebuild the infix headers and revert the infix pointers. */
            while (Ecolor ((word) infixes) != 3){
              infixes = (word *) ((word) infixes & ~(uintnat) 3);
              q = *infixes;
              while (Ecolor (q) == 2){
                word next;
                q = (word) q & ~(uintnat) 3;
                next = * (word *) q;
                * (word *) q = (word) Val_hp ((word *) newadr + (infixes - p));
                q = next;
              }
              CAMLassert (Ecolor (q) == 1 || Ecolor (q) == 3);
              /* No need to preserve any profinfo value on the [Infix_tag]
                 headers; the Spacetime profiling heap snapshot code doesn't
                 look at them. */
              *infixes = Make_header (infixes - p, Infix_tag, Caml_white);
              infixes = (word *) q;
            }
          }
          p += sz;
        }else{
          CAMLassert (Ecolor (q) == 3);
          /* This is guaranteed only if caml_compact_heap was called after a
             nonincremental major GC:    CAMLassert (Tag_ehd (q) == String_tag);
          */
          /* No pointers to the header and no infix header:
             the object was free. */
          *p = Make_header (Wosize_ehd (q), Tag_ehd (q), Caml_blue);
          p += Whsize_ehd (q);
        }
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
        if (Color_hd (q) == Caml_white){
          size_t sz = Bhsize_hd (q);
          char *newadr = compact_allocate (sz);
          memmove (newadr, p, sz);
          p += Wsize_bsize (sz);
        }else{
          CAMLassert (Color_hd (q) == Caml_blue);
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

  /* Rebuild the free list. */
  {
    ch = caml_heap_start;
    caml_fl_reset ();
    while (ch != NULL){
      if (Chunk_size (ch) > Chunk_alloc (ch)){
        caml_make_free_blocks ((value *) (ch + Chunk_alloc (ch)),
                               Wsize_bsize (Chunk_size(ch)-Chunk_alloc(ch)), 1,
                               Caml_white);
      }
      ch = Chunk_next (ch);
    }
  }
  ++ caml_stat_compactions;
  caml_gc_message (0x10, "done.\n");
}

uintnat caml_percent_max;  /* used in gc_ctrl.c and memory.c */

void caml_compact_heap (void)
{
  uintnat target_wsz, live;
  CAML_INSTR_SETUP(tmr, "compact");

  CAMLassert (caml_young_ptr == caml_young_alloc_end);
  CAMLassert (caml_ref_table.ptr == caml_ref_table.base);
  CAMLassert (caml_ephe_ref_table.ptr == caml_ephe_ref_table.base);
  CAMLassert (caml_custom_table.ptr == caml_custom_table.base);

  do_compaction ();
  CAML_INSTR_TIME (tmr, "compact/main");
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
  live = caml_stat_heap_wsz - caml_fl_cur_wsz;
  target_wsz = live + caml_percent_free * (live / 100 + 1)
                 + Wsize_bsize (Page_size);
  target_wsz = caml_clip_heap_chunk_wsz (target_wsz);

#ifdef HAS_HUGE_PAGES
  if (caml_use_huge_pages
      && Bsize_wsize (caml_stat_heap_wsz) <= HUGE_PAGE_SIZE)
    return;
#endif

  if (target_wsz < caml_stat_heap_wsz / 2){
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
    ++ caml_stat_heap_chunks;
    caml_stat_heap_wsz += Wsize_bsize (Chunk_size (chunk));
    if (caml_stat_heap_wsz > caml_stat_top_heap_wsz){
      caml_stat_top_heap_wsz = caml_stat_heap_wsz;
    }
    do_compaction ();
    CAMLassert (caml_stat_heap_chunks == 1);
    CAMLassert (Chunk_next (caml_heap_start) == NULL);
    CAMLassert (caml_stat_heap_wsz == Wsize_bsize (Chunk_size (chunk)));
    CAML_INSTR_TIME (tmr, "compact/recompact");
  }
}

void caml_compact_heap_maybe (void)
{
  /* Estimated free+garbage words in the heap:
         FW = fl_size_at_phase_change + 3 * (caml_fl_cur_wsz
                                             - caml_fl_wsz_at_phase_change)
         FW = 3 * caml_fl_cur_wsz - 2 * caml_fl_wsz_at_phase_change
     Estimated live words:      LW = caml_stat_heap_wsz - FW
     Estimated free percentage: FP = 100 * FW / LW
     We compact the heap if FP > caml_percent_max
  */
  double fw, fp;
  CAMLassert (caml_gc_phase == Phase_idle);
  if (caml_percent_max >= 1000000) return;
  if (caml_stat_major_collections < 3) return;
  if (caml_stat_heap_wsz <= 2 * caml_clip_heap_chunk_wsz (0)) return;

#ifdef HAS_HUGE_PAGES
  if (caml_use_huge_pages
      && Bsize_wsize (caml_stat_heap_wsz) <= HUGE_PAGE_SIZE)
    return;
#endif

  fw = 3.0 * caml_fl_cur_wsz - 2.0 * caml_fl_wsz_at_phase_change;
  if (fw < 0) fw = caml_fl_cur_wsz;

  if (fw >= caml_stat_heap_wsz){
    fp = 1000000.0;
  }else{
    fp = 100.0 * fw / (caml_stat_heap_wsz - fw);
    if (fp > 1000000.0) fp = 1000000.0;
  }
  caml_gc_message (0x200, "FL size at phase change = %"
                          ARCH_INTNAT_PRINTF_FORMAT "u words\n",
                   (uintnat) caml_fl_wsz_at_phase_change);
  caml_gc_message (0x200, "FL current size = %"
                          ARCH_INTNAT_PRINTF_FORMAT "u words\n",
                   (uintnat) caml_fl_cur_wsz);
  caml_gc_message (0x200, "Estimated overhead = %"
                          ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                   (uintnat) fp);
  if (fp >= caml_percent_max){
    caml_gc_message (0x200, "Automatic compaction triggered.\n");
    caml_empty_minor_heap ();  /* minor heap must be empty for compaction */
    caml_finish_major_cycle ();

    fw = caml_fl_cur_wsz;
    fp = 100.0 * fw / (caml_stat_heap_wsz - fw);
    caml_gc_message (0x200, "Measured overhead: %"
                            ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                     (uintnat) fp);
    if (fp >= caml_percent_max)
         caml_compact_heap ();
    else
         caml_gc_message (0x200, "Automatic compaction aborted.\n");

  }
}
