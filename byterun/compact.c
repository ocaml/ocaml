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

#include <string.h>

#include "config.h"
#include "freelist.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "memory.h"
#include "mlvalues.h"
#include "roots.h"
#include "weak.h"

extern int percent_free;                       /* major_gc.c */
extern void shrink_heap P((char *));           /* memory.c */

/* Encoded headers: the color is stored in the 2 least significant bits.
   (For pointer inversion, we need to distinguish headers from pointers.)
   s is a Wosize, t is a tag, and c is a color (a two-bit number)

   For the purpose of compaction, "colors" are:
   0: pointers (direct or inverted)
   1: integer or (unencoded) infix header
   2: inverted pointer for infix header
   3: integer or encoded (noninfix) header
*/
#define Make_ehd(s,t,c) (((s) << 10) | (t) << 2 | (c))
#define Whsize_ehd(h) Whsize_hd (h)
#define Wosize_ehd(h) Wosize_hd (h)
#define Tag_ehd(h) (((h) >> 2) & 0xFF)
#define Ecolor(w) ((w) & 3)

typedef unsigned long word;

static void invert_pointer_at (p)
     word *p;
{
  word q = *p;

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
                                                   Assert (Ecolor (*hp) == 3);
	if (Tag_ehd (*hp) == Closure_tag){
	  /* This is the first infix found in this block. */
          /* Save original header. */
	  *p = *hp;
          /* Link inverted infix list. */
	  Hd_val (q) = (header_t) ((word) p | 2);
	  /* Change block header's tag to Infix_tag, and change its size
             to point to the infix list. */
	  *hp = Make_ehd (Wosize_bhsize (q - val), Infix_tag, 3);
	}else{                            Assert (Tag_ehd (*hp) == Infix_tag);
	  /* Point the last of this infix list to the current first infix
             list of the block. */
          *p = (word) &Field (val, Wosize_ehd (*hp)) | 1;
	  /* Point the head of this infix list to the above. */
	  Hd_val (q) = (header_t) ((word) p | 2);
	  /* Change block header's size to point to this infix list. */
	  *hp = Make_ehd (Wosize_bhsize (q - val), Infix_tag, 3);
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

static void invert_root (v, p)
     value v;
     value *p;
{
  invert_pointer_at ((word *) p);
}

static char *compact_fl;

static void init_compact_allocate ()
{
  char *ch = heap_start;
  while (ch != NULL){
    Chunk_alloc (ch) = 0;
    ch = Chunk_next (ch);
  }
  compact_fl = heap_start;
}

static char *compact_allocate (size)
     mlsize_t size;                   /* in bytes, including header */
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
    chunk = Chunk_next (chunk);                         Assert (chunk != NULL);
  }
  adr = chunk + Chunk_alloc (chunk);
  Chunk_alloc (chunk) += size;
  return adr;
}

void compact_heap (void)
{
  char *ch, *chend;
                                               Assert (gc_phase == Phase_idle);
  gc_message ("Compacting heap...\n", 0);
  /* First pass: encode all noninfix headers. */
  {
    ch = heap_start;
    while (ch != NULL){
      header_t *p = (header_t *) ch;

      chend = ch + Chunk_size (ch);
      while ((char *) p < chend){
	header_t hd = Hd_hp (p);
	mlsize_t sz = Wosize_hd (hd);

	if (Is_blue_hd (hd)){
	  /* Free object.  Give it a string tag. */
	  Hd_hp (p) = Make_ehd (sz, String_tag, 3);
	}else{                                      Assert (Is_white_hd (hd));
	  /* Live object.  Keep its tag. */
          Hd_hp (p) = Make_ehd (sz, Tag_hd (hd), 3);
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
    ch = heap_start;
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
	  while (Ecolor (q) != 3) q = * (word *) (q & ~(unsigned long)3);
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
      value p = weak_list_head;
      word q;
      size_t sz, i;

      while (p != (value) NULL){
	q = Hd_val (p);
	while (Ecolor (q) == 0) q = * (word *) q;
	sz = Wosize_ehd (q);
	for (i = 1; i < sz; i++){
	  if (Field (p,i) != 0) invert_pointer_at (&(Field (p,i)));
	}
	p = Field (p, 0);
      }
    }
    /* Invert roots */
    do_roots (invert_root);
  }


  /* Third pass: reallocate virtually; revert pointers; decode headers.
     Rebuild infix headers. */
  {
    init_compact_allocate ();
    ch = heap_start;
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
	  word *infixes = NULL;
	  
	  while (Ecolor (q) == 0) q = * (word *) q;
	  sz = Whsize_ehd (q);
	  t = Tag_ehd (q);

	  if (t == Infix_tag){
	    /* Get the original header of this block. */
	    infixes = p + sz;
	    q = *infixes;                             Assert (Ecolor (q) == 2);
	    while (Ecolor (q) != 3) q = * (word *) (q & ~(unsigned long)3);
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
	  *p = Make_header (Wosize_whsize (sz), t, White);

	  if (infixes != NULL){
	    /* Rebuild the infix headers and revert the infix pointers. */
	    while (Ecolor ((word) infixes) != 3){
	      infixes = (word *) ((word) infixes & ~(unsigned long) 3);
	      q = *infixes;
	      while (Ecolor (q) == 2){
		word next;
		q = (word) q & ~(unsigned long) 3;
		next = * (word *) q;
		* (word *) q = (word) Val_hp ((word *) newadr + (infixes - p));
		q = next;
	      }                    Assert (Ecolor (q) == 1 || Ecolor (q) == 3);
	      *infixes = Make_header (infixes - p, Infix_tag, White);
	      infixes = (word *) q;
	    }
	  }
	  p += sz;
	}else{	                                      Assert (Ecolor (q) == 3);
          /* This is guaranteed only if compact_heap was called after a
             nonincremental major GC:       Assert (Tag_ehd (q) == String_tag);
          */
	  /* No pointers to the header and no infix header:
	     the object was free. */
	  *p = Make_header (Wosize_ehd (q), Tag_ehd (q), Blue);
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
    ch = heap_start;
    while (ch != NULL){
      word *p = (word *) ch;

      chend = ch + Chunk_size (ch);
      while ((char *) p < chend){
	word q = *p;
	if (Color_hd (q) == White){
	  size_t sz = Bhsize_hd (q);
	  char *newadr = compact_allocate (sz);  Assert (newadr <= (char *)p);
	  /* bcopy (source, destination, length) */
	  bcopy (p, newadr, sz);
	  p += Wsize_bsize (sz);
	}else{
	  Assert (Color_hd (q) == Blue);
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

    ch = heap_start;
    while (ch != NULL){
      if (Chunk_alloc (ch) != 0){
	live += Wsize_bsize (Chunk_alloc (ch));
	free += Wsize_bsize (Chunk_size (ch) - Chunk_alloc (ch));
      }
      ch = Chunk_next (ch);
    }

    /* Add up the empty chunks until there are enough, then remove the
       other empty chunks. */
    wanted = percent_free * live / 100;
    ch = heap_start;
    while (ch != NULL){
      char *next_chunk = Chunk_next (ch);  /* Chunk_next (ch) will be erased */

      if (Chunk_alloc (ch) == 0){
	if (free < wanted){
	  free += Chunk_size (ch);
	}else{
	  shrink_heap (ch);
	}
      }
      ch = next_chunk;
    }
  }

  /* Rebuild the free list. */
  {
    ch = heap_start;
    fl_reset ();
    while (ch != NULL){
      if (Chunk_size (ch) > Chunk_alloc (ch)){
	header_t *p = (header_t *) (ch + Chunk_alloc (ch));
	*p = Make_header (Wosize_bhsize (Chunk_size (ch) - Chunk_alloc (ch)),
			  0, White);
	fl_merge_block (Bp_hp (p));
      }
      ch = Chunk_next (ch);
    }
  }
  ++ stat_compactions;
  gc_message ("done.\n", 0);
}

int percent_max;

void compact_heap_maybe ()
{
  /* Estimated free words in the heap: FW = 1.5 * fl_cur_size
     Estimated live words: LW = stat_heap_size - FW
     We compact the heap if FW > percent_max / 100 * LW
  */
  float fw;
                                              Assert (gc_phase == Phase_idle);
  switch (percent_max){
  case 0:
    return;
  case 1:
    finish_major_cycle ();
    compact_heap ();
    break;
  default:
    fw = 1.5 * fl_cur_size;
    if (fw > 0.01 * percent_max * (Wsize_bsize (stat_heap_size) - fw)){
      finish_major_cycle ();
      compact_heap ();
    }
    break;
  }
}
