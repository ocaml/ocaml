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

#include <stdlib.h>
#include <string.h>
#include "fail.h"
#include "freelist.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "signals.h"

/* Allocate a block of the requested size, to be passed to
   [add_to_heap] later.
   [request] must be a multiple of [Page_size].
   [alloc_for_heap] returns NULL if the request cannot be satisfied.
   The returned pointer is a hp, but the header must be initialized by
   the caller.
*/
header_t *alloc_for_heap (asize_t request)
{
  char *mem;
  void *block;
                                              Assert (request % Page_size == 0);
  mem = aligned_malloc (request + sizeof (heap_chunk_head),
                        sizeof (heap_chunk_head), &block);
  if (mem == NULL) return NULL;
  mem += sizeof (heap_chunk_head);
  Chunk_size (mem) = request;
  Chunk_block (mem) = block;
  return (header_t *) mem;
}

/* Use this function to free a block allocated with [alloc_for_heap]
   if you don't add it with [add_to_heap].
*/
void free_for_heap (header_t *mem)
{
  free (Chunk_block (mem));
}

/* Take a block of memory as argument, which must be the result of a
   call to [alloc_for_heap], and insert it into the heap chaining.
   The contents of the block must be a sequence of valid objects and
   fragments: no space between objects and no trailing garbage.  If
   some objects are blue, they must be added to the free list by the
   caller.  All other objects must have the color [allocation_color(mem)].
   The caller must update [allocated_words] if applicable.
   Return value: 0 if no error; -1 in case of error.
*/
int add_to_heap (header_t *mem)
{
  asize_t i;
  char *m = (char *) mem;
                                     Assert (Chunk_size (m) % Page_size == 0);
#ifdef DEBUG
  /* Should check the contents of the block. */
#endif /* debug */

  /* Extend the page table as needed. */
  if (Page (m) < page_low){
    page_table_entry *block, *new_page_table;
    asize_t new_page_low = Page (m);
    asize_t new_size = page_high - new_page_low;
    
    gc_message (0x08, "Growing page table to %lu entries\n", new_size);
    block = malloc (new_size * sizeof (page_table_entry));
    if (block == NULL){
      gc_message (0x08, "No room for growing page table\n", 0);
      return -1;
    }
    new_page_table = block - new_page_low;
    for (i = new_page_low; i < page_low; i++) new_page_table [i] = Not_in_heap;
    for (i = page_low; i < page_high; i++) new_page_table [i] = page_table [i];
    free (page_table + page_low);
    page_table = new_page_table;
    page_low = new_page_low;
  }
  if (Page (m + Chunk_size (m)) > page_high){
    page_table_entry *block, *new_page_table;
    asize_t new_page_high = Page (m + Chunk_size (m));
    asize_t new_size = new_page_high - page_low;
    
    gc_message (0x08, "Growing page table to %lu entries\n", new_size);
    block = malloc (new_size * sizeof (page_table_entry));
    if (block == NULL){
      gc_message (0x08, "No room for growing page table\n", 0);
      return -1;
    }
    new_page_table = block - page_low;
    for (i = page_low; i < page_high; i++) new_page_table [i] = page_table [i];
    for (i = page_high; i < new_page_high; i++){
      new_page_table [i] = Not_in_heap;
    }
    free (page_table + page_low);
    page_table = new_page_table;
    page_high = new_page_high;
  }

  /* Update the heap bounds as needed. */
  if (m < heap_start) heap_start = m;
  if (m + Chunk_size (m) > heap_end) heap_end = m + Chunk_size (m);

  /* Mark the pages as being in the heap. */
  for (i = Page (m); i < Page (m + Chunk_size (m)); i++){
    page_table [i] = In_heap;
  }

  /* Chain this heap block. */
  if (m < heap_start){
    Chunk_next (m) = heap_start;
    heap_start = m;
  }else{
    char **last = &heap_start;
    char *cur = *last;

    while (cur != NULL && cur < m){
      last = &(Chunk_next (cur));
      cur = *last;
    }
    Chunk_next (m) = cur;
    *last = m;
  }
  stat_heap_size += Chunk_size (m);
  return 0;
}

/* Allocate more memory from malloc for the heap.
   Return a blue block of at least the requested size (in words).
   The caller must insert the block into the free list.
   Return NULL when out of memory.
*/
static char *expand_heap (mlsize_t request)
{
  header_t *mem;
  asize_t malloc_request;

  malloc_request = round_heap_chunk_size (Bhsize_wosize (request));
  gc_message (0x04, "Growing heap to %luk bytes\n",
              (stat_heap_size + malloc_request) / 1024);
  mem = alloc_for_heap (malloc_request);
  if (mem == NULL){
    gc_message (0x04, "No room for growing heap\n", 0);
    return NULL;
  }
  Assert (Wosize_bhsize (malloc_request) >= request);
  Hd_hp (mem) = Make_header (Wosize_bhsize (malloc_request), 0, Blue);

  if (add_to_heap (mem) != 0){
    free (mem);
    return NULL;
  }
  return Bp_hp (mem);
}

/* Remove the heap chunk [chunk] from the heap and give the memory back
   to [free].
*/
void shrink_heap (char *chunk)
{
  char **cp;
  int i;

  /* Never deallocate the first block, because heap_start is both the
     first block and the base address for page numbers, and we don't
     want to shift the page table, it's too messy (see above).
     It will never happen anyway, because of the way compaction works.
     (see compact.c)
  */
  if (chunk == heap_start) return;

  stat_heap_size -= Chunk_size (chunk);
  gc_message (0x04, "Shrinking heap to %luk bytes\n", stat_heap_size / 1024);

#ifdef DEBUG
  {
    mlsize_t i;
    for (i = 0; i < Wsize_bsize (Chunk_size (chunk)); i++){
      ((value *) chunk) [i] = not_random ();
    }
  }
#endif

  /* Remove [chunk] from the list of chunks. */
  cp = &heap_start;
  while (*cp != chunk) cp = &(Chunk_next (*cp));
  *cp = Chunk_next (chunk);

  /* Remove the pages of [chunk] from the page table. */
  for (i = Page (chunk); i < Page (chunk + Chunk_size (chunk)); i++){
    page_table [i] = Not_in_heap;
  }

  /* Free the [malloc]ed block that contains [chunk]. */
  free (Chunk_block (chunk));
}

color_t allocation_color (void *hp)
{
  if (gc_phase == Phase_mark
      || (gc_phase == Phase_sweep && (addr)hp >= (addr)gc_sweep_hp)){
    return Black;
  }else{
    Assert (gc_phase == Phase_idle
            || (gc_phase == Phase_sweep && (addr)hp < (addr)gc_sweep_hp));
    return White;
  }
}

value alloc_shr (mlsize_t wosize, tag_t tag)
{
  char *hp, *new_block;

  hp = fl_allocate (wosize);
  if (hp == NULL){
    new_block = expand_heap (wosize);
    if (new_block == NULL) {
      if (in_minor_collection)
        fatal_error ("Fatal error: out of memory.\n");
      else
        raise_out_of_memory ();
    }
    fl_add_block (new_block);
    hp = fl_allocate (wosize);
  }

  Assert (Is_in_heap (Val_hp (hp)));

  /* Inline expansion of allocation_color. */
  if (gc_phase == Phase_mark
      || (gc_phase == Phase_sweep && (addr)hp >= (addr)gc_sweep_hp)){
    Hd_hp (hp) = Make_header (wosize, tag, Black);
  }else{
    Assert (gc_phase == Phase_idle
            || (gc_phase == Phase_sweep && (addr)hp < (addr)gc_sweep_hp));
    Hd_hp (hp) = Make_header (wosize, tag, White);
  }
  Assert (Hd_hp (hp) == Make_header (wosize, tag, allocation_color (hp)));
  allocated_words += Whsize_wosize (wosize);
  if (allocated_words > Wsize_bsize (minor_heap_size)) urge_major_slice ();
  return Val_hp (hp);
}

/* Use this function to tell the major GC to speed up when you use
   finalized objects to automatically deallocate extra-heap objects.
   The GC will do at least one cycle every [max] allocated words;
   [mem] is the number of words allocated this time.
   Note that only [mem/max] is relevant.  You can use numbers of bytes
   (or kilobytes, ...) instead of words.  You can change units between
   calls to [adjust_gc_speed].
*/
void adjust_gc_speed (mlsize_t mem, mlsize_t max)
{
  if (max == 0) max = 1;
  if (mem > max) mem = max;
  extra_heap_memory += (double) mem / (double) max;
  if (extra_heap_memory > 1.0){
    extra_heap_memory = 1.0;
    urge_major_slice ();
  }
  if (extra_heap_memory > (double) Wsize_bsize (minor_heap_size)
                          / 2.0 / (double) stat_heap_size) {
    urge_major_slice ();
  }
}

/* You must use [initialize] to store the initial value in a field of
   a shared block, unless you are sure the value is not a young block.
   A block value [v] is a shared block if and only if [Is_in_heap (v)]
   is true.
*/
/* [initialize] never calls the GC, so you may call it while an object is
   unfinished (i.e. just after a call to [alloc_shr].) */
void initialize (value *fp, value val)
{
  *fp = val;
  Assert (Is_in_heap (fp));
  if (Is_block (val) && Is_young (val)){
    *ref_table_ptr++ = fp;
    if (ref_table_ptr >= ref_table_limit){
      realloc_ref_table ();
    }
  }
}

/* You must use [modify] to change a field of an existing shared block,
   unless you are sure the value being overwritten is not a shared block and
   the value being written is not a young block. */
/* [modify] never calls the GC. */
void modify (value *fp, value val)
{
  Modify (fp, val);
}

void * stat_alloc (asize_t sz)
{
  void * result = malloc (sz);

  if (result == NULL) raise_out_of_memory ();
  return result;
}

void stat_free (void * blk)
{
  free (blk);
}

void * stat_resize (void * blk, asize_t sz)
{
  void * result = realloc (blk, sz);

  if (result == NULL) raise_out_of_memory ();
  return result;
}

