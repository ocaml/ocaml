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

#include <stdlib.h>
#include <string.h>
#include "fail.h"
#include "freelist.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "memory.h"
#include "major_gc.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "signals.h"

#ifdef USE_MMAP_INSTEAD_OF_MALLOC
extern char * caml_aligned_mmap (asize_t size, int modulo, void ** block);
extern void caml_aligned_munmap (char * addr, asize_t size);
#endif

/* Allocate a block of the requested size, to be passed to
   [caml_add_to_heap] later.
   [request] must be a multiple of [Page_size].
   [caml_alloc_for_heap] returns NULL if the request cannot be satisfied.
   The returned pointer is a hp, but the header must be initialized by
   the caller.
*/
char *caml_alloc_for_heap (asize_t request)
{
  char *mem;
  void *block;
                                              Assert (request % Page_size == 0);
#ifdef USE_MMAP_INSTEAD_OF_MALLOC
  mem = caml_aligned_mmap (request + sizeof (heap_chunk_head),
                           sizeof (heap_chunk_head), &block);
#else
  mem = caml_aligned_malloc (request + sizeof (heap_chunk_head),
                             sizeof (heap_chunk_head), &block);
#endif
  if (mem == NULL) return NULL;
  mem += sizeof (heap_chunk_head);
  Chunk_size (mem) = request;
  Chunk_block (mem) = block;
  return mem;
}

/* Use this function to free a block allocated with [caml_alloc_for_heap]
   if you don't add it with [caml_add_to_heap].
*/
void caml_free_for_heap (char *mem)
{
#ifdef USE_MMAP_INSTEAD_OF_MALLOC
  caml_aligned_munmap (Chunk_block (mem),
                       Chunk_size (mem) + sizeof (heap_chunk_head));
#else
  free (Chunk_block (mem));
#endif
}

/* Take a chunk of memory as argument, which must be the result of a
   call to [caml_alloc_for_heap], and insert it into the heap chaining.
   The contents of the chunk must be a sequence of valid blocks and
   fragments: no space between blocks and no trailing garbage.  If
   some blocks are blue, they must be added to the free list by the
   caller.  All other blocks must have the color [caml_allocation_color(mem)].
   The caller must update [caml_allocated_words] if applicable.
   Return value: 0 if no error; -1 in case of error.
*/
int caml_add_to_heap (char *m)
{
  asize_t i;
                                     Assert (Chunk_size (m) % Page_size == 0);
#ifdef DEBUG
  /* Should check the contents of the block. */
#endif /* debug */

  caml_gc_message (0x04, "Growing heap to %luk bytes\n",
                   (caml_stat_heap_size + Chunk_size (m)) / 1024);

  /* Extend the page table as needed. */
  if (Page (m) < caml_page_low){
    page_table_entry *block, *new_page_table;
    asize_t new_page_low = Page (m);
    asize_t new_size = caml_page_high - new_page_low;
    
    caml_gc_message (0x08, "Growing page table to %lu entries\n", new_size);
    block = malloc (new_size * sizeof (page_table_entry));
    if (block == NULL){
      caml_gc_message (0x08, "No room for growing page table\n", 0);
      return -1;
    }
    new_page_table = block - new_page_low;
    for (i = new_page_low; i < caml_page_low; i++){
      new_page_table [i] = Not_in_heap;
    }
    for (i = caml_page_low; i < caml_page_high; i++){
      new_page_table [i] = caml_page_table [i];
    }
    free (caml_page_table + caml_page_low);
    caml_page_table = new_page_table;
    caml_page_low = new_page_low;
  }
  if (Page (m + Chunk_size (m)) > caml_page_high){
    page_table_entry *block, *new_page_table;
    asize_t new_page_high = Page (m + Chunk_size (m));
    asize_t new_size = new_page_high - caml_page_low;
    
    caml_gc_message (0x08, "Growing page table to %lu entries\n", new_size);
    block = malloc (new_size * sizeof (page_table_entry));
    if (block == NULL){
      caml_gc_message (0x08, "No room for growing page table\n", 0);
      return -1;
    }
    new_page_table = block - caml_page_low;
    for (i = caml_page_low; i < caml_page_high; i++){
      new_page_table [i] = caml_page_table [i];
    }
    for (i = caml_page_high; i < new_page_high; i++){
      new_page_table [i] = Not_in_heap;
    }
    free (caml_page_table + caml_page_low);
    caml_page_table = new_page_table;
    caml_page_high = new_page_high;
  }

  /* Mark the pages as being in the heap. */
  for (i = Page (m); i < Page (m + Chunk_size (m)); i++){
    caml_page_table [i] = In_heap;
  }

  /* Chain this heap chunk. */
  {
    char **last = &caml_heap_start;
    char *cur = *last;

    while (cur != NULL && cur < m){
      last = &(Chunk_next (cur));
      cur = *last;
    }
    Chunk_next (m) = cur;
    *last = m;

    ++ caml_stat_heap_chunks;
  }

  /* Update the heap bounds as needed. */
  /* already done:   if (m < caml_heap_start) heap_start = m; */
  if (m + Chunk_size (m) > caml_heap_end) caml_heap_end = m + Chunk_size (m);

  caml_stat_heap_size += Chunk_size (m);
  if (caml_stat_heap_size > caml_stat_top_heap_size){
    caml_stat_top_heap_size = caml_stat_heap_size;
  }
  return 0;
}

/* Allocate more memory from malloc for the heap.
   Return a blue block of at least the requested size (in words).
   The caller must insert the block into the free list.
   The request must be less than or equal to Max_wosize.
   Return NULL when out of memory.
*/
static char *expand_heap (mlsize_t request)
{
  char *mem;
  asize_t malloc_request;

  malloc_request = caml_round_heap_chunk_size (Bhsize_wosize (request));
  mem = caml_alloc_for_heap (malloc_request);
  if (mem == NULL){
    caml_gc_message (0x04, "No room for growing heap\n", 0);
    return NULL;
  }
  Assert (Wosize_bhsize (malloc_request) >= request);
  Hd_hp (mem) = Make_header (Wosize_bhsize (malloc_request), 0, Caml_blue);

  if (caml_add_to_heap (mem) != 0){
    caml_free_for_heap (mem);
    return NULL;
  }
  return Bp_hp (mem);
}

/* Remove the heap chunk [chunk] from the heap and give the memory back
   to [free].
*/
void caml_shrink_heap (char *chunk)
{
  char **cp;
  asize_t i;

  /* Never deallocate the first block, because caml_heap_start is both the
     first block and the base address for page numbers, and we don't
     want to shift the page table, it's too messy (see above).
     It will never happen anyway, because of the way compaction works.
     (see compact.c)
  */
  if (chunk == caml_heap_start) return;

  caml_stat_heap_size -= Chunk_size (chunk);
  caml_gc_message (0x04, "Shrinking heap to %luk bytes\n",
                   caml_stat_heap_size / 1024);

#ifdef DEBUG
  {
    mlsize_t i;
    for (i = 0; i < Wsize_bsize (Chunk_size (chunk)); i++){
      ((value *) chunk) [i] = Debug_free_shrink;
    }
  }
#endif

  -- caml_stat_heap_chunks;

  /* Remove [chunk] from the list of chunks. */
  cp = &caml_heap_start;
  while (*cp != chunk) cp = &(Chunk_next (*cp));
  *cp = Chunk_next (chunk);

  /* Remove the pages of [chunk] from the page table. */
  for (i = Page (chunk); i < Page (chunk + Chunk_size (chunk)); i++){
    caml_page_table [i] = Not_in_heap;
  }

  /* Free the [malloc] block that contains [chunk]. */
  caml_free_for_heap (chunk);
}

color_t caml_allocation_color (void *hp)
{
  if (caml_gc_phase == Phase_mark
      || (caml_gc_phase == Phase_sweep && (addr)hp >= (addr)caml_gc_sweep_hp)){
    return Caml_black;
  }else{
    Assert (caml_gc_phase == Phase_idle
            || (caml_gc_phase == Phase_sweep
                && (addr)hp < (addr)caml_gc_sweep_hp));
    return Caml_white;
  }
}

CAMLexport value caml_alloc_shr (mlsize_t wosize, tag_t tag)
{
  char *hp, *new_block;

  if (wosize > Max_wosize) caml_raise_out_of_memory ();
  hp = caml_fl_allocate (wosize);
  if (hp == NULL){
    new_block = expand_heap (wosize);
    if (new_block == NULL) {
      if (caml_in_minor_collection)
        caml_fatal_error ("Fatal error: out of memory.\n");
      else
        caml_raise_out_of_memory ();
    }
    caml_fl_add_block (new_block);
    hp = caml_fl_allocate (wosize);
  }

  Assert (Is_in_heap (Val_hp (hp)));

  /* Inline expansion of caml_allocation_color. */
  if (caml_gc_phase == Phase_mark
      || (caml_gc_phase == Phase_sweep && (addr)hp >= (addr)caml_gc_sweep_hp)){
    Hd_hp (hp) = Make_header (wosize, tag, Caml_black);
  }else{
    Assert (caml_gc_phase == Phase_idle
            || (caml_gc_phase == Phase_sweep
                && (addr)hp < (addr)caml_gc_sweep_hp));
    Hd_hp (hp) = Make_header (wosize, tag, Caml_white);
  }
  Assert (Hd_hp (hp) == Make_header (wosize, tag, caml_allocation_color (hp)));
  caml_allocated_words += Whsize_wosize (wosize);
  if (caml_allocated_words > Wsize_bsize (caml_minor_heap_size)){
    caml_urge_major_slice ();
  }
#ifdef DEBUG
  {
    unsigned long i;
    for (i = 0; i < wosize; i++){
      Field (Val_hp (hp), i) = Debug_uninit_major;
    }
  }
#endif
  return Val_hp (hp);
}

/* Dependent memory is all memory blocks allocated out of the heap
   that depend on the GC (and finalizers) for deallocation.
   For the GC to take dependent memory in its automatic speed setting,
   you must call [caml_alloc_dependent_memory] when you alloate some
   dependent memory, and [caml_free_dependent_memory] when you
   free it.  In both cases, you pass as argument the size of the
   block being allocated or freed.
*/
CAMLexport void caml_alloc_dependent_memory (mlsize_t nbytes)
{
  caml_dependent_size += nbytes / sizeof (value);
  caml_dependent_allocated += nbytes / sizeof (value);
}

CAMLexport void caml_free_dependent_memory (mlsize_t nbytes)
{
  if (caml_dependent_size < nbytes / sizeof (value)){
    caml_dependent_size = 0;
  }else{
    caml_dependent_size -= nbytes / sizeof (value);
  }
}

/* Use this function to tell the major GC to speed up when you use
   finalized blocks to automatically deallocate resources (other
   than memory). The GC will do at least one cycle every [max]
   allocated resources; [res] is the number of resources allocated
   this time.
   Note that only [res/max] is relevant.  The units (and kind of
   resource) can change between calls to [caml_adjust_gc_speed].
*/
CAMLexport void caml_adjust_gc_speed (mlsize_t res, mlsize_t max)
{
  if (max == 0) max = 1;
  if (res > max) res = max;
  caml_extra_heap_resources += (double) res / (double) max;
  if (caml_extra_heap_resources > 1.0){
    caml_extra_heap_resources = 1.0;
    caml_urge_major_slice ();
  }
  if (caml_extra_heap_resources
           > (double) Wsize_bsize (caml_minor_heap_size) / 2.0
             / (double) Wsize_bsize (caml_stat_heap_size)) {
    caml_urge_major_slice ();
  }
}

/* You must use [caml_initialize] to store the initial value in a field of
   a shared block, unless you are sure the value is not a young block.
   A block value [v] is a shared block if and only if [Is_in_heap (v)]
   is true.
*/
/* [caml_initialize] never calls the GC, so you may call it while an block is
   unfinished (i.e. just after a call to [caml_alloc_shr].) */
void caml_initialize (value *fp, value val)
{
  *fp = val;
  if (Is_block (val) && Is_young (val) && Is_in_heap (fp)){
    *caml_ref_table_ptr++ = fp;
    if (caml_ref_table_ptr >= caml_ref_table_limit){
      caml_realloc_ref_table ();
    }
  }
}

/* You must use [caml_modify] to change a field of an existing shared block,
   unless you are sure the value being overwritten is not a shared block and
   the value being written is not a young block. */
/* [caml_modify] never calls the GC. */
void caml_modify (value *fp, value val)
{
  Modify (fp, val);
}

CAMLexport void * caml_stat_alloc (asize_t sz)
{
  void * result = malloc (sz);

  /* malloc() may return NULL if size is 0 */
  if (result == NULL && sz != 0) caml_raise_out_of_memory ();
#ifdef DEBUG
  memset (result, Debug_uninit_stat, sz);
#endif
  return result;
}

CAMLexport void caml_stat_free (void * blk)
{
  free (blk);
}

CAMLexport void * caml_stat_resize (void * blk, asize_t sz)
{
  void * result = realloc (blk, sz);

  if (result == NULL) caml_raise_out_of_memory ();
  return result;
}
