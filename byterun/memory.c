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

/* Allocate more memory from malloc for the heap.
   Return a block of at least the requested size (in words).
   Return NULL when out of memory.
*/
static char *expand_heap (request)
     mlsize_t request;
{
  char *mem;
  page_table_entry *new_page_table;
  asize_t new_page_table_size;
  asize_t malloc_request;
  asize_t i, more_pages;

  malloc_request = round_heap_chunk_size (Bhsize_wosize (request));
  gc_message ("Growing heap to %ldk\n",
	      (stat_heap_size + malloc_request) / 1024);
  mem = aligned_malloc (malloc_request + sizeof (heap_chunk_head),
                        sizeof (heap_chunk_head));
  if (mem == NULL){
    gc_message ("No room for growing heap\n", 0);
    return NULL;
  }
  mem += sizeof (heap_chunk_head);
  (((heap_chunk_head *) mem) [-1]).size = malloc_request;
  Assert (Wosize_bhsize (malloc_request) >= request);
  Hd_hp (mem) = Make_header (Wosize_bhsize (malloc_request), 0, Blue);

  if (mem < heap_start){
    more_pages = -Page (mem);
  }else if (Page (mem + malloc_request) > page_table_size){
    Assert (mem >= heap_end);
    more_pages = Page (mem + malloc_request) - page_table_size;
  }else{
    more_pages = 0;
  }

  if (more_pages != 0){
    new_page_table_size = page_table_size + more_pages;
    new_page_table =
      (page_table_entry *) 
        malloc(new_page_table_size * sizeof(page_table_entry));
    if (new_page_table == NULL){
      gc_message ("No room for growing page table\n", 0);
      free (mem);
      return NULL;
    }
  } else {
    new_page_table = NULL;
    new_page_table_size = 0;
  }

  if (mem < heap_start){
    Assert (more_pages != 0);
    for (i = 0; i < more_pages; i++){
      new_page_table [i] = Not_in_heap;
    }
    bcopy (page_table, new_page_table + more_pages,
           page_table_size * sizeof(page_table_entry));
    (((heap_chunk_head *) mem) [-1]).next = heap_start;
    heap_start = mem;
  }else{
    char **last;
    char *cur;

    if (mem >= heap_end) heap_end = mem + malloc_request;
    if (more_pages != 0){
      for (i = page_table_size; i < new_page_table_size; i++){
        new_page_table [i] = Not_in_heap;
      }
      bcopy (page_table, new_page_table, 
             page_table_size * sizeof(page_table_entry));
    }
    last = &heap_start;
    cur = *last;
    while (cur != NULL && cur < mem){
      last = &((((heap_chunk_head *) cur) [-1]).next);
      cur = *last;
    }
    (((heap_chunk_head *) mem) [-1]).next = cur;
    *last = mem;
  }

  if (more_pages != 0){
    free ((char *) page_table);
    page_table = new_page_table;
    page_table_size = new_page_table_size;
  }

  for (i = Page (mem); i < Page (mem + malloc_request); i++){
    page_table [i] = In_heap;
  }
  stat_heap_size += malloc_request;
  return Bp_hp (mem);
}

value alloc_shr (wosize, tag)
     mlsize_t wosize;
     tag_t tag;
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

  if (gc_phase == Phase_mark || (addr)hp >= (addr)gc_sweep_hp){
    Hd_hp (hp) = Make_header (wosize, tag, Black);
  }else{
    Hd_hp (hp) = Make_header (wosize, tag, White);
  }
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
   calls to [adjust_collector_speed].
*/
void adjust_gc_speed (mem, max)
     mlsize_t mem, max;
{
  if (max == 0) max = 1;
  if (mem > max) mem = max;
  extra_heap_memory += ((float) mem / max) * stat_heap_size;
  if (extra_heap_memory > stat_heap_size){
    extra_heap_memory = stat_heap_size;
  }
  if (extra_heap_memory > Wsize_bsize (minor_heap_size) / 2) 
    urge_major_slice ();
}

/* You must use [initialize] to store the initial value in a field of
   a shared block, unless you are sure the value is not a young block.
   A block value [v] is a shared block if and only if [Is_in_heap (v)]
   is true.
*/
/* [initialize] never calls the GC, so you may call it while an object is
   unfinished (i.e. just after a call to [alloc_shr].) */
void initialize (fp, val)
     value *fp;
     value val;
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
void modify (fp, val)
     value *fp;
     value val;
{
  Modify (fp, val);
}

char *stat_alloc (sz)
     asize_t sz;
{
  char *result = (char *) malloc (sz);

  if (result == NULL) raise_out_of_memory ();
  return result;
}

void stat_free (blk)
     char * blk;
{
  free (blk);
}

char *stat_resize (blk, sz)
     char *blk;
     asize_t sz;
{
  char *result = (char *) realloc (blk, sz);

  if (result == NULL) raise_out_of_memory ();
  return result;
}

