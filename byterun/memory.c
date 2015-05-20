/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

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

int caml_huge_fallback_count = 0;
/* Number of times that mmapping big pages fails and we fell back to small
   pages. This counter is available to the program through
   [Gc.huge_fallback_count].
*/

extern uintnat caml_percent_free;                   /* major_gc.c */

/* Page table management */

#define Page(p) ((uintnat) (p) >> Page_log)
#define Page_mask ((uintnat) -1 << Page_log)

#ifdef ARCH_SIXTYFOUR

/* 64-bit implementation:
   The page table is represented sparsely as a hash table
   with linear probing */

struct page_table {
  mlsize_t size;                /* size == 1 << (wordsize - shift) */
  int shift;
  mlsize_t mask;                /* mask == size - 1 */
  mlsize_t occupancy;
  uintnat * entries;            /* [size]  */
};

static struct page_table caml_page_table;

/* Page table entries are the logical 'or' of
   - the key: address of a page (low Page_log bits = 0)
   - the data: a 8-bit integer */

#define Page_entry_matches(entry,addr) \
  ((((entry) ^ (addr)) & Page_mask) == 0)

/* Multiplicative Fibonacci hashing
   (Knuth, TAOCP vol 3, section 6.4, page 518).
   HASH_FACTOR is (sqrt(5) - 1) / 2 * 2^wordsize. */
#ifdef ARCH_SIXTYFOUR
#define HASH_FACTOR 11400714819323198486UL
#else
#define HASH_FACTOR 2654435769UL
#endif
#define Hash(v) (((v) * HASH_FACTOR) >> caml_page_table.shift)

int caml_page_table_lookup(void * addr)
{
  uintnat h, e;

  h = Hash(Page(addr));
  /* The first hit is almost always successful, so optimize for this case */
  e = caml_page_table.entries[h];
  if (Page_entry_matches(e, (uintnat)addr)) return e & 0xFF;
  while(1) {
    if (e == 0) return 0;
    h = (h + 1) & caml_page_table.mask;
    e = caml_page_table.entries[h];
    if (Page_entry_matches(e, (uintnat)addr)) return e & 0xFF;
  }
}

int caml_page_table_initialize(mlsize_t bytesize)
{
  uintnat pagesize = Page(bytesize);

  caml_page_table.size = 1;
  caml_page_table.shift = 8 * sizeof(uintnat);
  /* Aim for initial load factor between 1/4 and 1/2 */
  while (caml_page_table.size < 2 * pagesize) {
    caml_page_table.size <<= 1;
    caml_page_table.shift -= 1;
  }
  caml_page_table.mask = caml_page_table.size - 1;
  caml_page_table.occupancy = 0;
  caml_page_table.entries = calloc(caml_page_table.size, sizeof(uintnat));
  if (caml_page_table.entries == NULL)
    return -1;
  else
    return 0;
}

static int caml_page_table_resize(void)
{
  struct page_table old = caml_page_table;
  uintnat * new_entries;
  uintnat i, h;

  caml_gc_message (0x08, "Growing page table to %lu entries\n",
                   caml_page_table.size);

  new_entries = calloc(2 * old.size, sizeof(uintnat));
  if (new_entries == NULL) {
    caml_gc_message (0x08, "No room for growing page table\n", 0);
    return -1;
  }

  caml_page_table.size = 2 * old.size;
  caml_page_table.shift = old.shift - 1;
  caml_page_table.mask = caml_page_table.size - 1;
  caml_page_table.occupancy = old.occupancy;
  caml_page_table.entries = new_entries;

  for (i = 0; i < old.size; i++) {
    uintnat e = old.entries[i];
    if (e == 0) continue;
    h = Hash(Page(e));
    while (caml_page_table.entries[h] != 0)
      h = (h + 1) & caml_page_table.mask;
    caml_page_table.entries[h] = e;
  }

  free(old.entries);
  return 0;
}

static int caml_page_table_modify(uintnat page, int toclear, int toset)
{
  uintnat h;

  Assert ((page & ~Page_mask) == 0);

  /* Resize to keep load factor below 1/2 */
  if (caml_page_table.occupancy * 2 >= caml_page_table.size) {
    if (caml_page_table_resize() != 0) return -1;
  }
  h = Hash(Page(page));
  while (1) {
    if (caml_page_table.entries[h] == 0) {
      caml_page_table.entries[h] = page | toset;
      caml_page_table.occupancy++;
      break;
    }
    if (Page_entry_matches(caml_page_table.entries[h], page)) {
      caml_page_table.entries[h] =
        (caml_page_table.entries[h] & ~toclear) | toset;
      break;
    }
    h = (h + 1) & caml_page_table.mask;
  }
  return 0;
}

#else

/* 32-bit implementation:
   The page table is represented as a 2-level array of unsigned char */

CAMLexport unsigned char * caml_page_table[Pagetable1_size];
static unsigned char caml_page_table_empty[Pagetable2_size] = { 0, };

int caml_page_table_initialize(mlsize_t bytesize)
{
  int i;
  for (i = 0; i < Pagetable1_size; i++)
    caml_page_table[i] = caml_page_table_empty;
  return 0;
}

static int caml_page_table_modify(uintnat page, int toclear, int toset)
{
  uintnat i = Pagetable_index1(page);
  uintnat j = Pagetable_index2(page);

  if (caml_page_table[i] == caml_page_table_empty) {
    unsigned char * new_tbl = calloc(Pagetable2_size, 1);
    if (new_tbl == 0) return -1;
    caml_page_table[i] = new_tbl;
  }
  caml_page_table[i][j] = (caml_page_table[i][j] & ~toclear) | toset;
  return 0;
}

#endif

int caml_page_table_add(int kind, void * start, void * end)
{
  uintnat pstart = (uintnat) start & Page_mask;
  uintnat pend = ((uintnat) end - 1) & Page_mask;
  uintnat p;

  for (p = pstart; p <= pend; p += Page_size)
    if (caml_page_table_modify(p, 0, kind) != 0) return -1;
  return 0;
}

int caml_page_table_remove(int kind, void * start, void * end)
{
  uintnat pstart = (uintnat) start & Page_mask;
  uintnat pend = ((uintnat) end - 1) & Page_mask;
  uintnat p;

  for (p = pstart; p <= pend; p += Page_size)
    if (caml_page_table_modify(p, kind, 0) != 0) return -1;
  return 0;
}


#ifdef MMAP_INTERVAL

static void *raw_heap_start = NULL, *raw_heap_end = NULL;

void *caml_mmap_heap (void *addr, size_t length, int prot, int flags)
{
  void *result;
  result = mmap (addr, length, prot,
                 flags | MAP_PRIVATE | MAP_ANONYMOUS | Huge_pages_flag,
                 -1, 0);
#ifdef HAS_HUGE_PAGES
  if (result == MAP_FAILED){
    result = mmap (addr, length, prot,
                   flags | MAP_PRIVATE | MAP_ANONYMOUS,
                   -1, 0);
    if (result != MAP_FAILED) ++caml_huge_fallback_count;
  }
#endif
  return result;
}

int caml_init_alloc_for_heap (void)
{
  char *block;

  block = caml_mmap_heap (NULL, HEAP_INTERVAL_SIZE, PROT_NONE, MAP_NORESERVE);
  if (block == MAP_FAILED) return -1;
  raw_heap_start = raw_heap_end = block;
  caml_young_end = block + HEAP_INTERVAL_SIZE;
  caml_young_start = caml_young_ptr = caml_young_end;
  return 0;
}

char *caml_alloc_for_heap (asize_t request)
{
  uintnat size = Round_mmap_size (sizeof (heap_chunk_head) + request);
  void *block;
  char *mem;

  CAMLassert (raw_heap_end != NULL);
  if ((char *) raw_heap_end + size
      > (char *) raw_heap_start + HEAP_INTERVAL_SIZE / 2){
    /* No room for growing heap. */
    return NULL;
  }
  block = caml_mmap_heap (raw_heap_end, size, PROT_READ | PROT_WRITE,
                          MAP_FIXED);
  if (block == MAP_FAILED) return NULL;
  raw_heap_end = (char *) block + size;
  mem = (char *) block + sizeof (heap_chunk_head);
  Chunk_size (mem) = size - sizeof (heap_chunk_head);
  Chunk_block (mem) = block;
  return mem;
}

void caml_free_for_heap (char *mem)
{
  char *block;

  CAMLassert (mem + Chunk_size (mem) == raw_heap_end);
  raw_heap_end = mem - sizeof (heap_chunk_head);
  block = caml_mmap_heap (raw_heap_end,
                          Chunk_size (mem) + sizeof (heap_chunk_head),
                          PROT_NONE, MAP_FIXED | MAP_NORESERVE);
  CAMLassert (block == raw_heap_end);
}

/* Reduce the size of a heap chunk. This chunk must be the last one. */
void caml_shrink_chunk (char *chunk, uintnat req_bsz)
{
  uintnat old_size = Chunk_size (chunk) + sizeof (heap_chunk_head);
  uintnat new_size = Round_mmap_size (req_bsz + sizeof (heap_chunk_head));
  void *block;

  CAMLassert (chunk + Chunk_size (chunk) == raw_heap_end);
  CAMLassert (new_size <= old_size);
  if (new_size < old_size){
    uintnat diff = old_size - new_size;
    raw_heap_end -= diff;
    block = caml_mmap_heap (raw_heap_end, diff, PROT_NONE,
                            MAP_FIXED | MAP_NORESERVE);
    CAMLassert (block == raw_heap_end);
    Chunk_size (chunk) = new_size - sizeof (heap_chunk_head);
    caml_stat_heap_size -= diff;
  }
}

#elif defined(MMAP_HUGE_PAGES)

int caml_init_alloc_for_heap (void)
{
  return 0;
}

char *caml_alloc_for_heap (asize_t request)
{
  uintnat size = Round_mmap_size (sizeof (heap_chunk_head) + request);
  void *block;
  char *mem;
  block = mmap (NULL, size, PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS | MAP_HUGETLB, -1, 0);
  if (block == MAP_FAILED) return NULL;
  mem = (char *) block + sizeof (heap_chunk_head);
  Chunk_size (mem) = size - sizeof (heap_chunk_head);
  Chunk_block (mem) = block;
  return mem;
}

void caml_free_for_heap (char *mem)
{
  munmap (Chunk_block (mem), Chunk_size (mem) + sizeof (heap_chunk_head));
}

#else /* neither MMAP_INTERVAL nor MMAP_HUGE_PAGES */

/* Initialize the [alloc_for_heap] system.
   This function must be called exactly once, and it must be called
   before the first call to [alloc_for_heap].
   It returns 0 on success and -1 on failure.
*/
int caml_init_alloc_for_heap (void)
{
  return 0;
}

/* Allocate a block of the requested size, to be passed to
   [caml_add_to_heap] later.
   [request] will be rounded up to some implementation-dependent size.
   The caller must use [Chunk_size] on the result to recover the actual
   size.
   Return NULL if the request cannot be satisfied. The returned pointer
   is a hp, but the header (and the contents) must be initialized by the
   caller.
*/
char *caml_alloc_for_heap (asize_t request)
{
  char *mem;
  void *block;

  request = ((request + Page_size - 1) >> Page_log) << Page_log;
  mem = caml_aligned_malloc (request + sizeof (heap_chunk_head),
                             sizeof (heap_chunk_head), &block);
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
  free (Chunk_block (mem));
}
#endif /* MMAP_INTERVAL or MMAP_HUGE_PAGES or none */

/* Take a chunk of memory as argument, which must be the result of a
   call to [caml_alloc_for_heap], and insert it into the heap chaining.
   The contents of the chunk must be a sequence of valid blocks and
   fragments: no space between blocks and no trailing garbage.  If
   some blocks are blue, they must be added to the free list by the
   caller.  All other blocks must have the color [caml_allocation_color(m)].
   The caller must update [caml_allocated_words] if applicable.
   Return value: 0 if no error; -1 in case of error.

   See also: caml_compact_heap, which duplicates most of this function.
*/
int caml_add_to_heap (char *m)
{
#ifdef DEBUG
  /* Should check the contents of the block. */
#endif /* DEBUG */

  caml_gc_message (0x04, "Growing heap to %luk bytes\n",
                   (caml_stat_heap_size + Chunk_size (m)) / 1024);

  /* Register block in page table */
  if (caml_page_table_add(In_heap, m, m + Chunk_size(m)) != 0)
    return -1;

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

  caml_stat_heap_size += Chunk_size (m);
  if (caml_stat_heap_size > caml_stat_top_heap_size){
    caml_stat_top_heap_size = caml_stat_heap_size;
  }
  return 0;
}

/* Allocate more memory from malloc for the heap.
   Return a blue block of at least the requested size.
   The blue block is chained to a sequence of blue blocks (through their
   field 0); the last block of the chain is pointed by field 1 of the
   first.  There may be a fragment after the last block.
   The caller must insert the blocks into the free list.
   The request must be less than or equal to Max_wosize.
   Return NULL when out of memory.
*/
static char *expand_heap (mlsize_t request)
{
  char *mem, *hp, *prev;
  asize_t over_request, malloc_request, remain;

  Assert (request <= Max_wosize);
  over_request = request + request / 100 * caml_percent_free;
  malloc_request = caml_clip_heap_chunk_size (Bhsize_wosize (over_request));
  mem = caml_alloc_for_heap (malloc_request);
  if (mem == NULL){
    caml_gc_message (0x04, "No room for growing heap\n", 0);
    return NULL;
  }
  remain = Chunk_size (mem);
  prev = hp = mem;
  /* FIXME find a way to do this with a call to caml_make_free_blocks */
  while (Wosize_bhsize (remain) > Max_wosize){
    Hd_hp (hp) = Make_header (Max_wosize, 0, Caml_blue);
#ifdef DEBUG
    caml_set_fields (Bp_hp (hp), 0, Debug_free_major);
#endif
    hp += Bhsize_wosize (Max_wosize);
    remain -= Bhsize_wosize (Max_wosize);
    Field (Op_hp (mem), 1) = Field (Op_hp (prev), 0) = (value) Op_hp (hp);
    prev = hp;
  }
  if (remain > 1){
    Hd_hp (hp) = Make_header (Wosize_bhsize (remain), 0, Caml_blue);
#ifdef DEBUG
    caml_set_fields (Bp_hp (hp), 0, Debug_free_major);
#endif
    Field (Op_hp (mem), 1) = Field (Op_hp (prev), 0) = (value) Op_hp (hp);
    Field (Op_hp (hp), 0) = (value) NULL;
  }else{
    Field (Op_hp (prev), 0) = (value) NULL;
    if (remain == 1) Hd_hp (hp) = Make_header (0, 0, Caml_white);
  }
  Assert (Wosize_hp (mem) >= request);
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

  /* Never deallocate the first chunk, because caml_heap_start is both the
     first block and the base address for page numbers, and we don't
     want to shift the page table, it's too messy (see above).
     It will never happen anyway, because of the way compaction works.
     (see compact.c)
  */
  if (chunk == caml_heap_start) return;

  caml_stat_heap_size -= Chunk_size (chunk);
  caml_gc_message (0x04, "Shrinking heap to %luk bytes\n",
                   (unsigned long) caml_stat_heap_size / 1024);

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
  caml_page_table_remove(In_heap, chunk, chunk + Chunk_size (chunk));

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
    caml_fl_add_blocks (new_block);
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
    CAML_INSTR_INT ("urge_major/alloc_shr@", 1);
    caml_urge_major_slice ();
  }
#ifdef DEBUG
  {
    uintnat i;
    for (i = 0; i < wosize; i++){
      Field (Val_hp (hp), i) = Debug_uninit_major;
    }
  }
#endif
  return Val_hp (hp);
}

/* Dependent memory is all memory blocks allocated out of the heap
   that depend on the GC (and finalizers) for deallocation.
   For the GC to take dependent memory into account when computing
   its automatic speed setting,
   you must call [caml_alloc_dependent_memory] when you alloate some
   dependent memory, and [caml_free_dependent_memory] when you
   free it.  In both cases, you pass as argument the size (in bytes)
   of the block being allocated or freed.
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
    CAML_INSTR_INT ("urge_major/adjust_gc_speed_1@", 1);
    caml_extra_heap_resources = 1.0;
    caml_urge_major_slice ();
  }
  if (caml_extra_heap_resources
           > (double) Wsize_bsize (caml_minor_heap_size) / 2.0
             / (double) Wsize_bsize (caml_stat_heap_size)) {
    CAML_INSTR_INT ("urge_major/adjust_gc_speed_2@", 1);
    caml_urge_major_slice ();
  }
}

/* You must use [caml_initialize] to store the initial value in a field of
   a shared block, unless you are sure the value is not a young block.
   A block value [v] is a shared block if and only if [Is_in_heap (v)]
   is true.
*/
/* [caml_initialize] never calls the GC, so you may call it while a block is
   unfinished (i.e. just after a call to [caml_alloc_shr].) */
/* PR#6084 workaround: define it as a weak symbol */
CAMLexport CAMLweakdef void caml_initialize (value *fp, value val)
{
  CAMLassert(Is_in_heap(fp));
  *fp = val;
  if (Is_block (val) && Is_young (val)) {
    if (caml_ref_table.ptr >= caml_ref_table.limit){
      caml_realloc_ref_table (&caml_ref_table);
    }
    *caml_ref_table.ptr++ = fp;
  }
}

/* You must use [caml_modify] to change a field of an existing shared block,
   unless you are sure the value being overwritten is not a shared block and
   the value being written is not a young block. */
/* [caml_modify] never calls the GC. */
/* [caml_modify] can also be used to do assignment on data structures that are
   in the minor heap instead of in the major heap.  In this case, it
   is a bit slower than simple assignment.
   In particular, you can use [caml_modify] when you don't know whether the
   block being changed is in the minor heap or the major heap. */
/* PR#6084 workaround: define it as a weak symbol */

CAMLexport CAMLweakdef void caml_modify (value *fp, value val)
{
  /* The write barrier implemented by [caml_modify] checks for the
     following two conditions and takes appropriate action:
     1- a pointer from the major heap to the minor heap is created
        --> add [fp] to the remembered set
     2- a pointer from the major heap to the major heap is overwritten,
        while the GC is in the marking phase
        --> call [caml_darken] on the overwritten pointer so that the
            major GC treats it as an additional root.
  */
  value old;

  if (Is_young((value)fp)) {
    /* The modified object resides in the minor heap.
       Conditions 1 and 2 cannot occur. */
    *fp = val;
  } else {
    /* The modified object resides in the major heap. */
    CAMLassert(Is_in_heap(fp));
    old = *fp;
    *fp = val;
    if (Is_block(old)) {
      /* If [old] is a pointer within the minor heap, we already
         have a major->minor pointer and [fp] is already in the
         remembered set.  Conditions 1 and 2 cannot occur. */
      if (Is_young(old)) return;
      /* Here, [old] can be a pointer within the major heap.
         Check for condition 2. */
      if (caml_gc_phase == Phase_mark) caml_darken(old, NULL);
    }
    /* Check for condition 1. */
    if (Is_block(val) && Is_young(val)) {
      /* Add [fp] to remembered set */
      if (caml_ref_table.ptr >= caml_ref_table.limit){
        CAMLassert (caml_ref_table.ptr == caml_ref_table.limit);
        caml_realloc_ref_table (&caml_ref_table);
      }
      *caml_ref_table.ptr++ = fp;
    }
  }
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
