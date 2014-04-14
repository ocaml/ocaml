#include <sys/mman.h> /* FIXME: only works on POSIX */
#include <unistd.h>


#include "minor_heap.h"
#include "fail.h"
#include "plat_threads.h"


CAMLexport __thread char *caml_young_start = NULL, *caml_young_end = NULL;


static asize_t round_up(asize_t size, asize_t align) {
  /* align must be a power of 2 */
  Assert(align != 0 && (align & (align - 1)) == 0);
  return (size + align - 1) & ~(align - 1);
}


asize_t caml_norm_minor_heap_size (intnat wsize)
{
  asize_t page_size = sysconf(_SC_PAGESIZE);
  asize_t bs, max;
  if (wsize < Minor_heap_min) wsize = Minor_heap_min;
  bs = round_up(Bsize_wsize (wsize), page_size);

  Assert(page_size * 2 < (1 << Minor_heap_align_bits));
  max = (1 << Minor_heap_align_bits) - page_size * 2;

  if (bs > max) bs = max;

  return bs;
}


static void* heaps_base;
static int heaps_allocated;
static int next_heap[1 << Minor_heap_sel_bits];
static plat_mutex heaps_lock;

void caml_init_minor_heaps()
{
  int i;
  uintnat alloc_sz, base, aligned;

  plat_mutex_init(&heaps_lock);
  for (i = 0; i < (1 << Minor_heap_sel_bits); i++)
    next_heap[i] = i;
  heaps_allocated = 0;


  alloc_sz = (uintnat)1 << (Minor_heap_sel_bits + Minor_heap_align_bits);
  Assert(alloc_sz*2 > 0);
  heaps_base = mmap(0, alloc_sz * 2,
                    PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (heaps_base == MAP_FAILED) {
    caml_fatal_error("Failed to reserve memory for minor heaps");
  }
  /* trim to an aligned region */
  base = (uintnat)heaps_base;
  aligned = round_up(base, alloc_sz);
  munmap((void*)base, aligned - base);
  munmap((void*)(aligned + alloc_sz), (base + alloc_sz * 2) - (aligned + alloc_sz));

  heaps_base = (char*)aligned;
}


void caml_allocate_minor_heap (asize_t heap_size)
{
  uintnat mem = 0, page_size = sysconf(_SC_PAGESIZE);
  Assert (heap_size == caml_norm_minor_heap_size(Wsize_bsize(heap_size)));
  Assert (caml_young_start == NULL && caml_young_end == NULL);

  plat_mutex_lock(&heaps_lock);
  if (heaps_allocated < (1 << Minor_heap_sel_bits)) {
    int heap = next_heap[heaps_allocated];
    heaps_allocated++;
    mem = (uintnat)heaps_base + (1 << Minor_heap_align_bits) * heap;
  }
  plat_mutex_unlock(&heaps_lock);

  if (!mem) caml_raise_out_of_memory();

  /* leave a guard page at the start */
  mem += page_size;
  if (mmap((void*)mem, heap_size, PROT_READ | PROT_WRITE,
           MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED,
           -1, 0) == MAP_FAILED) {
    caml_raise_out_of_memory();
  }

#ifdef DEBUG
  {
    uintnat* p = (uintnat*)mem;
    for (; p < (uintnat*)(mem + heap_size); p++) *p = Debug_uninit_align;
  }
#endif

  caml_young_start = (char*)mem;
  caml_young_end = (char*)(mem + heap_size);
}

void caml_free_minor_heap ()
{
  uintnat heap = (uintnat)caml_young_start;

  Assert(caml_young_start != NULL && caml_young_end != NULL);

  /* this should be a pointer just past the guard page */
  heap -= sysconf(_SC_PAGESIZE);
  Assert ((heap & ((1 << Minor_heap_align_bits) - 1)) == 0);

  /* instead of unmapping the heap, we map PROT_NONE space over it, so
     there's no race whereby other code could attempt to reuse the memory. */
  if (mmap((void*)heap, (1 << Minor_heap_align_bits), PROT_NONE,
           MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0) == MAP_FAILED) {
    /* if that fails, your OS is broken */
    caml_fatal_error("wtf");
  }

  plat_mutex_lock(&heaps_lock);
  Assert (heaps_allocated > 0);
  heaps_allocated--;
  next_heap[heaps_allocated] = (heap - (uintnat)heaps_base) / (1 << Minor_heap_align_bits);
  plat_mutex_unlock(&heaps_lock);

  caml_young_start = caml_young_end = NULL;
}
