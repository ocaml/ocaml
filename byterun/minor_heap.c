#include "minor_heap.h"
#include "fail.h"
#include "platform.h"
#include "domain.h"

CAMLexport __thread char *caml_young_start = NULL, *caml_young_end = NULL;

asize_t caml_norm_minor_heap_size (intnat wsize)
{
  asize_t page_size = caml_mem_round_up_pages(1);
  asize_t bs, max;
  if (wsize < Minor_heap_min) wsize = Minor_heap_min;
  bs = caml_mem_round_up_pages(Bsize_wsize (wsize));

  Assert(page_size * 2 < (1 << Minor_heap_align_bits));
  max = (1 << Minor_heap_align_bits) - page_size * 2;

  if (bs > max) bs = max;

  return bs;
}


static void* heaps_base;
static int heaps_allocated;
static int next_heap[1 << Minor_heap_sel_bits];
static struct domain* heap_owners[1 << Minor_heap_sel_bits];
static caml_plat_mutex heaps_lock;

void caml_init_minor_heaps()
{
  int i;
  uintnat size = (uintnat)1 << (Minor_heap_sel_bits + Minor_heap_align_bits);

  /* sanity check configuration */
  if (caml_mem_round_up_pages(1 << Minor_heap_align_bits) != (1 << Minor_heap_align_bits))
    caml_fatal_error("Minor_heap_align_bits misconfigured for this platform");
  
  caml_plat_mutex_init(&heaps_lock);
  for (i = 0; i < (1 << Minor_heap_sel_bits); i++)
    next_heap[i] = i;
  heaps_allocated = 0;

  /* To ensure Is_foreign gives no false positives, we reserve twice
     the address space needed and only use the first half */
  heaps_base = caml_mem_map(size*2, size*2, 1 /* reserve_only */);
}

void caml_init_young_ptrs ()
{
  Assert (caml_young_start == 0);
  Assert (heaps_base);
  caml_young_start = caml_young_end = (char*)heaps_base;
}

void caml_allocate_minor_heap (asize_t heap_size)
{
  uintnat mem = 0;
  Assert (heap_size == caml_norm_minor_heap_size(Wsize_bsize(heap_size)));
  Assert (caml_young_start == NULL && caml_young_end == NULL);

  caml_plat_lock(&heaps_lock);
  if (heaps_allocated < (1 << Minor_heap_sel_bits)) {
    int heap = next_heap[heaps_allocated];
    heap_owners[heap] = caml_domain_self();
    heaps_allocated++;
    mem = (uintnat)heaps_base + (1 << Minor_heap_align_bits) * heap;
  }
  caml_plat_unlock(&heaps_lock);

  if (!mem) caml_raise_out_of_memory();

  /* leave a guard page at the start */
  mem = caml_mem_round_up_pages(mem + 1);
  caml_mem_commit((void*)mem, heap_size);
  
#ifdef DEBUG
  {
    uintnat* p = (uintnat*)mem;
    for (; p < (uintnat*)(mem + heap_size); p++) *p = Debug_uninit_align;
  }
#endif

  caml_young_start = (char*)mem;
  caml_young_end = (char*)(mem + heap_size);
}

static int heap_id(value v)
{
  return ((uintnat)v - (uintnat)heaps_base) / (1 << Minor_heap_align_bits);
}

void caml_free_minor_heap ()
{
  uintnat heap = (uintnat)caml_young_start;

  Assert(caml_young_start != NULL && caml_young_end != NULL);
  
  if (caml_young_end != (char*)heaps_base) {
    /* this should be a pointer just past the guard page */
    heap -= caml_mem_round_up_pages(1);
    Assert ((heap & ((1 << Minor_heap_align_bits) - 1)) == 0);
  
    /* instead of unmapping the heap, we decommit it, so
       there's no race whereby other code could attempt to reuse the memory. */
    caml_mem_decommit((void*)heap, (1 << Minor_heap_align_bits));
  
    caml_plat_lock(&heaps_lock);
    Assert (heaps_allocated > 0);
    heaps_allocated--;
    next_heap[heaps_allocated] = heap_id(heap);
    Assert(heap_owners[heap_id(heap)] != 0);
    heap_owners[heap_id(heap)] = 0;
    caml_plat_unlock(&heaps_lock);
  }
  caml_young_start = caml_young_end = NULL;
}

struct domain* caml_owner_of_young_block(value v) {
  Assert(Is_minor(v));
  return heap_owners[heap_id(v)];
}
