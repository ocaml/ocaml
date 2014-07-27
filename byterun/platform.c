#include <sys/mman.h>
#include <unistd.h>
#include "platform.h"
#include "fail.h"

#define Is_power_2(align) \
  ((align) != 0 && ((align) & ((align) - 1)) == 0)

static asize_t round_up(asize_t size, asize_t align) {
  Assert(Is_power_2(align));
  return (size + align - 1) & ~(align - 1);
}


asize_t caml_mem_round_up_pages(asize_t size)
{
  return round_up(size, sysconf(_SC_PAGESIZE));
}

void* caml_mem_map(uintnat size, uintnat alignment, int reserve_only)
{
  uintnat alloc_sz = caml_mem_round_up_pages(size + alignment);
  void* mem;
  uintnat base, aligned_start, aligned_end;

  Assert(Is_power_2(alignment));
  alignment = caml_mem_round_up_pages(alignment);

  Assert (alloc_sz > size);
  mem = mmap(0, alloc_sz, reserve_only ? PROT_NONE : (PROT_READ | PROT_WRITE),
             MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mem == MAP_FAILED) {
    return 0;
  }
  
  /* trim to an aligned region */
  base = (uintnat)mem;
  aligned_start = round_up(base, alignment);
  aligned_end = aligned_start + caml_mem_round_up_pages(size);
  caml_mem_unmap((void*)base, aligned_start - base);
  caml_mem_unmap((void*)aligned_end, (base + alloc_sz) - aligned_end);
  return (void*)aligned_start;
}

static void map_fixed(void* mem, uintnat size, int prot)
{
  if (mmap((void*)mem, size, prot,
           MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED,
           -1, 0) == MAP_FAILED) {
    caml_raise_out_of_memory();
  }
}

void caml_mem_commit(void* mem, uintnat size)
{
  map_fixed(mem, size, PROT_READ | PROT_WRITE);
}

void caml_mem_decommit(void* mem, uintnat size)
{
  map_fixed(mem, size, PROT_NONE);
}

void caml_mem_unmap(void* mem, uintnat size)
{
  munmap(mem, size);
}
