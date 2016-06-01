#include <sys/mman.h>
#include <unistd.h>
#include "caml/platform.h"
#include "caml/fail.h"

/* One-shot events */


void caml_plat_event_init(caml_plat_event* e)
{
  pthread_mutex_init(&e->mutex, 0);
  pthread_cond_init(&e->cond, 0);
  e->triggered = e->waited = 0;
}

void caml_plat_event_wait(caml_plat_event* e)
{
  pthread_mutex_lock(&e->mutex);
  Assert(!e->waited);
  e->waited = 1;
  while (!e->triggered) {
    pthread_cond_wait(&e->cond, &e->mutex);
  }
  pthread_mutex_unlock(&e->mutex);
  pthread_mutex_destroy(&e->mutex);
  pthread_cond_destroy(&e->cond);
}

void caml_plat_event_trigger(caml_plat_event* e)
{
  pthread_mutex_lock(&e->mutex);
  Assert(!e->triggered);
  e->triggered = 1;
  pthread_cond_broadcast(&e->cond);
  pthread_mutex_unlock(&e->mutex);
}


/* Memory management */

#define Is_power_2(align) \
  ((align) != 0 && ((align) & ((align) - 1)) == 0)

static uintnat round_up(uintnat size, uintnat align) {
  Assert(Is_power_2(align));
  return (size + align - 1) & ~(align - 1);
}


uintnat caml_mem_round_up_pages(uintnat size)
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

#define Min_sleep_ns       10000 // 10 us
#define Slow_sleep_ns    1000000 //  1 ms
#define Max_sleep_ns  1000000000 //  1 s

unsigned caml_plat_spin_wait(unsigned spins,
                             const char* file, int line,
                             const char* function)
{
  if (spins < Min_sleep_ns) spins = Min_sleep_ns;
  if (spins > Max_sleep_ns) spins = Max_sleep_ns;
  unsigned next_spins = spins + spins / 4;
  if (spins < Slow_sleep_ns && Slow_sleep_ns <= next_spins) {
    caml_gc_log("Slow spin-wait loop in %s at %s:%d", function, file, line);
  }
  usleep(spins/1000);
  return next_spins;
}
