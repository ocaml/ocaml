#define _GNU_SOURCE /* for PTHREAD_MUTEX_ERRORCHECK_NP */
#include <sys/mman.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/time.h>
#include "caml/platform.h"
#include "caml/fail.h"

/* Mutexes */
static void check_err(char* action, int err)
{
  if (err) {
    caml_fatal_error_arg2("Fatal error during %s", action, ": %s\n", strerror(err));
  }
}

void caml_plat_mutex_init(caml_plat_mutex* m)
{
#ifdef __linux__
  pthread_mutexattr_t ma;
  pthread_mutexattr_init(&ma);
  pthread_mutexattr_settype(&ma, PTHREAD_MUTEX_ERRORCHECK_NP);
  check_err("mutex_init", pthread_mutex_init(m, &ma));
#else
  check_err("mutex_init", pthread_mutex_init(m, 0));
#endif
}

void caml_plat_lock(caml_plat_mutex* m)
{
  check_err("lock", pthread_mutex_lock(m));
}

int caml_plat_try_lock(caml_plat_mutex* m)
{
  int r = pthread_mutex_trylock(m);
  if (r == EBUSY) {
    return 0;
  } else {
    check_err("try_lock", r);
    return 1;
  }
}

void caml_plat_assert_locked(caml_plat_mutex* m)
{
#ifdef DEBUG
  int r = pthread_mutex_trylock(m);
  if (r == EBUSY) {
    /* ok, it was locked */
    return;
  } else if (r == 0) {
    caml_fatal_error("Required mutex not locked");
  } else {
    check_err("assert_locked", r);
  }
#endif
}

void caml_plat_unlock(caml_plat_mutex* m)
{
  check_err("unlock", pthread_mutex_unlock(m));
}

void caml_plat_mutex_free(caml_plat_mutex* m)
{
  check_err("mutex_free", pthread_mutex_destroy(m));
}

/* Condition variables */
void caml_plat_cond_init(caml_plat_cond* cond, caml_plat_mutex* m)
{
  pthread_condattr_t attr;
  pthread_condattr_init(&attr);
#if defined(_POSIX_TIMERS) && defined(_POSIX_MONOTONIC_CLOCK) && _POSIX_MONOTONIC_CLOCK != (-1)
  pthread_condattr_setclock(&attr, CLOCK_MONOTONIC);
#endif
  pthread_cond_init(&cond->cond, &attr);
  cond->mutex = m;
}

void caml_plat_wait(caml_plat_cond* cond)
{
  caml_plat_assert_locked(cond->mutex);
  check_err("wait", pthread_cond_wait(&cond->cond, cond->mutex));
}

int caml_plat_timedwait(caml_plat_cond* cond, int64_t until)
{
  struct timespec t;
  int err;
  if (until < 0) {
    /* until < 0 has definitely timed out, long ago.
       letting the code below run risks feeding negative tv_nsec
       to timedwait, since (-1 % 1000000000) = -1. */
    return 1;
  }
  t.tv_sec  = until / 1000000000;
  t.tv_nsec = until % 1000000000;
  err = pthread_cond_timedwait(&cond->cond, cond->mutex, &t);
  if (err == ETIMEDOUT) {
    return 1;
  } else {
    return 0;
  }
}

void caml_plat_broadcast(caml_plat_cond* cond)
{
  caml_plat_assert_locked(cond->mutex);
  check_err("signal", pthread_cond_broadcast(&cond->cond));
}

void caml_plat_cond_free(caml_plat_cond* cond)
{
  check_err("cond_free", pthread_cond_destroy(&cond->cond));
  cond->mutex=0;
}


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

static void* map_fixed(void* mem, uintnat size, int prot)
{
  if (mmap((void*)mem, size, prot,
           MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED,
           -1, 0) == MAP_FAILED) {
    return 0;
  } else {
    return mem;
  }
}

void* caml_mem_commit(void* mem, uintnat size)
{
  void* p = map_fixed(mem, size, PROT_READ | PROT_WRITE);
  /*
    FIXME: On Linux, with overcommit, you stand a better
    chance of getting good error messages in OOM conditions
    by forcing the kernel to allocate actual memory by touching
    all the pages. Not sure whether this is a good idea, though.

      if (p) memset(p, 0, size);
  */
  return p;
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
