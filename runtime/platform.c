/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                   Stephen Dolan, University of Cambridge               */
/*                                                                        */
/*   Copyright 2016 Indian Institute of Technology, Madras                */
/*   Copyright 2016 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/
#define CAML_INTERNALS

#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/time.h>
#include "caml/platform.h"
#include "caml/fail.h"
#include "caml/lf_skiplist.h"
#ifdef HAS_SYS_MMAN_H
#include <sys/mman.h>
#endif
#ifdef _WIN32
#include <windows.h>
#endif
#ifdef DEBUG
#include "caml/domain.h"
#endif

/* Error reporting */

void caml_plat_fatal_error(const char * action, int err)
{
  char buf[1024];
  caml_fatal_error("Fatal error during %s: %s\n",
                   action, caml_strerror(err, buf, sizeof(buf)));
}

/* Mutexes */

void caml_plat_mutex_init(caml_plat_mutex * m)
{
  int rc;
  pthread_mutexattr_t attr;
  rc = pthread_mutexattr_init(&attr);
  if (rc != 0) goto error1;
  rc = pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
  if (rc != 0) goto error2;
  rc = pthread_mutex_init(m, &attr);
  // fall through
error2:
  pthread_mutexattr_destroy(&attr);
error1:
  check_err("mutex_init", rc);
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

void caml_plat_assert_all_locks_unlocked(void)
{
#ifdef DEBUG
  if (lockdepth) caml_fatal_error("Locks still locked at termination");
#endif
}

void caml_plat_mutex_free(caml_plat_mutex* m)
{
  check_err("mutex_free", pthread_mutex_destroy(m));
}

static void caml_plat_cond_init_aux(caml_plat_cond *cond)
{
  pthread_condattr_t attr;
  pthread_condattr_init(&attr);
#if defined(_POSIX_TIMERS) && \
    defined(_POSIX_MONOTONIC_CLOCK) && \
    _POSIX_MONOTONIC_CLOCK != (-1)
  pthread_condattr_setclock(&attr, CLOCK_MONOTONIC);
#endif
  pthread_cond_init(&cond->cond, &attr);
}

/* Condition variables */
void caml_plat_cond_init(caml_plat_cond* cond, caml_plat_mutex* m)
{
  caml_plat_cond_init_aux(cond);
  cond->mutex = m;
}

void caml_plat_wait(caml_plat_cond* cond)
{
  caml_plat_assert_locked(cond->mutex);
  check_err("wait", pthread_cond_wait(&cond->cond, cond->mutex));
}

void caml_plat_broadcast(caml_plat_cond* cond)
{
  caml_plat_assert_locked(cond->mutex);
  check_err("cond_broadcast", pthread_cond_broadcast(&cond->cond));
}

void caml_plat_signal(caml_plat_cond* cond)
{
  caml_plat_assert_locked(cond->mutex);
  check_err("cond_signal", pthread_cond_signal(&cond->cond));
}

void caml_plat_cond_free(caml_plat_cond* cond)
{
  check_err("cond_free", pthread_cond_destroy(&cond->cond));
  cond->mutex=0;
}


/* Memory management */

static uintnat round_up(uintnat size, uintnat align) {
  CAMLassert(Is_power_of_2(align));
  return (size + align - 1) & ~(align - 1);
}

long caml_sys_pagesize = 0;

uintnat caml_mem_round_up_pages(uintnat size)
{
  return round_up(size, caml_sys_pagesize);
}

#ifdef _WIN32
#define MAP_FAILED 0
#endif

#ifdef DEBUG
static struct lf_skiplist mmap_blocks = {NULL};
#endif

#ifndef _WIN32
Caml_inline void safe_munmap(uintnat addr, uintnat size)
{
  if (size > 0)
    munmap((void*)addr, size);
}
#endif

void* caml_mem_map(uintnat size, uintnat alignment, int reserve_only)
{
  uintnat alloc_sz = caml_mem_round_up_pages(size + alignment);
  void* mem;
  uintnat base, aligned_start, aligned_end;

#ifdef DEBUG
  if (mmap_blocks.head == NULL) {
    /* The first call to caml_mem_map should be during caml_init_domains, called
       by caml_init_gc during startup - i.e. before any domains have started. */
    CAMLassert(atomic_load_acq(&caml_num_domains_running) <= 1);
    caml_lf_skiplist_init(&mmap_blocks);
  }
#endif

  CAMLassert(Is_power_of_2(alignment));
  alignment = caml_mem_round_up_pages(alignment);

  CAMLassert (alloc_sz > size);
#ifdef _WIN32
  /* Memory is only reserved at this point. It'll be committed after the
     trim. */
again:
  mem = VirtualAlloc(NULL, alloc_sz, MEM_RESERVE, PAGE_NOACCESS);
#else
  mem = mmap(0, alloc_sz, reserve_only ? PROT_NONE : (PROT_READ | PROT_WRITE),
             MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
#endif
  if (mem == MAP_FAILED) {
    return 0;
  }

  /* trim to an aligned region */
  base = (uintnat)mem;
  aligned_start = round_up(base, alignment);
  aligned_end = aligned_start + caml_mem_round_up_pages(size);
#ifdef _WIN32
  /* VirtualFree can be used to decommit portions of memory, but it can only
     release the entire block of memory. For Windows, repeat the call but this
     time specify the address. */
  VirtualFree(mem, 0, MEM_RELEASE);
  mem = VirtualAlloc((void*)aligned_start,
                     aligned_end - aligned_start,
                     MEM_RESERVE | (reserve_only ? 0 : MEM_COMMIT),
                     reserve_only ? PAGE_NOACCESS : PAGE_READWRITE);
  if (mem == NULL) {
    /* VirtualAlloc can return the following three interesting errors:
         - ERROR_INVALID_ADDRESS - pages are already reserved (race)
         - ERROR_NOT_ENOUGH_MEMORY - address space exhausted
         - ERROR_COMMITMENT_LIMIT - memory exhausted */
    if (GetLastError() == ERROR_INVALID_ADDRESS) {
      SetLastError(0);
      /* Raced - try again. */
      goto again;
    } else {
      return 0;
    }
  }
  CAMLassert(mem == (void*)aligned_start);
#else
  safe_munmap(base, aligned_start - base);
  safe_munmap(aligned_end, (base + alloc_sz) - aligned_end);
#endif
#ifdef DEBUG
  caml_lf_skiplist_insert(&mmap_blocks,
                          aligned_start, aligned_end - aligned_start);
#endif
  return (void*)aligned_start;
}

#ifndef _WIN32
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
#endif

void* caml_mem_commit(void* mem, uintnat size)
{
#ifdef _WIN32
  return VirtualAlloc(mem, size, MEM_COMMIT, PAGE_READWRITE);
#else
  void* p = map_fixed(mem, size, PROT_READ | PROT_WRITE);
  /*
    FIXME: On Linux, it might be useful to populate page tables with
    MAP_POPULATE to reduce the time spent blocking on page faults at
    a later point.
  */
  return p;
#endif
}

void caml_mem_decommit(void* mem, uintnat size)
{
  if (size) {
#ifdef _WIN32
    VirtualFree(mem, size, MEM_DECOMMIT);
#else
    map_fixed(mem, size, PROT_NONE);
#endif
  }
}

void caml_mem_unmap(void* mem, uintnat size)
{
#ifdef DEBUG
  uintnat data;
  CAMLassert(caml_lf_skiplist_find(&mmap_blocks, (uintnat)mem, &data) != 0);
  CAMLassert(data == size);
#endif
#ifdef _WIN32
  if (!VirtualFree(mem, 0, MEM_RELEASE))
    CAMLassert(0);
#else
  if (munmap(mem, size) != 0)
    CAMLassert(0);
#endif
#ifdef DEBUG
  caml_lf_skiplist_remove(&mmap_blocks, (uintnat)mem);
#endif
}

#define Min_sleep_ns       10000 // 10 us
#define Slow_sleep_ns    1000000 //  1 ms
#define Max_sleep_ns  1000000000 //  1 s

unsigned caml_plat_spin_wait(unsigned spins,
                             const char* file, int line,
                             const char* function)
{
  unsigned next_spins;
  if (spins < Min_sleep_ns) spins = Min_sleep_ns;
  if (spins > Max_sleep_ns) spins = Max_sleep_ns;
  next_spins = spins + spins / 4;
  if (spins < Slow_sleep_ns && Slow_sleep_ns <= next_spins) {
    caml_gc_log("Slow spin-wait loop in %s at %s:%d", function, file, line);
  }
  usleep(spins/1000);
  return next_spins;
}
