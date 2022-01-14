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
#ifdef HAS_SYS_MMAN_H
#include <sys/mman.h>
#endif
#ifdef _WIN32
#include <windows.h>
#endif

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

void* caml_mem_map(uintnat size, uintnat alignment, int reserve_only)
{
  uintnat alloc_sz = caml_mem_round_up_pages(size + alignment);
  void* mem;
  uintnat base, aligned_start, aligned_end;

  CAMLassert(Is_power_of_2(alignment));
  alignment = caml_mem_round_up_pages(alignment);

  CAMLassert (alloc_sz > size);
#ifdef _WIN32
  /* Memory is only reserved at this point. It'll be committed after the
     trim. */
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
  if (!VirtualFree(mem, 0, MEM_RELEASE))
    printf("The world seems to be upside down\n");
  mem = VirtualAlloc((void*)aligned_start,
                     aligned_end - aligned_start + 1,
                     MEM_RESERVE | (reserve_only ? 0 : MEM_COMMIT),
                     reserve_only ? PAGE_NOACCESS : PAGE_READWRITE);
  if (!mem)
    printf("Trimming failed\n");
  else if (mem != (void*)aligned_start)
    printf("Hang on a sec - it's allocated a different block?!\n");
#else
  caml_mem_unmap((void*)base, aligned_start - base);
  caml_mem_unmap((void*)aligned_end, (base + alloc_sz) - aligned_end);
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
    FIXME: On Linux, with overcommit, you stand a better
    chance of getting good error messages in OOM conditions
    by forcing the kernel to allocate actual memory by touching
    all the pages. Not sure whether this is a good idea, though.

      if (p) memset(p, 0, size);
  */
  return p;
#endif
}

void caml_mem_decommit(void* mem, uintnat size)
{
#ifdef _WIN32
  if (!VirtualFree(mem, size, MEM_DECOMMIT))
    printf("VirtualFree failed to decommit\n");
#else
  map_fixed(mem, size, PROT_NONE);
#endif
}

void caml_mem_unmap(void* mem, uintnat size)
{
#ifdef _WIN32
  if (!VirtualFree(mem, size, MEM_RELEASE))
    printf("VirtualFree failed\n");
#else
  munmap(mem, size);
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
