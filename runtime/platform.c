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

intnat caml_sys_pagesize = 0;

uintnat caml_mem_round_up_pages(uintnat size)
{
  return round_up(size, caml_sys_pagesize);
}

#define Is_page_aligned(size) ((size & (caml_sys_pagesize - 1)) == 0)

#ifdef _WIN32
#define MAP_FAILED 0
#endif

#ifdef DEBUG
static struct lf_skiplist mmap_blocks = {NULL};
#endif

#ifndef _WIN32
Caml_inline void safe_munmap(uintnat addr, uintnat size)
{
  if (size > 0) {
    caml_gc_message(0x1000, "munmap %" ARCH_INTNAT_PRINTF_FORMAT "d"
                            " bytes at %" ARCH_INTNAT_PRINTF_FORMAT "x"
                            " for heaps\n", size, addr);
    munmap((void*)addr, size);
  }
}
#endif

void* caml_mem_map(uintnat size, uintnat alignment, int reserve_only)
{
  CAMLassert(Is_power_of_2(alignment));
  CAMLassert(Is_page_aligned(size));
  alignment = caml_mem_round_up_pages(alignment);

#ifdef MMAP_ALIGNS_TO_PAGESIZE
  uintnat alloc_sz = size;
#else
  uintnat alloc_sz = size + alignment;
  uintnat base, aligned_start, aligned_end;
#endif
  void* mem;

#ifdef DEBUG
  if (mmap_blocks.head == NULL) {
    /* The first call to caml_mem_map should be during caml_init_domains, called
       by caml_init_gc during startup - i.e. before any domains have started. */
    CAMLassert(atomic_load_acq(&caml_num_domains_running) <= 1);
    caml_lf_skiplist_init(&mmap_blocks);
  }
#endif

#ifdef _WIN32
  /* caml_sys_pagesize has been engineered to be the granularity of
     VirtualAlloc, so trimming will be unnecessary. */
  if (alignment > caml_sys_pagesize)
    caml_fatal_error("Cannot align memory to %" ARCH_INTNAT_PRINTF_FORMAT "x"
                     " on this platform", alignment);
  mem =
    VirtualAlloc(NULL, alloc_sz,
                 MEM_RESERVE | (reserve_only ? 0 : MEM_COMMIT),
                 reserve_only ? PAGE_NOACCESS : PAGE_READWRITE);
#else
#ifdef __CYGWIN__
  if (alignment > caml_sys_pagesize)
    caml_fatal_error("Cannot align memory to %lx on this platform", alignment);
#endif
  mem = mmap(0, alloc_sz, reserve_only ? PROT_NONE : (PROT_READ | PROT_WRITE),
             MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
#endif
  if (mem == MAP_FAILED) {
    caml_gc_message(0x1000, "mmap %" ARCH_INTNAT_PRINTF_FORMAT "d bytes failed",
                            alloc_sz);
    return 0;
  }
  caml_gc_message(0x1000, "mmap %" ARCH_INTNAT_PRINTF_FORMAT "d"
                          " bytes at %p for heaps\n", alloc_sz, mem);

#ifndef MMAP_ALIGNS_TO_PAGESIZE
  /* trim to an aligned region */
  base = (uintnat)mem;
  aligned_start = round_up(base, alignment);
  aligned_end = aligned_start + size;
  safe_munmap(base, aligned_start - base);
  safe_munmap(aligned_end, (base + alloc_sz) - aligned_end);
  mem = (void*)aligned_start;
#endif

#ifdef DEBUG
  caml_lf_skiplist_insert(&mmap_blocks, (uintnat)mem, size);
#endif

  return mem;
}

#ifndef _WIN32
static void* map_fixed(void* mem, uintnat size, int prot)
{
#ifdef __CYGWIN__
  if (mprotect(mem, size, prot) != 0) {
#else
  if (mmap(mem, size, prot, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED,
           -1, 0) == MAP_FAILED) {
#endif
    return 0;
  } else {
    return mem;
  }
}
#endif

void* caml_mem_commit(void* mem, uintnat size)
{
  CAMLassert(Is_page_aligned(size));
  caml_gc_message(0x1000, "commit %" ARCH_INTNAT_PRINTF_FORMAT "d"
                          " bytes at %p for heaps\n", size, mem);
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
    caml_gc_message(0x1000, "decommit %" ARCH_INTNAT_PRINTF_FORMAT "d"
                            " bytes at %p for heaps\n", size, mem);
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
  caml_gc_message(0x1000, "munmap %" ARCH_INTNAT_PRINTF_FORMAT "d"
                          " bytes at %p for heaps\n", size, mem);
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
