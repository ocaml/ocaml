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
#include "caml/osdeps.h"
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

intnat caml_plat_pagesize = 0;
intnat caml_plat_mmap_alignment = 0;

uintnat caml_mem_round_up_pages(uintnat size)
{
  return round_up(size, caml_plat_pagesize);
}

#define Is_page_aligned(size) ((size & (caml_plat_pagesize - 1)) == 0)

#ifdef DEBUG
static struct lf_skiplist mmap_blocks = {NULL};
#endif

#ifndef _WIN32
#endif

void* caml_mem_map(uintnat size, uintnat alignment, int reserve_only)
{
  CAMLassert(Is_power_of_2(alignment));
  CAMLassert(Is_page_aligned(size));
  alignment = round_up(alignment, caml_plat_mmap_alignment);

#ifdef DEBUG
  if (mmap_blocks.head == NULL) {
    /* The first call to caml_mem_map should be during caml_init_domains, called
       by caml_init_gc during startup - i.e. before any domains have started. */
    CAMLassert(atomic_load_acq(&caml_num_domains_running) <= 1);
    caml_lf_skiplist_init(&mmap_blocks);
  }
#endif

  void* mem = caml_plat_mem_map(size, alignment, reserve_only);

  if (mem == 0) {
    caml_gc_message(0x1000, "mmap %" ARCH_INTNAT_PRINTF_FORMAT "d bytes failed",
                            size);
    return 0;
  }

  caml_gc_message(0x1000, "mmap %" ARCH_INTNAT_PRINTF_FORMAT "d"
                          " bytes at %p for heaps\n", size, mem);

#ifdef DEBUG
  caml_lf_skiplist_insert(&mmap_blocks, (uintnat)mem, size);
#endif

  return mem;
}

void* caml_mem_commit(void* mem, uintnat size)
{
  CAMLassert(Is_page_aligned(size));
  caml_gc_message(0x1000, "commit %" ARCH_INTNAT_PRINTF_FORMAT "d"
                          " bytes at %p for heaps\n", size, mem);
  return caml_plat_mem_commit(mem, size);
}

void caml_mem_decommit(void* mem, uintnat size)
{
  if (size) {
    caml_gc_message(0x1000, "decommit %" ARCH_INTNAT_PRINTF_FORMAT "d"
                            " bytes at %p for heaps\n", size, mem);
    caml_plat_mem_decommit(mem, size);
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
  caml_plat_mem_unmap(mem, size);
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
