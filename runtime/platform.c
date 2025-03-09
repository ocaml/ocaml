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

#include "caml/config.h"
#include <string.h>
#ifndef _WIN32
#include <unistd.h>
#endif
#include <errno.h>
#include "caml/osdeps.h"
#include "caml/platform.h"
#include "caml/fail.h"
#include "caml/lf_skiplist.h"
#include "caml/misc.h"
#include "caml/signals.h"
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

CAMLexport void caml_plat_mutex_init(caml_plat_mutex * m)
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

#ifdef DEBUG
CAMLexport CAMLthread_local int caml_lockdepth = 0;
#endif

void caml_plat_assert_all_locks_unlocked(void)
{
#ifdef DEBUG
  if (caml_lockdepth) caml_fatal_error("Locks still locked at termination");
#endif
}

CAMLexport void caml_plat_lock_non_blocking_actual(caml_plat_mutex* m)
{
  /* Avoid exceptions */
  caml_enter_blocking_section_no_pending();
  int rc = pthread_mutex_lock(m);
  caml_leave_blocking_section();
  check_err("lock_non_blocking", rc);
  DEBUG_LOCK(m);
}

void caml_plat_mutex_free(caml_plat_mutex* m)
{
  check_err("mutex_free", pthread_mutex_destroy(m));
}

CAMLexport void caml_plat_mutex_reinit(caml_plat_mutex *m)
{
#ifdef DEBUG
  /* The following logic is needed to let caml_plat_assert_all_locks_unlocked()
     behave correctly in child processes after a fork operation. */
  if (caml_plat_try_lock(m)) {
    /* lock was not held at fork time */
    caml_plat_unlock(m);
  } else {
    /* lock was held at fork time, parent process still holds it, but we
       don't and need to fix lock count */
    DEBUG_UNLOCK(m);
  }
#endif
  caml_plat_mutex_init(m);
}

/* Condition variables */
static void caml_plat_cond_init_aux(caml_plat_cond *cond)
{
  pthread_condattr_t attr;
  pthread_condattr_init(&attr);
#if defined(_POSIX_TIMERS) && \
    defined(_POSIX_MONOTONIC_CLOCK) && \
    _POSIX_MONOTONIC_CLOCK != (-1)
  pthread_condattr_setclock(&attr, CLOCK_MONOTONIC);
#endif
  pthread_cond_init(cond, &attr);
}

void caml_plat_cond_init(caml_plat_cond* cond)
{
  caml_plat_cond_init_aux(cond);
}

void caml_plat_wait(caml_plat_cond* cond, caml_plat_mutex* mut)
{
  caml_plat_assert_locked(mut);
  check_err("wait", pthread_cond_wait(cond, mut));
}

void caml_plat_broadcast(caml_plat_cond* cond)
{
  check_err("cond_broadcast", pthread_cond_broadcast(cond));
}

void caml_plat_signal(caml_plat_cond* cond)
{
  check_err("cond_signal", pthread_cond_signal(cond));
}

void caml_plat_cond_free(caml_plat_cond* cond)
{
  check_err("cond_free", pthread_cond_destroy(cond));
}

/* Futexes */

#ifdef CAML_PLAT_FUTEX_FALLBACK

/* Condition-variable-based futex implementation, for when a native OS
   version isn't available. This also illustrates the semantics of the
   [wait()] and [wake_all()] operations. */

void caml_plat_futex_wait(caml_plat_futex* futex,
                          caml_plat_futex_value undesired) {
  caml_plat_lock_blocking(&futex->mutex);
  while (atomic_load_acquire(&futex->value) == undesired) {
    caml_plat_wait(&futex->cond, &futex->mutex);
  }
  caml_plat_unlock(&futex->mutex);
}

void caml_plat_futex_wake_all(caml_plat_futex* futex) {
  caml_plat_lock_blocking(&futex->mutex);
  caml_plat_broadcast(&futex->cond);
  caml_plat_unlock(&futex->mutex);
}

void caml_plat_futex_init(caml_plat_futex* ftx, caml_plat_futex_value value) {
  ftx->value = value;
  caml_plat_mutex_init(&ftx->mutex);
  caml_plat_cond_init(&ftx->cond);
}

void caml_plat_futex_free(caml_plat_futex* ftx) {
  caml_plat_mutex_free(&ftx->mutex);
  check_err("cond_destroy", pthread_cond_destroy(&ftx->cond));
}

#else /* ! CAML_PLAT_FUTEX_FALLBACK */

/* Platform-specific futex implementation.

   For each platform we define [WAIT(futex_word* ftx, futex_value
   undesired)] and [WAKE(futex_word* ftx)] in terms of
   platform-specific syscalls. The exact semantics vary, but these are
   the weakest expected guarantees:

   - [WAIT()] compares the value at [ftx] to [undesired], and if they
     are equal, goes to sleep on [ftx].

   - [WAKE()] wakes up all [WAIT()]-ers on [ftx].

   - [WAIT()] must be atomic with respect to [WAKE()], in that if the
     [WAIT()]-ing thread observes the undesired value and goes to
     sleep, it will not miss a wakeup from the [WAKE()]-ing thread
     between the comparison and sleep.

   - [WAIT()]'s initial read of [ftx] is to be treated as being atomic
     with [memory_order_relaxed]. That is, no memory ordering is
     guaranteed around it.

   - Spurious wakeups of [WAIT()] may be possible.
*/

#  if defined(_WIN32)
#    include <synchapi.h>
#    define CAML_PLAT_FUTEX_WAIT(ftx, undesired)  \
  WaitOnAddress((volatile void *)ftx, &undesired, \
                sizeof(undesired), INFINITE)
#    define CAML_PLAT_FUTEX_WAKE(ftx)           \
  WakeByAddressAll((void *)ftx)

#  elif defined(__linux__)
#    include <linux/futex.h>
#    include <sys/syscall.h>
#    define CAML_PLAT_FUTEX_WAIT(ftx, undesired)    \
  syscall(SYS_futex, ftx, FUTEX_WAIT_PRIVATE,       \
          /* expected */ undesired,                 \
          /* timeout */ NULL,                       \
          /* ignored */ NULL, 0)
#    define CAML_PLAT_FUTEX_WAKE(ftx)           \
  syscall(SYS_futex, ftx, FUTEX_WAKE_PRIVATE,   \
          /* count */ INT_MAX,                  \
          /* timeout */ NULL,                   \
          /* ignored */ NULL, 0)

#  elif 0 /* defined(__APPLE__)
   macOS has [__ulock_(wait|wake)()] which is used in implementations
   of libc++, (e.g. by LLVM) but the API is private and unstable.
   Therefore, we currently use the condition variable fallback on
   macOS. */

#  elif defined(__FreeBSD__)
#    include <sys/umtx.h>
#    define CAML_PLAT_FUTEX_WAIT(ftx, undesired) \
  _umtx_op(ftx, UMTX_OP_WAIT_UINT_PRIVATE,       \
           /* expected */ undesired,             \
           /* timeout */ NULL, NULL)
#    define CAML_PLAT_FUTEX_WAKE(ftx) \
  _umtx_op(ftx, UMTX_OP_WAKE_PRIVATE, \
           /* count */ INT_MAX,       \
           /* unused */ NULL, NULL)

#  elif defined(__OpenBSD__)
#    include <sys/futex.h>
#    define CAML_PLAT_FUTEX_WAIT(ftx, undesired)      \
  futex((volatile uint32_t*)ftx, FUTEX_WAIT_PRIVATE,  \
        /* expected */ undesired,                     \
        /* timeout */ NULL,                           \
        /* ignored */ NULL)
#    define CAML_PLAT_FUTEX_WAKE(ftx)                \
  futex((volatile uint32_t*)ftx, FUTEX_WAKE_PRIVATE, \
        /* count */ INT_MAX,                         \
        /* ignored */ NULL, NULL)

#  elif 0 /* defined(__NetBSD__)
   TODO The following code for NetBSD is untested,
   we currently use the fallback instead. */
#    include <sys/futex.h>
#    include <sys/syscall.h>
#    define CAML_PLAT_FUTEX_WAIT(ftx, undesired)    \
  syscall(SYS___futex, ftx,                         \
          FUTEX_WAIT | FUTEX_PRIVATE_FLAG,          \
          /* expected */ undesired,                 \
          /* timeout */ NULL,                       \
          /* ignored */ NULL, 0, 0)
#    define CAML_PLAT_FUTEX_WAKE(ftx)            \
  sycall(SYS___futex, ftx,                       \
         FUTEX_WAKE | FUTEX_PRIVATE_FLAG,        \
         /* count */ INT_MAX,                    \
         /* ignored */ NULL, NULL, 0, 0)

#  elif 0 /* defined(__DragonFly__)
   TODO The following code for DragonFly is untested,
   we currently use the fallback instead. */
#    define CAML_PLAT_FUTEX_WAIT(ftx, undesired)        \
  umtx_sleep((volatile const int*)ftx, undesired, 0)
#    define CAML_PLAT_FUTEX_WAKE(ftx)               \
  umtx_wakeup((volatile const int*)ftx, INT_MAX)

#  else
#    error "No futex implementation available"
#  endif

void caml_plat_futex_wait(caml_plat_futex* ftx,
                          caml_plat_futex_value undesired) {
  while (atomic_load_acquire(&ftx->value) == undesired) {
    CAML_PLAT_FUTEX_WAIT(&ftx->value, undesired);
  }
}

void caml_plat_futex_wake_all(caml_plat_futex* ftx) {
  CAML_PLAT_FUTEX_WAKE(&ftx->value);
}

void caml_plat_futex_init(caml_plat_futex* ftx,
                          caml_plat_futex_value value) {
  ftx->value = value;
}

void caml_plat_futex_free(caml_plat_futex* ftx) {
  (void) ftx; /* noop */
}

#endif /* CAML_PLAT_FUTEX_FALLBACK */

/* Latches */

void caml_plat_latch_release(caml_plat_binary_latch* latch) {
  /* if nobody is blocking, release in user-space */
  if (atomic_exchange(&latch->value, Latch_released)
      != Latch_unreleased) {
    /* at least one thread is (going to be) blocked on the futex, notify */
    caml_plat_futex_wake_all(latch);
  }
}

Caml_inline void latchlike_wait(caml_plat_futex *ftx,
                                caml_plat_futex_value unreleased,
                                caml_plat_futex_value contested) {
  /* indicate that we are about to block */
  caml_plat_futex_value expected = unreleased;
  (void)atomic_compare_exchange_strong
    (&ftx->value, &expected, contested);
  /* ftx is either already released (neither [unreleased] nor
     [contested]), or we are going to block (== [contested]),
     [futex_wait()] here will take care of both */
  caml_plat_futex_wait(ftx, contested);
}

void caml_plat_latch_wait(caml_plat_binary_latch* latch) {
  latchlike_wait(latch, Latch_unreleased, Latch_contested);
}

/* Sense-reversing barrier */
/* futex states:
   - X...0 if nobody is blocking (but they may be spinning)
   - X...1 if anybody is blocking (or about to)

   where X is the sense bit
 */

void caml_plat_barrier_flip(caml_plat_barrier* barrier,
                            barrier_status current_sense) {
  uintnat new_sense = current_sense ^ BARRIER_SENSE_BIT;
  atomic_store_relaxed(&barrier->arrived, new_sense);
  /* if a thread observes the flip below, it will also observe the
     reset counter, since any currently waiting threads will check the
     futex before leaving, they will see the counter correctly */

  caml_plat_futex_value
    current_sense_word = (caml_plat_futex_value) current_sense,
    new_sense_word = (caml_plat_futex_value) new_sense;

  /* if nobody is blocking, flip in user-space */
  if (atomic_exchange(&barrier->futex.value, new_sense_word)
      != current_sense_word) {
    /* a thread is (about to be) blocked, notify */
    caml_plat_futex_wake_all(&barrier->futex);
  }
}

void caml_plat_barrier_wait_sense(caml_plat_barrier* barrier,
                                  barrier_status sense_bit) {
  latchlike_wait(&barrier->futex, sense_bit, sense_bit | 1);
}

/* Memory management */

intnat caml_plat_pagesize = 0;
intnat caml_plat_mmap_alignment = 0;

uintnat caml_mem_round_up_pages(uintnat size)
{
  return caml_round_up(size, caml_plat_pagesize);
}

#define Is_page_aligned(size) ((size & (caml_plat_pagesize - 1)) == 0)

#ifdef DEBUG
static struct lf_skiplist mmap_blocks;
#endif

#ifndef _WIN32
#endif

void* caml_mem_map(uintnat size, int reserve_only)
{
#ifdef DEBUG
  if (mmap_blocks.head == NULL) {
    /* The first call to caml_mem_map should be during caml_init_domains, called
       by caml_init_gc during startup - i.e. before any domains have started. */
    CAMLassert(atomic_load_acquire(&caml_num_domains_running) <= 1);
    caml_lf_skiplist_init(&mmap_blocks);
  }
#endif

  void* mem = caml_plat_mem_map(size, reserve_only);

  if (mem == 0) {
    CAML_GC_MESSAGE(ADDRSPACE,
                    "mmap %" ARCH_INTNAT_PRINTF_FORMAT "d bytes failed",
                    size);
    return 0;
  }

  CAML_GC_MESSAGE(ADDRSPACE,
                  "mmap %" ARCH_INTNAT_PRINTF_FORMAT "d"
                  " bytes at %p for heaps\n", size, mem);

#ifdef DEBUG
  caml_lf_skiplist_insert(&mmap_blocks, (uintnat)mem, size);
#endif

  return mem;
}

void* caml_mem_commit(void* mem, uintnat size)
{
  CAMLassert(Is_page_aligned(size));
  CAML_GC_MESSAGE(ADDRSPACE,
                  "commit %" ARCH_INTNAT_PRINTF_FORMAT "d"
                  " bytes at %p for heaps\n", size, mem);
  return caml_plat_mem_commit(mem, size);
}

void caml_mem_decommit(void* mem, uintnat size)
{
  if (size) {
    CAML_GC_MESSAGE(ADDRSPACE,
                    "decommit %" ARCH_INTNAT_PRINTF_FORMAT "d"
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
  CAML_GC_MESSAGE(ADDRSPACE,
                  "munmap %" ARCH_INTNAT_PRINTF_FORMAT "d"
                  " bytes at %p for heaps\n", size, mem);
  caml_plat_mem_unmap(mem, size);
#ifdef DEBUG
  caml_lf_skiplist_remove(&mmap_blocks, (uintnat)mem);
#endif
}

#define Min_sleep_nsec  (10 * NSEC_PER_USEC) /* 10 usec */
#define Slow_sleep_nsec  (1 * NSEC_PER_MSEC) /*  1 msec */
#define Max_sleep_nsec   (1 * NSEC_PER_SEC)  /*  1 sec  */

unsigned caml_plat_spin_back_off(unsigned sleep_nsec,
                                 const struct caml_plat_srcloc* loc)
{
  if (sleep_nsec < Min_sleep_nsec) sleep_nsec = Min_sleep_nsec;
  if (sleep_nsec > Max_sleep_nsec) sleep_nsec = Max_sleep_nsec;
  unsigned next_sleep_nsec = sleep_nsec + sleep_nsec / 4;
  if (sleep_nsec < Slow_sleep_nsec && Slow_sleep_nsec <= next_sleep_nsec) {
    caml_gc_log("Slow spin-wait loop in %s at %s:%d",
                loc->function, loc->file, loc->line);
  }
#ifdef _WIN32
  Sleep(sleep_nsec / NSEC_PER_MSEC);
#elif defined (HAS_NANOSLEEP)
  const struct timespec req = caml_timespec_of_nsec(sleep_nsec);
  nanosleep(&req, NULL);
#else
  usleep(sleep_nsec / NSEC_PER_USEC);
#endif
  return next_sleep_nsec;
}
