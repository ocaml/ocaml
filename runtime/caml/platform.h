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

/* Platform-specific concurrency and memory primitives */

#ifndef CAML_PLAT_THREADS_H
#define CAML_PLAT_THREADS_H

#ifdef CAML_INTERNALS

#include <pthread.h>
#include <errno.h>
#include <string.h>
#include "config.h"
#include "mlvalues.h"
#include "sys.h"
#ifdef _MSC_VER
#include <intrin.h>
#endif

#if defined(MAP_ANON) && !defined(MAP_ANONYMOUS)
#define MAP_ANONYMOUS MAP_ANON
#endif

/* Hint for busy-waiting loops */

Caml_inline void cpu_relax(void) {
#ifdef __GNUC__
#if defined(__x86_64__) || defined(__i386__)
  __asm__ volatile("pause" ::: "memory");
#elif defined(__aarch64__)
  __asm__ volatile ("yield" ::: "memory");
#elif defined(__riscv)
  /* Encoding of the pause instruction */
  __asm__ volatile (".4byte 0x100000F");
#elif defined(__ppc64__)
  __asm__ volatile ("or 1, 1, 1 # low priority");
  __asm__ volatile ("or 2, 2, 2 # medium priority");
  __asm__ volatile ("" ::: "memory");
#else
  /* Just a compiler barrier */
  __asm__ volatile ("" ::: "memory");
#endif
#elif defined(_MSC_VER)
/* It would be better to use YieldProcessor to have a portable implementation
   but this would require windows.h which we can't include here (it would
   conflict with caml/instruct.h on ATOM, for instance)
*/
#if defined(_M_IX86) || defined(_M_X64)
  _mm_pause();
#endif
#endif
}


/* Warning: blocking functions.

   Blocking functions are for use in the runtime outside of the
   mutator, or when the domain lock is not held.

   In order to use them inside the mutator and while holding the
   domain lock, one must make sure that the wait is very short, and
   that no deadlock can arise from the interaction with the domain
   locks and the stop-the-world sections.

   In particular one must not call [caml_plat_lock_blocking] on a
   mutex while the domain lock is held:
    - if any critical section of the mutex crosses an allocation, a
      blocking section releasing the domain lock, or any other
      potential STW section, nor
    - if the same lock is acquired at any point using [Mutex.lock] or
      [caml_plat_lock_non_blocking] on the same domain (circular
      deadlock with the domain lock).

   Hence, as a general rule, prefer [caml_plat_lock_non_blocking] to
   lock a mutex when inside the mutator and holding the domain lock.
   The domain lock must be held in order to call
   [caml_plat_lock_non_blocking].

   It is possible to combine calls to [caml_plat_lock_non_blocking] on
   a mutex from the mutator holding the domain lock with calls to
   [caml_plat_lock_blocking] on another mutator that has released
   their domain lock, but not with calls to [caml_plat_lock_blocking]
   from a STW section or a custom block finaliser.

   These functions never raise exceptions; errors are fatal. Thus, for
   usages where bugs are susceptible to be introduced by users, the
   functions from caml/sync.h should be used instead.
*/

typedef pthread_mutex_t caml_plat_mutex;
#define CAML_PLAT_MUTEX_INITIALIZER PTHREAD_MUTEX_INITIALIZER
CAMLextern void caml_plat_mutex_init(caml_plat_mutex*);
Caml_inline void caml_plat_lock_blocking(caml_plat_mutex*);
Caml_inline void caml_plat_lock_non_blocking(caml_plat_mutex*);
Caml_inline int caml_plat_try_lock(caml_plat_mutex*);
void caml_plat_assert_locked(caml_plat_mutex*);
void caml_plat_assert_all_locks_unlocked(void);
Caml_inline void caml_plat_unlock(caml_plat_mutex*);
void caml_plat_mutex_free(caml_plat_mutex*);
CAMLextern void caml_plat_mutex_reinit(caml_plat_mutex*);
typedef pthread_cond_t caml_plat_cond;
#define CAML_PLAT_COND_INITIALIZER PTHREAD_COND_INITIALIZER
void caml_plat_cond_init(caml_plat_cond*);
void caml_plat_wait(caml_plat_cond*, caml_plat_mutex*); /* blocking */
void caml_plat_broadcast(caml_plat_cond*);
void caml_plat_signal(caml_plat_cond*);
void caml_plat_cond_free(caml_plat_cond*);

/* Futexes

   A futex is an integer that can be waited on and woken, used to build other
   synchronisation primitives. Either uses OS facilities directly, or a
   condition variable fallback.
*/
typedef struct caml_plat_futex /* {
  // this field is available regardless of implementation
  caml_plat_futex_word value;
  <possibly other fields>; ...
} */ caml_plat_futex;

typedef uint32_t caml_plat_futex_value;
typedef _Atomic caml_plat_futex_value caml_plat_futex_word;

/* Block while [futex] has the value [undesired], until woken by [wake_all()] */
void caml_plat_futex_wait(caml_plat_futex* futex,
                          caml_plat_futex_value undesired);
/* Wake all threads [wait()]-ing on [futex] */
void caml_plat_futex_wake_all(caml_plat_futex* futex);
/* Initialise the futex for the first time, use [CAML_PLAT_FUTEX_INITIALIZER] to
   do this statically */
void caml_plat_futex_init(caml_plat_futex* ftx, caml_plat_futex_value value);
/* Deinitialise the futex; no-op if native futexes are used */
void caml_plat_futex_free(caml_plat_futex*);

/* [CAML_PLAT_FUTEX_FALLBACK] can be defined to use the condition variable
   fallback, even if a futex implementation is available. */
#ifndef CAML_PLAT_FUTEX_FALLBACK
#  if defined(_WIN32)                                   \
  || (defined(__linux__) && defined(HAS_LINUX_FUTEX_H)) \
  || defined(__FreeBSD__) || defined(__OpenBSD__)
/* TODO We have implementations for these platforms, but they are
   currently untested, so use the fallback instead.
  || defined(__NetBSD__) || defined(__DragonFly__) */
#  else
/* Use the fallback on platforms that we do not have an OS-specific
   implementation for, such as macOS. */
#    define CAML_PLAT_FUTEX_FALLBACK
#  endif
#endif

#ifdef CAML_PLAT_FUTEX_FALLBACK
struct caml_plat_futex {
  caml_plat_futex_word value;
  caml_plat_mutex mutex;
  caml_plat_cond cond;
};
#  define CAML_PLAT_FUTEX_INITIALIZER(value) \
  { (value), CAML_PLAT_MUTEX_INITIALIZER, CAML_PLAT_COND_INITIALIZER }
#else
struct caml_plat_futex {
  caml_plat_futex_word value;
};
#  define CAML_PLAT_FUTEX_INITIALIZER(value) { (value) }
#endif /* CAML_PLAT_FUTEX_FALLBACK */

/* Latches

   A binary latch is a boolean value with a [wait()] operation. It has two
   states, "released" and "unreleased" (or "set"). [latch_set()] can be used to
   set the latch to unreleased, [latch_release()] can be used to release it, and
   [latch_wait()] can be used from the unreleased state to block until
   [latch_release()] is called.

                    [latch_set()]
         +------------------------------------+
         v                                    |
     UNRELEASED                            RELEASED
         |                                    ^
         +-< unblock [latch_wait()] callers >-+
                   [latch_release()]

   This type of object is also called a manual-reset event in Windows APIs, or
   it can be considered a special case of Java's [CountDownLatch] or C++'s
   [std::latch] with the counter capped at one.
 */
typedef caml_plat_futex caml_plat_binary_latch;

/* Released state */
#define Latch_released 0 /* must be zero, see barrier initialisation */
/* Unreleased state, no [latch_wait()] callers */
#define Latch_unreleased 1
/* Unreleased state, at least one [latch_wait()] caller */
#define Latch_contested 2

/* Initialise the latch to a released state */
#define CAML_PLAT_LATCH_INITIALIZER CAML_PLAT_FUTEX_INITIALIZER(Latch_released)
Caml_inline void caml_plat_latch_init(caml_plat_binary_latch* latch) {
  caml_plat_futex_init(latch, Latch_released);
}
/* Release the latch, waking any waiters */
void caml_plat_latch_release(caml_plat_binary_latch*);
/* Block until released. This is no-op (but more expensive than checking with
   [is_released()]) if the latch has already been released. */
void caml_plat_latch_wait(caml_plat_binary_latch*);
/* Check if a latch is released */
Caml_inline int caml_plat_latch_is_released(caml_plat_binary_latch* latch) {
  return atomic_load_acquire(&latch->value) == Latch_released;
}
/* Check if a latch is unreleased */
Caml_inline int caml_plat_latch_is_set(caml_plat_binary_latch* latch) {
  return !caml_plat_latch_is_released(latch);
}
/* Set the latch to unreleased */
Caml_inline void caml_plat_latch_set(caml_plat_binary_latch* latch) {
  atomic_store_release(&latch->value, Latch_unreleased);
}

/* Barriers

   A barrier is an object used to synchronise a variable number of
   threads/parties. Each party arrives at the barrier, and only once all parties
   have arrived can any threads leave the barrier. There are two variants: the
   "single-sense" barrier must be manually reset before it can be reused,
   whereas the "sense-reversing" barrier can be reused immediately after it has
   been released.

   | Operation | [caml_plat_barrier_*] function      |
   |           |---------------+---------------------|
   |           | Single-sense  | Sense-reversing     |
   |-----------|---------------+---------------------|
   | Reset     | [reset]       | automatic at [flip] |
   | Arrive    | [arrive]      | [arrive]            |
   | Check     | [is_released] | [sense_has_flipped] |
   | Block     | [wait]        | [wait_sense]        |
   | Release   | [release]     | [flip]              |

   The lifecycle is as follows:

        Reset (1 thread)          (other threads)
                |                       |
                +----------+------------+
                           |
                        Arrive (all threads)
                           |
                           | check arrival number
                +----------+------------+
                |                       |
         Check or Block              Release
       (non-final threads)        (final thread)
                |                       |

   Leaving the barrier after [Block] or a nonzero [Check] result synchronises
   with the [Release] of the barrier from the final thread, which in turn
   synchronises with the non-final threads at the time they [Arrive]d.

   That is, on non-final threads, anything performed before [Check]/[Block] may
   race with code in other threads that happens before they [Arrive], and
   anything performed after [Arrive] is entirely unsynchronised by the barrier,
   so may race with code in other threads that happens after they [Arrive]. In
   particular, code between [Arrive] and [Check]/[Block] may race with code
   before or after the barrier in all other threads. The final thread is the
   exception, and may execute code after [Arrive] but before [Release] that will
   still be synchronised by the barrier.
*/
typedef struct caml_plat_barrier {
  caml_plat_futex futex;
  atomic_uintnat arrived; /* includes sense bit */
} caml_plat_barrier;

/* This initialises both a single-sense and sense-reversing barrier, for
   single-sense this is the released state ([Latch_released], which must be 0)
   and for sense-reversing it is just a valid initialised state. */
#define CAML_PLAT_BARRIER_INITIALIZER \
  { CAML_PLAT_FUTEX_INITIALIZER(Latch_released), 0 }

typedef uintnat barrier_status;
#define BARRIER_SENSE_BIT 0x100000
/* Arrive at the barrier, returns the number of parties that have arrived at the
   barrier (including this one); the caller should check whether it is the last
   expected party to arrive, and release or flip the barrier if so.

   In a sense-reversing barrier, this also encodes the current sense of the
   barrier in [BARRIER_SENSE_BIT], which should be masked off if checking for
   the last arrival. */
Caml_inline barrier_status caml_plat_barrier_arrive(caml_plat_barrier* barrier)
{
  return 1 + atomic_fetch_add(&barrier->arrived, 1);
}

/* -- Single-sense --
   [futex] is used as a binary latch. */

/* Reset the barrier to 0 arrivals, block new waiters */
Caml_inline void caml_plat_barrier_reset(caml_plat_barrier* barrier) {
  caml_plat_latch_set(&barrier->futex);
  atomic_store_release(&barrier->arrived, 0);
}
/* Check if the barrier has been released */
Caml_inline int caml_plat_barrier_is_released(caml_plat_barrier* barrier) {
  return caml_plat_latch_is_released(&barrier->futex);
}
/* Release the barrier unconditionally, letting all parties through */
Caml_inline void caml_plat_barrier_release(caml_plat_barrier* barrier) {
  caml_plat_latch_release(&barrier->futex);
}
/* Block until released */
Caml_inline void caml_plat_barrier_wait(caml_plat_barrier* barrier) {
  caml_plat_latch_wait(&barrier->futex);
}

/* -- Sense-reversing -- */
/* Flip the sense of the barrier, releasing current waiters and
   blocking new ones.

   [current_sense] should be [(b & BARRIER_SENSE_BIT)] with [b] as
   returned by [barrier_arrive()]. */
void caml_plat_barrier_flip(caml_plat_barrier*, barrier_status current_sense);
Caml_inline int
caml_plat_barrier_sense_has_flipped(caml_plat_barrier* barrier,
                                    barrier_status current_sense)
{
  return (atomic_load_acquire(&barrier->futex.value) & BARRIER_SENSE_BIT)
    != current_sense;
}
/* Block until flipped */
void caml_plat_barrier_wait_sense(caml_plat_barrier*,
                                  barrier_status current_sense);

/* Spin-wait loops

   We provide the macros [SPIN_WAIT], [SPIN_WAIT_NTIMES(N)] and
   [SPIN_WAIT_BOUNDED] that expand to [for]-loop headers for spin-wait
   loops. The latter two are expected to be used alongside OS-based
   synchronisation (e.g. latches, barriers).

   Example usage:

   SPIN_WAIT {
     if (condition_has_come_true()) {
       break; // or return;
     }

     perform_useful_spin_work();
   }

   [SPIN_WAIT] spins for unbounded time, and should only be used when hashing
   out contention over a short critical section that only one thread needs to
   run, where more complex synchronisation would be too expensive and
   unnecessary.

   [SPIN_WAIT_NTIMES(N)] should be used with one of the [Max_spins_*] constants
   defined below (though the N expression doesn't need to be a constant), it
   loops the body up to N times and then ends, even if the condition hasn't come
   true. Exactly how much spinning is optimal can be tricky and may warrant
   profiling, with the caveat that it is also probably machine-dependent.
   Typically, [Max_spins_long] iterations are only useful when there are exactly
   2 domains, otherwise [Max_spins_short] is best to yield to OS synchronisation
   as fast as possible.

   [SPIN_WAIT_BOUNDED] expands to [SPIN_WAIT_NTIMES(Max_spins_medium)] and
   should be used when there is useful work to do in the body of the loop.
 */

/* The exact values here are estimates based on data from a specific machine,
   and shouldn't be focused on too much. */
#define Max_spins_long 1000
#define Max_spins_medium 300
#define Max_spins_short 30

#define SPIN_WAIT_NTIMES(N)                             \
  unsigned CAML_GENSYM(spins) = 0;                      \
  unsigned CAML_GENSYM(max_spins) = (N);                \
  for (; CAML_GENSYM(spins) < CAML_GENSYM(max_spins);   \
       cpu_relax(), ++CAML_GENSYM(spins))
#define SPIN_WAIT_BOUNDED SPIN_WAIT_NTIMES(Max_spins_medium)
#define SPIN_WAIT SPIN_WAIT_BACK_OFF(Max_spins_long)

/* [SPIN_WAIT_*] implementation details */

struct caml_plat_srcloc {
  const char* file;
  int line;
  const char* function;
};

/* Start/continue backing off, returns the next [sleep_ns] */
CAMLextern unsigned caml_plat_spin_back_off(unsigned sleep_ns,
                                            const struct caml_plat_srcloc* loc);

Caml_inline unsigned caml_plat_spin_step(unsigned spins,
                                         unsigned max_spins,
                                         const struct caml_plat_srcloc *loc) {
  cpu_relax();
  if (CAMLlikely(spins < max_spins)) {
    return spins + 1;
  } else {
    /* [spins] becomes [sleep_ns] at this point, which remains greater than
       [max_spins] */
    return caml_plat_spin_back_off(spins, loc);
  }
}

#define SPIN_WAIT_BACK_OFF(max_spins)                                   \
  unsigned CAML_GENSYM(spins) = 0;                                      \
  unsigned CAML_GENSYM(max_spins) = (max_spins);                        \
  static const struct caml_plat_srcloc CAML_GENSYM(loc) = {             \
    __FILE__, __LINE__, __func__                                        \
  };                                                                    \
  for (; 1; CAML_GENSYM(spins) = caml_plat_spin_step(                   \
         CAML_GENSYM(spins), CAML_GENSYM(max_spins), &CAML_GENSYM(loc)))

/* Memory management primitives (mmap) */

uintnat caml_mem_round_up_pages(uintnat size);
/* The size given to caml_mem_map and caml_mem_commit must be a multiple of
   caml_plat_pagesize. The size given to caml_mem_unmap and caml_mem_decommit
   must match the size given to caml_mem_map/caml_mem_commit for mem.
*/
void* caml_mem_map(uintnat size, int reserve_only);
void* caml_mem_commit(void* mem, uintnat size);
void caml_mem_decommit(void* mem, uintnat size);
void caml_mem_unmap(void* mem, uintnat size);


CAMLnoret void caml_plat_fatal_error(const char * action, int err);

Caml_inline void check_err(const char* action, int err)
{
  if (err) caml_plat_fatal_error(action, err);
}

#ifdef DEBUG
CAMLextern CAMLthread_local int caml_lockdepth;
#define DEBUG_LOCK(m) (caml_lockdepth++)
#define DEBUG_UNLOCK(m) (caml_lockdepth--)
#else
#define DEBUG_LOCK(m)
#define DEBUG_UNLOCK(m)
#endif

Caml_inline void caml_plat_lock_blocking(caml_plat_mutex* m)
{
  check_err("lock", pthread_mutex_lock(m));
  DEBUG_LOCK(m);
}

Caml_inline int caml_plat_try_lock(caml_plat_mutex* m)
{
  int r = pthread_mutex_trylock(m);
  if (r == EBUSY) {
    return 0;
  } else {
    check_err("try_lock", r);
    DEBUG_LOCK(m);
    return 1;
  }
}

CAMLextern void caml_plat_lock_non_blocking_actual(caml_plat_mutex* m);

Caml_inline void caml_plat_lock_non_blocking(caml_plat_mutex* m)
{
  if (!caml_plat_try_lock(m)) {
    caml_plat_lock_non_blocking_actual(m);
  }
}

Caml_inline void caml_plat_unlock(caml_plat_mutex* m)
{
  DEBUG_UNLOCK(m);
  check_err("unlock", pthread_mutex_unlock(m));
}

extern intnat caml_plat_pagesize;
extern intnat caml_plat_mmap_alignment;

#endif /* CAML_INTERNALS */

#endif /* CAML_PLATFORM_H */
