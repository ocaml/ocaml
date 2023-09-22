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

#ifndef CAML_PLAT_THREADS_H
#define CAML_PLAT_THREADS_H
/* Platform-specific concurrency and memory primitives */

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


/* Atomic read-modify-write instructions, with full fences */

Caml_inline uintnat atomic_fetch_add_verify_ge0(atomic_uintnat* p, uintnat v) {
  uintnat result = atomic_fetch_add(p,v);
  CAMLassert ((intnat)result > 0);
  return result;
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
typedef pthread_cond_t caml_plat_cond;
#define CAML_PLAT_COND_INITIALIZER PTHREAD_COND_INITIALIZER
void caml_plat_cond_init(caml_plat_cond*);
void caml_plat_wait(caml_plat_cond*, caml_plat_mutex*); /* blocking */
void caml_plat_broadcast(caml_plat_cond*);
void caml_plat_signal(caml_plat_cond*);
void caml_plat_cond_free(caml_plat_cond*);

/* Futexes */

/* An integer that can be waited on and woken, used to build other
   synchronisation primitives; either uses OS facilities directly, or
   a condition-variable fallback */
typedef struct caml_plat_futex caml_plat_futex;

typedef uint32_t caml_plat_futex_value;
typedef _Atomic caml_plat_futex_value caml_plat_futex_word;

/* Block while `futex` has the value `undesired`, until woken by `wake_all` */
void caml_plat_futex_wait(caml_plat_futex* futex,
                          caml_plat_futex_value undesired);
/* Wake all threads `wait`ing on `futex` */
void caml_plat_futex_wake_all(caml_plat_futex* futex);

/* Define CAML_PLAT_FUTEX_FALLBACK to use the condition-variable
   fallback, even if a futex implementation is available */
#ifndef CAML_PLAT_FUTEX_FALLBACK
#  if defined(_WIN32)                                   \
  || (defined(__linux__) && defined(HAS_LINUX_FUTEX_H)) \
  || defined(__FreeBSD__) || defined(__OpenBSD__)
/*  These exist, but are untested
     defined(__NetBSD__) || defined(__DragonFly__) */
#  else
#    define CAML_PLAT_FUTEX_FALLBACK
#  endif
#endif

#if !defined(CAML_PLAT_FUTEX_FALLBACK)
struct caml_plat_futex {
  caml_plat_futex_word value;
};
#  define CAML_PLAT_FUTEX_INITIALIZER(value) { (value) }

Caml_inline void caml_plat_futex_init(caml_plat_futex* ftx,
                                      caml_plat_futex_value value) {
  ftx->value = value;
}
Caml_inline void caml_plat_futex_free(caml_plat_futex* ftx) {
  (void) ftx; /* noop */
}
#else
struct caml_plat_futex {
  caml_plat_futex_word value;
  caml_plat_mutex mutex;
  caml_plat_cond cond;
};
#  define CAML_PLAT_FUTEX_INITIALIZER(value) \
  { value, CAML_PLAT_MUTEX_INITIALIZER, CAML_PLAT_COND_INITIALIZER }

void caml_plat_futex_init(caml_plat_futex* ftx, caml_plat_futex_value value);
void caml_plat_futex_free(caml_plat_futex*);
#endif

/* Barriers */

/*
 * A barrier that can be either single-sense (separate release and
 * reset) or sense-reversing (unified release and reset).
 *
 * | Operation | `caml_plat_barrier_*` function      |
 * |           |---------------+---------------------|
 * |           | Single-sense  | Sense-reversing     |
 * |-----------|---------------+---------------------|
 * | Reset     | `reset`       | automatic at `flip` |
 * | Arrive    | `arrive`      | `arrive`            |
 * | Check     | `is_released` | `sense_has_flipped` |
 * | Block     | `wait`        | `wait_sense`        |
 * | Release   | `release`     | `flip`              |
 *
 * The lifecycle is as follows:
 *
 *      Reset (1 thread)          (other threads)
 *              |                       |
 *              +----------+------------+
 *                         |
 *                      Arrive (all threads)
 *                         |
 *                         | check arrival number
 *              +----------+------------+
 *              |                       |
 *       Check or Block              Release
 *     (non-final threads)         (final thread)
 *              |                       |
 */
typedef struct caml_plat_barrier {
  caml_plat_futex futex;
  atomic_uintnat arrived; /* includes sense bit */
} caml_plat_barrier;
#define CAML_PLAT_BARRIER_INITIALIZER \
  { CAML_PLAT_FUTEX_INITIALIZER(0), ATOMIC_UINTNAT_INIT(0) }

typedef uintnat barrier_status;
/* Arrive at the barrier, returns the number of parties that have
   arrived at the barrier (including this one); the caller should
   check whether it is the last expected party to arrive, and release
   or flip the barrier if so.

   In a sense-reversing barrier, this also encodes the current sense
   of the barrier in BARRIER_SENSE_BIT, which should be masked off if
   checking for the last arrival. */
Caml_inline barrier_status caml_plat_barrier_arrive(caml_plat_barrier* barrier)
{
  return 1 + atomic_fetch_add(&barrier->arrived, 1);
}
#define BARRIER_SENSE_BIT 0x100000

/* -- Single-sense --

   Futex states:
   - 0 if released
   - 1 if nobody is blocking (but they may be spinning)
   - 2 if anybody is blocking (or about to)
 */
#define Barrier_released 0
#define Barrier_unreleased 1
#define Barrier_contested 2

/* Reset the barrier to 0 arrivals, block new waiters */
Caml_inline void caml_plat_barrier_reset(caml_plat_barrier* barrier) {
  atomic_store_relaxed(&barrier->futex.value, Barrier_unreleased);
  /* threads check arrivals before the futex, 'release' ordering
     ensures they see it reset */
  atomic_store_release(&barrier->arrived, 0);
}
/* Check if the barrier has been released */
Caml_inline int caml_plat_barrier_is_released(caml_plat_barrier* barrier) {
  return atomic_load_acquire(&barrier->futex.value) == Barrier_released;
}

/* Release and wait, but on a futex only.

   This is like a(n inverted) binary semaphore, but with no decrement
   on `wait`. That is, `release` sets the futex to 0, which `wait`
   waits for.

   A futex used this way is 0 (Barrier_released) when released, and
   nonzero otherwise. It should be set to 1 (Barrier_unreleased) to
   block.
*/
void caml_plat_barrier_raw_release(caml_plat_futex* futex);
void caml_plat_barrier_raw_wait(caml_plat_futex* futex);

/* Release the barrier unconditionally, letting all parties through */
Caml_inline void caml_plat_barrier_release(caml_plat_barrier* barrier) {
  caml_plat_barrier_raw_release(&barrier->futex);
}
/* Block until released */
Caml_inline void caml_plat_barrier_wait(caml_plat_barrier* barrier) {
  caml_plat_barrier_raw_wait(&barrier->futex);
}

/* -- Sense-reversing -- */
/* Flip the sense of the barrier, releasing current waiters and
   blocking new ones.

   `current_sense` should be just `(b & BARRIER_SENSE_BIT)`
   with b as returned by `caml_plat_barrier_arrive`. */
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

/* Spin-wait loops */

#define GENSYM_3(name, l) name##l
#define GENSYM_2(name, l) GENSYM_3(name, l)
#define GENSYM(name) GENSYM_2(name, __LINE__)

#define Max_spins_long 1000
#define Max_spins_medium 300
#define Max_spins_short 30

/* Spin up to some number of times, should be used when we have useful
   work to do, or expect the condition to come true fast */
#define SPIN_WAIT_BOUNDED SPIN_WAIT_NTIMES(Max_spins_medium)
#define SPIN_WAIT_BOUNDED_LONG SPIN_WAIT_NTIMES(Max_spins_long)
#define SPIN_WAIT_NTIMES(N)                             \
  unsigned GENSYM(caml__spins) = 0;                     \
  unsigned GENSYM(caml__max_spins) = (N);               \
  for (; GENSYM(caml__spins) < GENSYM(caml__max_spins); \
       cpu_relax(), ++GENSYM(caml__spins))

/* Spin for unbounded time, this should only be used when there is a
   very short critical section we are waiting on */
#define SPIN_WAIT SPIN_WAIT_BACK_OFF(Max_spins_long)

struct caml_plat_srcloc {
  const char* file;
  int line;
  const char* function;
};

CAMLextern unsigned caml_plat_spin_wait(unsigned spins,
                                        const struct caml_plat_srcloc* loc);

Caml_inline unsigned caml_plat_spin_step(unsigned spins,
                                         unsigned max_spins,
                                         const struct caml_plat_srcloc *loc) {
  cpu_relax();
  if (CAMLlikely(spins < max_spins)) {
    return spins + 1;
  } else {
    return caml_plat_spin_wait(spins, loc);
  }
}

#define SPIN_WAIT_BACK_OFF(max_spins)                        \
  unsigned GENSYM(caml__spins) = 0;                          \
  unsigned GENSYM(caml__max_spins) = (max_spins);            \
  static const struct caml_plat_srcloc GENSYM(caml__loc) = { \
    __FILE__, __LINE__, __func__                             \
  };                                                         \
  for (; 1; GENSYM(caml__spins) = caml_plat_spin_step(       \
         GENSYM(caml__spins), GENSYM(caml__max_spins),       \
         &GENSYM(caml__loc)))

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
