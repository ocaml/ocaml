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

/* Spin-wait loops */

#define Max_spins 1000

CAMLextern unsigned caml_plat_spin_wait(unsigned spins,
                                        const char* file, int line,
                                        const char* function);

#define GENSYM_3(name, l) name##l
#define GENSYM_2(name, l) GENSYM_3(name, l)
#define GENSYM(name) GENSYM_2(name, __LINE__)

#define SPIN_WAIT                                                       \
  unsigned GENSYM(caml__spins) = 0;                                     \
  for (; 1; cpu_relax(),                                                \
         GENSYM(caml__spins) =                                          \
           CAMLlikely(GENSYM(caml__spins) < Max_spins) ?                \
         GENSYM(caml__spins) + 1 :                                      \
         caml_plat_spin_wait(GENSYM(caml__spins),                       \
                             __FILE__, __LINE__, __func__))

Caml_inline uintnat atomic_load_wait_nonzero(atomic_uintnat* p) {
  SPIN_WAIT {
    uintnat v = atomic_load_acquire(p);
    if (v) return v;
  }
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
