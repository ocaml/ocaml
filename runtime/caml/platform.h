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
#else
  /* Just a compiler barrier */
  __asm__ volatile ("" ::: "memory");
#endif
#endif
}

/* Loads and stores with acquire and release semantics respectively */

Caml_inline uintnat atomic_load_acq(atomic_uintnat* p)
{
  return atomic_load_explicit(p, memory_order_acquire);
}

Caml_inline uintnat atomic_load_relaxed(atomic_uintnat* p)
{
  return atomic_load_explicit(p, memory_order_relaxed);
}

Caml_inline void atomic_store_rel(atomic_uintnat* p, uintnat v)
{
  atomic_store_explicit(p, v, memory_order_release);
}

Caml_inline void atomic_store_relaxed(atomic_uintnat* p, uintnat v)
{
  atomic_store_explicit(p, v, memory_order_relaxed);
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
    uintnat v = atomic_load_acq(p);
    if (v) return v;
  }
}

/* Atomic read-modify-write instructions, with full fences */

Caml_inline uintnat atomic_fetch_add_verify_ge0(atomic_uintnat* p, uintnat v) {
  uintnat result = atomic_fetch_add(p,v);
  CAMLassert ((intnat)result > 0);
  return result;
}


typedef pthread_mutex_t caml_plat_mutex;
#define CAML_PLAT_MUTEX_INITIALIZER PTHREAD_MUTEX_INITIALIZER
void caml_plat_mutex_init(caml_plat_mutex*);
Caml_inline void caml_plat_lock(caml_plat_mutex*);
Caml_inline int caml_plat_try_lock(caml_plat_mutex*);
void caml_plat_assert_locked(caml_plat_mutex*);
void caml_plat_assert_all_locks_unlocked(void);
Caml_inline void caml_plat_unlock(caml_plat_mutex*);
void caml_plat_mutex_free(caml_plat_mutex*);
typedef struct { pthread_cond_t cond; caml_plat_mutex* mutex; } caml_plat_cond;
#define CAML_PLAT_COND_INITIALIZER(m) { PTHREAD_COND_INITIALIZER, m }
void caml_plat_cond_init(caml_plat_cond*, caml_plat_mutex*);
void caml_plat_wait(caml_plat_cond*);
/* like caml_plat_wait, but if nanoseconds surpasses the second parameter
   without a signal, then this function returns 1. */
void caml_plat_broadcast(caml_plat_cond*);
void caml_plat_signal(caml_plat_cond*);
void caml_plat_cond_free(caml_plat_cond*);

/* Memory management primitives (mmap) */

uintnat caml_mem_round_up_pages(uintnat size);
void* caml_mem_map(uintnat size, uintnat alignment, int reserve_only);
void* caml_mem_commit(void* mem, uintnat size);
void caml_mem_decommit(void* mem, uintnat size);
void caml_mem_unmap(void* mem, uintnat size);


CAMLnoreturn_start
void caml_plat_fatal_error(const char * action, int err)
CAMLnoreturn_end;

Caml_inline void check_err(const char* action, int err)
{
  if (err) caml_plat_fatal_error(action, err);
}

#ifdef DEBUG
static __thread int lockdepth;
#define DEBUG_LOCK(m) (lockdepth++)
#define DEBUG_UNLOCK(m) (lockdepth--)
#else
#define DEBUG_LOCK(m)
#define DEBUG_UNLOCK(m)
#endif

Caml_inline void caml_plat_lock(caml_plat_mutex* m)
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

Caml_inline void caml_plat_unlock(caml_plat_mutex* m)
{
  DEBUG_UNLOCK(m);
  check_err("unlock", pthread_mutex_unlock(m));
}

/* On Windows, the SYSTEM_INFO.dwPageSize is a DWORD (32-bit), but conveniently
   long is also 32-bit */
extern long caml_sys_pagesize;

#endif /* CAML_INTERNALS */

#endif /* CAML_PLATFORM_H */
