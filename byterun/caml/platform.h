#ifndef CAML_PLAT_THREADS_H
#define CAML_PLAT_THREADS_H
/* Platform-specific concurrency and memory primitives */

#ifdef __linux__
#define _GNU_SOURCE /* for PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP */
#endif
#include <pthread.h>
#include "mlvalues.h"
#include "memory.h"

/*
FIXME: This file should use C11 atomics if they are available.

#if __STDC_VERSION__ >= 201112L
... stuff ...
#endif
*/

#if defined(MAP_ANON) && !defined(MAP_ANONYMOUS)
#define MAP_ANONYMOUS MAP_ANON
#endif

#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L) || defined(__GNUC__)
#define INLINE static inline
#else
#define INLINE static
#endif

/* Loads and stores with acquire and release semantics respectively */

#if defined(__x86_64__) || defined(__i386__)

#if defined(__GNUC__)
#define compiler_barrier() __asm__ __volatile__ ("" ::: "memory");
#else
#error "no compiler barrier defined for this compiler"
#endif

INLINE void cpu_relax() {
  asm volatile("pause" ::: "memory");
}

#define ATOMIC_UINTNAT_INIT(x) { (x) }

/* On x86, all loads are acquire and all stores are release. So, only
   compiler barriers are necessary in the following. */

INLINE uintnat atomic_load_acq(atomic_uintnat* p) {
  uintnat v = p->val;
  compiler_barrier();
  return v;
}

INLINE void atomic_store_rel(atomic_uintnat* p, uintnat v) {
  compiler_barrier();
  p->val = v;
}

#elif defined(__aarch64__)

#if defined(__GNUC__)
#define dmb() __asm__ __volatile__ ("dmb sy" ::: "memory");
#else
#error "no compiler barrier defined for this compiler"
#endif

#define ATOMIC_UINTNAT_INIT(x) { (x) }

INLINE void cpu_relax() {
  asm volatile ("yield" ::: "memory");
}

INLINE uintnat atomic_load_acq(atomic_uintnat* p) {
  uintnat v;
  dmb();
  v = p->val;
  dmb();
  return v;
}

INLINE void atomic_store_rel(atomic_uintnat* p, uintnat v) {
  dmb();
  p->val = v;
  dmb();
}

#else
#error "unsupported platform (i.e. not x86)"
#endif

/* Spin-wait loops */

#define Max_spins 1000

unsigned caml_plat_spin_wait(unsigned spins,
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

INLINE uintnat atomic_load_wait_nonzero(atomic_uintnat* p) {
  SPIN_WAIT {
    uintnat v = atomic_load_acq(p);
    if (v) return v;
  }
}

/* Atomic read-modify-write instructions, with full fences */

#if defined(__GNUC__)

/* atomically: old = *p; *p += v; return old; */
INLINE uintnat atomic_fetch_add(atomic_uintnat* p, uintnat v) {
  return __sync_fetch_and_add(&p->val, v);
}

INLINE uintnat atomic_fetch_add_verify_ge0(atomic_uintnat* p, uintnat v) {
  uintnat result = atomic_fetch_add(p,v);
  CAMLassert ((intnat)result > 0);
  return result;
}

/* atomically: if (*p == vold) { *p = vnew; return 1; } else { return 0; }
   may spuriously return 0 even when *p == vold */
INLINE int atomic_cas(atomic_uintnat* p, uintnat vold, uintnat vnew) {
  return __sync_bool_compare_and_swap(&p->val, vold, vnew);
}

/* atomically: if (*p == vold) { *p = vnew; } */
INLINE void atomic_cas_strong(atomic_uintnat* p, uintnat vold, uintnat vnew) {
  __sync_val_compare_and_swap(&p->val, vold, vnew);
}

#else
#error "unsupported platform"
#endif

typedef pthread_mutex_t caml_plat_mutex;
#define CAML_PLAT_MUTEX_INITIALIZER PTHREAD_MUTEX_INITIALIZER
void caml_plat_mutex_init(caml_plat_mutex*);
void caml_plat_lock(caml_plat_mutex*);
int caml_plat_try_lock(caml_plat_mutex*);
void caml_plat_assert_locked(caml_plat_mutex*);
void caml_plat_assert_all_locks_unlocked();
void caml_plat_unlock(caml_plat_mutex*);
void caml_plat_mutex_free(caml_plat_mutex*);
typedef struct { pthread_cond_t cond; caml_plat_mutex* mutex; } caml_plat_cond;
#define CAML_PLAT_COND_INITIALIZER(m) { PTHREAD_COND_INITIALIZER, m }
void caml_plat_cond_init(caml_plat_cond*, caml_plat_mutex*);
void caml_plat_wait(caml_plat_cond*);
/* like caml_plat_wait, but if caml_time_counter() surpasses the second parameter
   without a signal, then this function returns 1. */
int caml_plat_timedwait(caml_plat_cond*, int64_t);
void caml_plat_broadcast(caml_plat_cond*);
void caml_plat_cond_free(caml_plat_cond*);

struct caml__mutex_unwind {
  caml_plat_mutex* mutex;
  struct caml__mutex_unwind* next;
};

#define With_mutex(mutex)                               \
  Assert(CAML_LOCAL_ROOTS);                             \
  caml_plat_mutex* caml__mutex = (mutex);               \
  int caml__mutex_go = 1;                               \
  struct caml__mutex_unwind caml__locked_mutex =        \
    { caml__mutex, CAML_LOCAL_ROOTS->mutexes };         \
  CAML_LOCAL_ROOTS->mutexes = &caml__locked_mutex;      \
  for (caml_enter_blocking_section(),                   \
         caml_plat_lock(caml__mutex),                   \
         caml_leave_blocking_section();                 \
       caml__mutex_go;                                  \
       caml_plat_unlock(caml__mutex),                   \
         caml__mutex_go = 0,                            \
         CAML_LOCAL_ROOTS->mutexes =                    \
         CAML_LOCAL_ROOTS->mutexes->next)

/* Memory management primitives (mmap) */

uintnat caml_mem_round_up_pages(uintnat size);
void* caml_mem_map(uintnat size, uintnat alignment, int reserve_only);
void* caml_mem_commit(void* mem, uintnat size);
void caml_mem_decommit(void* mem, uintnat size);
void caml_mem_unmap(void* mem, uintnat size);

#endif /* CAML_PLATFORM_H */
