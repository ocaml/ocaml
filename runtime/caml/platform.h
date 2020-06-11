#ifndef CAML_PLAT_THREADS_H
#define CAML_PLAT_THREADS_H
/* Platform-specific concurrency and memory primitives */

#ifdef __linux__
#define _GNU_SOURCE /* for PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP */
#endif
#include <pthread.h>
#include <errno.h>
#include <string.h>
#include "mlvalues.h"
#include "memory.h"

#if defined(MAP_ANON) && !defined(MAP_ANONYMOUS)
#define MAP_ANONYMOUS MAP_ANON
#endif

#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L) || defined(__GNUC__)
#define INLINE static inline
#else
#define INLINE static
#endif

/* Loads and stores with acquire and release semantics respectively */

INLINE void cpu_relax() {
#if defined(__x86_64__) || defined(__i386__)
  asm volatile("pause" ::: "memory");
#elif defined(__aarch64__)
  asm volatile ("yield" ::: "memory");
#else
  #warning "cpu_relax() undefined for this architecture!"
#endif
}

INLINE uintnat atomic_load_acq(atomic_uintnat* p) {
  return atomic_load_explicit(p, memory_order_acquire);
}

INLINE void atomic_store_rel(atomic_uintnat* p, uintnat v) {
  atomic_store_explicit(p, v, memory_order_release);
}

/* Spin-wait loops */

#define Max_spins 1000

unsigned caml_plat_spin_wait(unsigned spins,
                             const char* file, int line,
                             const char* function);

#define SPIN_WAIT                                                       \
  for (; 1; cpu_relax())

INLINE uintnat atomic_load_wait_nonzero(atomic_uintnat* p) {
  SPIN_WAIT {
    uintnat v = atomic_load_acq(p);
    if (v) return v;
  }
}

/* Atomic read-modify-write instructions, with full fences */

INLINE uintnat atomic_fetch_add_verify_ge0(atomic_uintnat* p, uintnat v) {
  uintnat result = atomic_fetch_add(p,v);
  CAMLassert ((intnat)result > 0);
  return result;
}


typedef pthread_mutex_t caml_plat_mutex;
#define CAML_PLAT_MUTEX_INITIALIZER PTHREAD_MUTEX_INITIALIZER
void caml_plat_mutex_init(caml_plat_mutex*);
static inline void caml_plat_lock(caml_plat_mutex*);
static inline int caml_plat_try_lock(caml_plat_mutex*);
void caml_plat_assert_locked(caml_plat_mutex*);
void caml_plat_assert_all_locks_unlocked();
static inline void caml_plat_unlock(caml_plat_mutex*);
void caml_plat_mutex_free(caml_plat_mutex*);
typedef struct { pthread_cond_t cond; caml_plat_mutex* mutex; } caml_plat_cond;
#define CAML_PLAT_COND_INITIALIZER(m) { PTHREAD_COND_INITIALIZER, m }
void caml_plat_cond_init(caml_plat_cond*, caml_plat_mutex*);
void caml_plat_wait(caml_plat_cond*);
/* like caml_plat_wait, but if caml_time_counter() surpasses the second parameter
   without a signal, then this function returns 1. */
int caml_plat_timedwait(caml_plat_cond*, int64_t);
void caml_plat_broadcast(caml_plat_cond*);
void caml_plat_signal(caml_plat_cond*);
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
  for (caml_plat_try_lock(caml__mutex) ||               \
         ((caml_enter_blocking_section(),               \
           caml_plat_lock(caml__mutex),                 \
           caml_leave_blocking_section()), 0);          \
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


static inline void check_err(char* action, int err)
{
  if (err) {
    caml_fatal_error_arg2("Fatal error during %s", action, ": %s\n", strerror(err));
  }
}

#ifdef DEBUG
static __thread int lockdepth;
#define DEBUG_LOCK(m) (lockdepth++)
#define DEBUG_UNLOCK(m) (lockdepth--)
#else
#define DEBUG_LOCK(m)
#define DEBUG_UNLOCK(m)
#endif

static inline void caml_plat_lock(caml_plat_mutex* m)
{
  check_err("lock", pthread_mutex_lock(m));
  DEBUG_LOCK(m);
}

static inline int caml_plat_try_lock(caml_plat_mutex* m)
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

static inline void caml_plat_unlock(caml_plat_mutex* m)
{
  DEBUG_UNLOCK(m);
  check_err("unlock", pthread_mutex_unlock(m));
}


#endif /* CAML_PLATFORM_H */
