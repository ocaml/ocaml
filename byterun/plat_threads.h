#ifndef CAML_PLAT_THREADS_H
#define CAML_PLAT_THREADS_H
/* Platform-specific concurrency primitives */

#include <pthread.h>
#include "mlvalues.h"
#include "memory.h"

/*
FIXME: This file should use C11 atomics if they are available.

#if __STDC_VERSION__ >= 201112L
... stuff ...
#endif
*/



#if (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L) || defined(__GNUC__)
#define INLINE static inline
#else
#define INLINE static
#endif

#if defined(__GNUC__)
#define compiler_barrier() __asm__ __volatile__ ("" ::: "memory");
#else
#error "no compiler barrier defined for this compiler"
#endif



/* Loads and stores with acquire and release semantics respectively */

#if defined(__x86_64__) || defined(__i386__)

INLINE void cpu_relax() {
  asm volatile("pause" ::: "memory");
}

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

#else
#error "unsupported platform (i.e. not x86)"
#endif


INLINE uintnat atomic_load_wait_nonzero(atomic_uintnat* p) {
  while (1) {
    uintnat v = atomic_load_acq(p);
    if (v) return v;
    cpu_relax();
  }
}




/* Atomic read-modify-write instructions, with full fences */

#if defined(__GNUC__)

/* atomically: old = *p; *p += v; return old; */
INLINE uintnat atomic_fetch_add(atomic_uintnat* p, uintnat v) {
  return __sync_fetch_and_add(&p->val, v);
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




typedef pthread_mutex_t plat_mutex;
#define plat_mutex_init(m) pthread_mutex_init(m, 0);
#define plat_mutex_lock pthread_mutex_lock
#define plat_mutex_try_lock(l) (pthread_mutex_trylock(l) == 0)
#define plat_mutex_unlock pthread_mutex_unlock
#define plat_mutex_free pthread_mutex_destroy

struct caml__mutex_unwind {
  plat_mutex* mutex;
  struct caml__mutex_unwind* next;
};

#define With_mutex(mutex)                               \
  Assert(caml_local_roots);                             \
  plat_mutex* caml__mutex = (mutex);                    \
  int caml__mutex_go = 1;                               \
  struct caml__mutex_unwind caml__locked_mutex =        \
    { caml__mutex, caml_local_roots->mutexes };         \
  caml_local_roots->mutexes = &caml__locked_mutex;      \
  for (caml_enter_blocking_section(),                   \
         plat_mutex_lock(caml__mutex),                  \
         caml_leave_blocking_section();                 \
       caml__mutex_go;                                  \
       plat_mutex_unlock(caml__mutex),                  \
         caml__mutex_go = 0,                            \
         caml_local_roots->mutexes =                    \
         caml_local_roots->mutexes->next)



/* Better implementations of shared_stack can use CAS (+ABA protection) or LL/SC */

typedef struct shared_stack_node {
  struct shared_stack_node* next;
} shared_stack_node;

typedef struct shared_stack {
  plat_mutex lock;
  struct shared_stack_node first;
} shared_stack;

#define SHARED_STACK_INIT { PTHREAD_MUTEX_INITIALIZER, { 0 } }

INLINE void shared_stack_init(shared_stack* stk) {
  stk->first.next = 0;
  plat_mutex_init(&stk->lock);
}

INLINE void shared_stack_push(shared_stack* stk, shared_stack_node* node) {
  plat_mutex_lock(&stk->lock);
  node->next = stk->first.next;
  stk->first.next = node;
  plat_mutex_unlock(&stk->lock);
}

INLINE void* shared_stack_pop(shared_stack* stk) {
  plat_mutex_lock(&stk->lock);
  shared_stack_node* n = stk->first.next;
  if (n) {
    stk->first.next = n->next;
  }
  plat_mutex_unlock(&stk->lock);
  return n;
}


#endif /* CAML_PLAT_THREADS_H */
