/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2009 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* POSIX thread implementation of the "st" interface */

#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

typedef int st_retcode;

/* Variables used to stop "tick" threads */
static atomic_uintnat tick_thread_stop[Max_domains];
#define Tick_thread_stop tick_thread_stop[Caml_state->id]

/* OS-specific initialization */

static int st_initialize(void)
{
  atomic_store_rel(&Tick_thread_stop, 0);
  return 0;
}

/* Thread creation.  Created in detached mode if [res] is NULL. */

typedef pthread_t st_thread_id;


static int st_thread_create(st_thread_id * res,
                            void * (*fn)(void *), void * arg)
{
  pthread_t thr;
  pthread_attr_t attr;
  int rc;

  pthread_attr_init(&attr);
  if (res == NULL) pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  rc = pthread_create(&thr, &attr, fn, arg);
  if (res != NULL) *res = thr;
  return rc;
}

#define ST_THREAD_FUNCTION void *

/* Thread termination */

static void st_thread_join(st_thread_id thr)
{
  pthread_join(thr, NULL);
  /* best effort: ignore errors */
}

/* Thread-specific state */

typedef pthread_key_t st_tlskey;

static int st_tls_newkey(st_tlskey * res)
{
  return pthread_key_create(res, NULL);
}

Caml_inline void * st_tls_get(st_tlskey k)
{
  return pthread_getspecific(k);
}

Caml_inline void st_tls_set(st_tlskey k, void * v)
{
  pthread_setspecific(k, v);
}

/* The master lock.  This is a mutex that is held most of the time,
   so we implement it in a slightly convoluted way to avoid
   all risks of busy-waiting.  Also, we count the number of waiting
   threads. */

typedef struct {
  int init;                       /* have the mutex and the cond been
                                     initialized already? */
  pthread_mutex_t lock;           /* to protect contents */
  uintnat busy;                   /* 0 = free, 1 = taken */
  atomic_uintnat waiters;         /* number of threads waiting on master lock */
  pthread_cond_t is_free;         /* signaled when free */
} st_masterlock;

static void st_masterlock_init(st_masterlock * m)
{
  if (!m->init) {
    // FIXME: check errors
    pthread_mutex_init(&m->lock, NULL);
    pthread_cond_init(&m->is_free, NULL);
    m->init = 1;
  }
  m->busy = 1;
  atomic_store_rel(&m->waiters, 0);

  return;
};

static uintnat st_masterlock_waiters(st_masterlock * m)
{
  return atomic_load_acq(&m->waiters);
}

static void st_bt_lock_acquire(st_masterlock *m) {

  /* We do not want to signal the backup thread is it is not "working"
     as it may very well not be, because we could have just resumed
     execution from another thread right away. */
  if (caml_bt_is_in_blocking_section()) {
    caml_bt_enter_ocaml();
  }

  caml_acquire_domain_lock();

  return;
}

static void st_bt_lock_release(st_masterlock *m) {

  /* Here we do want to signal the backup thread iff there's
     no thread waiting to be scheduled, and the backup thread is currently
     idle. */
  if (st_masterlock_waiters(m) == 0 &&
      caml_bt_is_in_blocking_section() == 0) {
    caml_bt_exit_ocaml();
  }

  caml_release_domain_lock();

  return;
}

static void st_masterlock_acquire(st_masterlock *m)
{
  pthread_mutex_lock(&m->lock);
  while (m->busy) {
    atomic_fetch_add(&m->waiters, +1);
    pthread_cond_wait(&m->is_free, &m->lock);
    atomic_fetch_add(&m->waiters, -1);
  }
  m->busy = 1;
  st_bt_lock_acquire(m);
  pthread_mutex_unlock(&m->lock);

  return;
}

static void st_masterlock_release(st_masterlock * m)
{
  pthread_mutex_lock(&m->lock);
  m->busy = 0;
  st_bt_lock_release(m);
  pthread_cond_signal(&m->is_free);
  pthread_mutex_unlock(&m->lock);

  return;
}

/* Scheduling hints */

/* This is mostly equivalent to release(); acquire(), but better. In particular,
   release(); acquire(); leaves both us and the waiter we signal() racing to
   acquire the lock. Calling yield or sleep helps there but does not solve the
   problem. Sleeping ourselves is much more reliable--and since we're handing
   off the lock to a waiter we know exists, it's safe, as they'll certainly
   re-wake us later.
*/
Caml_inline void st_thread_yield(st_masterlock * m)
{
  pthread_mutex_lock(&m->lock);
  /* We must hold the lock to call this. */

  /* We already checked this without the lock, but we might have raced--if
     there's no waiter, there's nothing to do and no one to wake us if we did
     wait, so just keep going. */
  uintnat waiters = st_masterlock_waiters(m);

  if (waiters == 0) {
    pthread_mutex_unlock(&m->lock);
    return;
  }

  m->busy = 0;
  atomic_fetch_add(&m->waiters, +1);
  pthread_cond_signal(&m->is_free);
  /* releasing the domain lock but not triggering bt messaging
     messaging the bt should not be required because yield assumes
     that a thread will resume execution (be it the yielding thread
     or a waiting thread */
  caml_release_domain_lock();

  do {
    /* Note: the POSIX spec prevents the above signal from pairing with this
       wait, which is good: we'll reliably continue waiting until the next
       yield() or enter_blocking_section() call (or we see a spurious condvar
       wakeup, which are rare at best.) */
       pthread_cond_wait(&m->is_free, &m->lock);
  } while (m->busy);

  m->busy = 1;
  atomic_fetch_add(&m->waiters, -1);

  caml_acquire_domain_lock();

  pthread_mutex_unlock(&m->lock);

  return;
}

/* Triggered events */

typedef struct st_event_struct {
  pthread_mutex_t lock;         /* to protect contents */
  int status;                   /* 0 = not triggered, 1 = triggered */
  pthread_cond_t triggered;     /* signaled when triggered */
} * st_event;


static int st_event_create(st_event * res)
{
  int rc;
  st_event e = caml_stat_alloc_noexc(sizeof(struct st_event_struct));
  if (e == NULL) return ENOMEM;
  rc = pthread_mutex_init(&e->lock, NULL);
  if (rc != 0) { caml_stat_free(e); return rc; }
  rc = pthread_cond_init(&e->triggered, NULL);
  if (rc != 0)
  { pthread_mutex_destroy(&e->lock); caml_stat_free(e); return rc; }
  e->status = 0;
  *res = e;
  return 0;
}

static int st_event_destroy(st_event e)
{
  int rc1, rc2;
  rc1 = pthread_mutex_destroy(&e->lock);
  rc2 = pthread_cond_destroy(&e->triggered);
  caml_stat_free(e);
  return rc1 != 0 ? rc1 : rc2;
}

static int st_event_trigger(st_event e)
{
  int rc;
  rc = pthread_mutex_lock(&e->lock);
  if (rc != 0) return rc;
  e->status = 1;
  rc = pthread_mutex_unlock(&e->lock);
  if (rc != 0) return rc;
  rc = pthread_cond_broadcast(&e->triggered);
  return rc;
}

static int st_event_wait(st_event e)
{
  int rc;
  rc = pthread_mutex_lock(&e->lock);
  if (rc != 0) return rc;
  while(e->status == 0) {
    rc = pthread_cond_wait(&e->triggered, &e->lock);
    if (rc != 0) return rc;
  }
  rc = pthread_mutex_unlock(&e->lock);
  return rc;
}

/* The tick thread: interrupt the domain periodically to force preemption  */

static void * caml_thread_tick(void * arg)
{
  int *domain_id = (int *) arg;

  caml_init_domain_self(*domain_id);
  caml_domain_state *domain = Caml_state;

  while(! atomic_load_acq(&Tick_thread_stop)) {
    st_msleep(Thread_timeout);

    atomic_store_rel(&domain->requested_external_interrupt, 1);
    caml_interrupt_self();
  }
  return NULL;
}
