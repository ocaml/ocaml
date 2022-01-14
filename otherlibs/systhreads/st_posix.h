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

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <signal.h>
#include <time.h>
#include <caml/sync.h>
#include <sys/time.h>
#ifdef __linux__
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

/* Cleanup at thread exit */

Caml_inline void st_thread_cleanup(void)
{
  return;
}

/* Thread termination */

CAMLnoreturn_start
static void st_thread_exit(void)
CAMLnoreturn_end;

static void st_thread_exit(void)
{
#ifdef _WIN32
  ExitThread(0);
#else
  pthread_exit(NULL);
#endif
}

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

/* Windows-specific hook. */
Caml_inline void st_thread_set_id(intnat id)
{
  return;
}

/* The master lock.  This is a mutex that is held most of the time,
   so we implement it in a slightly convoluted way to avoid
   all risks of busy-waiting.  Also, we count the number of waiting
   threads. */

typedef struct {
  pthread_mutex_t lock;           /* to protect contents */
  uintnat busy;                   /* 0 = free, 1 = taken */
  atomic_uintnat waiters;         /* number of threads waiting on master lock */
  pthread_cond_t is_free;         /* signaled when free */
} st_masterlock;

static void st_masterlock_init(st_masterlock * m)
{

  pthread_mutex_init(&m->lock, NULL);
  pthread_cond_init(&m->is_free, NULL);
  m->busy = 1;
  atomic_store_rel(&m->waiters, 0);

  return;
};

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
  if (atomic_load_acq(&m->waiters) == 0 &&
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
  uintnat waiters;

  pthread_mutex_lock(&m->lock);
  /* We must hold the lock to call this. */

  /* We already checked this without the lock, but we might have raced--if
     there's no waiter, there's nothing to do and no one to wake us if we did
     wait, so just keep going. */
  waiters = atomic_load_acq(&m->waiters);

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
  caml_domain_state *domain;
  uintnat *domain_id = (uintnat *) arg;
  struct timeval timeout;

  caml_init_domain_self(*domain_id);
  domain = Caml_state;

  caml_domain_set_name("Tick");
  while(! atomic_load_acq(&Tick_thread_stop)) {
    /* select() seems to be the most efficient way to suspend the
       thread for sub-second intervals */
    timeout.tv_sec = 0;
    timeout.tv_usec = Thread_timeout * 1000;
    select(0, NULL, NULL, NULL, &timeout);

    atomic_store_rel((atomic_uintnat*)&domain->requested_external_interrupt, 1);
    caml_interrupt_self();
  }
  return NULL;
}

/* "At fork" processing */

#if defined(__ANDROID__)
/* Android's libc does not include declaration of pthread_atfork;
   however, it implements it since API level 10 (Gingerbread).
   The reason for the omission is that Android (GUI) applications
   are not supposed to fork at all, however this workaround is still
   included in case OCaml is used for an Android CLI utility. */
int pthread_atfork(void (*prepare)(void), void (*parent)(void),
                   void (*child)(void));
#endif

static int st_atfork(void (*fn)(void))
{
  caml_atfork_hook = fn;
  return 0;
}

/* Signal handling */

#ifndef _WIN32
static void st_decode_sigset(value vset, sigset_t * set)
{
  sigemptyset(set);
  while (vset != Val_int(0)) {
    int sig = caml_convert_signal_number(Int_val(Field(vset, 0)));
    sigaddset(set, sig);
    vset = Field(vset, 1);
  }
}

#ifndef NSIG
#define NSIG 64
#endif

static value st_encode_sigset(sigset_t * set)
{
  value res = Val_int(0);
  int i;

  Begin_root(res)
    for (i = 1; i < NSIG; i++)
      if (sigismember(set, i) > 0) {
        value newcons = caml_alloc_small(2, 0);
        Field(newcons, 0) = Val_int(caml_rev_convert_signal_number(i));
        Field(newcons, 1) = res;
        res = newcons;
      }
  End_roots();
  return res;
}

static int sigmask_cmd[3] = { SIG_SETMASK, SIG_BLOCK, SIG_UNBLOCK };
#endif

value caml_thread_sigmask(value cmd, value sigs) /* ML */
{
#ifndef _WIN32
  int how;
  sigset_t set, oldset;
  int retcode;

  how = sigmask_cmd[Int_val(cmd)];
  st_decode_sigset(sigs, &set);
  caml_enter_blocking_section();
  retcode = pthread_sigmask(how, &set, &oldset);
  caml_leave_blocking_section();
  sync_check_error(retcode, "Thread.sigmask");
  /* Run any handlers for just-unmasked pending signals */
  caml_process_pending_signals();
  return st_encode_sigset(&oldset);
#else
  caml_invalid_argument("Thread.sigmask not implemented");
  return Val_int(0);            /* not reached */
#endif
}

value caml_wait_signal(value sigs) /* ML */
{
#ifdef HAS_SIGWAIT
  sigset_t set;
  int retcode, signo;

  st_decode_sigset(sigs, &set);
  caml_enter_blocking_section();
  retcode = sigwait(&set, &signo);
  caml_leave_blocking_section();
  sync_check_error(retcode, "Thread.wait_signal");
  return Val_int(caml_rev_convert_signal_number(signo));
#else
  caml_invalid_argument("Thread.wait_signal not implemented");
  return Val_int(0);            /* not reached */
#endif
}
