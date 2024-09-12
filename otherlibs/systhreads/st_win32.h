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

/* Win32 implementation of the "st" interface */

#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0400
#include <windows.h>

Caml_inline void st_msleep(int msec)
{
  Sleep(msec);
}

#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <time.h>
#include <caml/osdeps.h>


typedef HANDLE st_thread_id;

/* Thread creation. Created in detached mode if [res] is NULL. */
static int st_thread_create(st_thread_id * res,
                            unsigned ( WINAPI *start_address )( void * ),
                            void * arg)
{
  st_thread_id thr;
  thr = (st_thread_id) _beginthreadex(
    NULL, /* security: handle can't be inherited */
    0,    /* stack size */
    start_address,
    arg,
    0,    /* run immediately */
    NULL  /* thread identifier */
    );
  if (thr == 0)
    return errno;

  if (res == NULL) {
    /* detach */
    if (!CloseHandle(thr)) {
      int err = caml_posixerr_of_win32err(GetLastError());
      return err == 0 ? EINVAL : err;
    }
  } else {
    *res = thr;
  }
  return 0;
}

/* Thread termination */

static void st_thread_join(st_thread_id thr)
{
  WaitForSingleObject(thr, INFINITE);
  /* best effort: ignore errors */
}

/* Thread-specific state */

typedef DWORD st_tlskey;

static int st_tls_newkey(st_tlskey * res)
{
  DWORD index = TlsAlloc();
  if (index == TLS_OUT_OF_INDEXES) {
    int err = caml_posixerr_of_win32err(GetLastError());
    return err == 0 ? EINVAL : err;
  }
  *res = index;
  return 0;
}

Caml_inline void * st_tls_get(st_tlskey k)
{
  /* No errors are returned from pthread_getspecific(). Ignore
   * TlsGetValue() errors.  */
  return TlsGetValue(k);
}

Caml_inline void st_tls_set(st_tlskey k, void * v)
{
  (void)TlsSetValue(k, v);
}

/* The master lock.  This is a mutex that is held most of the time,
   so we implement it in a slightly convoluted way to avoid
   all risks of busy-waiting.  Also, we count the number of waiting
   threads. */

typedef struct {
  atomic_bool init;               /* have the mutex and the cond been
                                     initialized already? */
  SRWLOCK lock;                   /* to protect contents */
  atomic_bool busy;               /* false = free, true = taken */
  atomic_uintnat waiters;         /* number of threads waiting on master lock */
  CONDITION_VARIABLE is_free;     /* signaled when free */
} st_masterlock;

/* Returns non-zero on failure */
static int st_masterlock_init(st_masterlock * m)
{
  if (!m->init) {
    InitializeSRWLock(&m->lock);
    InitializeConditionVariable(&m->is_free);
    m->init = true;
  }
  m->busy = true;
  atomic_store_release(&m->waiters, 0);
  return 0;
}

static uintnat st_masterlock_waiters(st_masterlock * m)
{
  return atomic_load_acquire(&m->waiters);
}

static void st_masterlock_acquire(st_masterlock *m)
{
  AcquireSRWLockExclusive(&m->lock);
  while (m->busy) {
    atomic_fetch_add(&m->waiters, +1);
    SleepConditionVariableSRW(&m->is_free, &m->lock,
                              INFINITE, 0 /* exclusive */);
    atomic_fetch_add(&m->waiters, -1);
  }
  m->busy = true;
  st_bt_lock_acquire();
  ReleaseSRWLockExclusive(&m->lock);

  return;
}

static void st_masterlock_release(st_masterlock * m)
{
  AcquireSRWLockExclusive(&m->lock);
  m->busy = false;
  st_bt_lock_release(st_masterlock_waiters(m) == 0);
  WakeConditionVariable(&m->is_free);
  ReleaseSRWLockExclusive(&m->lock);

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
  AcquireSRWLockExclusive(&m->lock);
  /* We must hold the lock to call this. */

  /* We already checked this without the lock, but we might have raced--if
     there's no waiter, there's nothing to do and no one to wake us if we did
     wait, so just keep going. */
  uintnat waiters = st_masterlock_waiters(m);

  if (waiters == 0) {
    ReleaseSRWLockExclusive(&m->lock);
    return;
  }

  m->busy = false;
  atomic_fetch_add(&m->waiters, +1);
  WakeConditionVariable(&m->is_free);
  /* Releasing the domain lock but not triggering bt messaging.
     Messaging the bt should not be required because yield assumes
     that a thread will resume execution (either the yielding thread
     or a waiting thread). */
  caml_release_domain_lock();

  do {
    /* Note: the POSIX spec prevents the above signal from pairing with this
       wait, which is good: we'll reliably continue waiting until the next
       yield() or enter_blocking_section() call (or we see a spurious condvar
       wakeup, which are rare at best.) */
       SleepConditionVariableSRW(&m->is_free, &m->lock,
                                 INFINITE, 0 /* exclusive */);
  } while (m->busy);

  m->busy = true;
  atomic_fetch_add(&m->waiters, -1);

  caml_acquire_domain_lock();

  ReleaseSRWLockExclusive(&m->lock);

  return;
}

/* Triggered events */

typedef struct st_event_struct {
  SRWLOCK lock;                 /* to protect contents */
  atomic_bool status;           /* false = not triggered, true = triggered */
  CONDITION_VARIABLE triggered; /* signaled when triggered */
} * st_event;


static int st_event_create(st_event * res)
{
  st_event e = caml_stat_alloc_noexc(sizeof(struct st_event_struct));
  if (e == NULL) return ENOMEM;
  InitializeSRWLock(&e->lock);
  InitializeConditionVariable(&e->triggered);
  e->status = false;
  *res = e;
  return 0;
}

static int st_event_destroy(st_event e)
{
  caml_stat_free(e);
  return 0;
}

static int st_event_trigger(st_event e)
{
  AcquireSRWLockExclusive(&e->lock);
  e->status = true;
  ReleaseSRWLockExclusive(&e->lock);
  WakeAllConditionVariable(&e->triggered);
  return 0;
}

static int st_event_wait(st_event e)
{
  AcquireSRWLockExclusive(&e->lock);
  while(!e->status) {
    BOOL rc = SleepConditionVariableSRW(&e->triggered, &e->lock,
                                        INFINITE, 0 /* exclusive */);
    if (!rc) {
      int err = caml_posixerr_of_win32err(GetLastError());
      return err == 0 ? EINVAL : err;
    }
  }
  ReleaseSRWLockExclusive(&e->lock);
  return 0;
}
