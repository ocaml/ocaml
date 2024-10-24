/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                         Antonin Decimo, Tarides                        */
/*                                                                        */
/*   Copyright 2024 Tarides                                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Windows implementation of the user facing Mutex and Condition */
/* To be included in runtime/sync.c */

#ifndef CAML_SYNC_WIN32_H
#define CAML_SYNC_WIN32_H

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "caml/sync.h"
#include "caml/osdeps.h"

typedef int sync_retcode;

/* Mutexes */

Caml_inline int sync_mutex_create(sync_mutex * res)
{
  sync_mutex m = caml_stat_alloc_noexc(sizeof(caml_plat_mutex));
  if (m == NULL) return ENOMEM;
  InitializeSRWLock(&m->lock);
  m->owner_tid = 0;
  *res = m;
  return 0;
}

Caml_inline int sync_mutex_destroy(sync_mutex m)
{
  caml_stat_free(m);
  return 0;
}

Caml_inline int sync_mutex_lock(sync_mutex m)
{
  DWORD self_tid = GetCurrentThreadId();
  if (m->owner_tid != self_tid) {
    AcquireSRWLockExclusive(&m->lock);
    m->owner_tid = self_tid;
    return 0;
  } else {
    return EDEADLK;
  }
}

#define MUTEX_PREVIOUSLY_UNLOCKED 0
#define MUTEX_ALREADY_LOCKED EBUSY

Caml_inline int sync_mutex_trylock(sync_mutex m)
{
  if (TryAcquireSRWLockExclusive(&m->lock)) {
    m->owner_tid = GetCurrentThreadId();
    return MUTEX_PREVIOUSLY_UNLOCKED;
  } else {
    return MUTEX_ALREADY_LOCKED;
  }
}

Caml_inline int sync_mutex_unlock(sync_mutex m)
{
  DWORD self_tid = GetCurrentThreadId();
  if (m->owner_tid == self_tid) {
    m->owner_tid = 0;
    ReleaseSRWLockExclusive(&m->lock);
    return 0;
  } else {
    return EPERM;
  }
}

/* Condition variables */

Caml_inline int sync_condvar_create(sync_condvar * res)
{
  sync_condvar c = caml_stat_alloc_noexc(sizeof(CONDITION_VARIABLE));
  if (c == NULL) return ENOMEM;
  InitializeConditionVariable(c);
  *res = c;
  return 0;
}

Caml_inline int sync_condvar_destroy(sync_condvar c)
{
  caml_stat_free(c);
  return 0;
}

Caml_inline int sync_condvar_signal(sync_condvar c)
{
  WakeConditionVariable(c);
  return 0;
}

Caml_inline int sync_condvar_broadcast(sync_condvar c)
{
  WakeAllConditionVariable(c);
  return 0;
}

Caml_inline int sync_condvar_wait(sync_condvar c, sync_mutex m)
{
  DWORD self_tid = GetCurrentThreadId();
  int rc = 0;
  if (m->owner_tid == self_tid) {
    m->owner_tid = 0;
    if (SleepConditionVariableSRW(c, &m->lock, INFINITE,
                                  0 /* exclusive */)) {
      m->owner_tid = self_tid;
    } else {
      rc = caml_posixerr_of_win32err(GetLastError());
      /* Not clear if the thread owns the mutex or not, but there's a
       * fatal error anyway.  */
    }
  } else {
    rc = EPERM;
  }
  return rc;
}

#endif
