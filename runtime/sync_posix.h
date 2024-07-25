/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* POSIX thread implementation of the user facing Mutex and Condition */
/* To be included in runtime/sync.c */

#ifndef CAML_SYNC_POSIX_H
#define CAML_SYNC_POSIX_H

#include <errno.h>
#include <pthread.h>
#include <string.h>

#include "caml/sync.h"

typedef int sync_retcode;

/* Mutexes */

Caml_inline int sync_mutex_create(sync_mutex * res)
{
  int rc;
  pthread_mutexattr_t attr;
  sync_mutex m;

  rc = pthread_mutexattr_init(&attr);
  if (rc != 0) goto error1;
  rc = pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
  if (rc != 0) goto error2;
  m = caml_stat_alloc_noexc(sizeof(pthread_mutex_t));
  if (m == NULL) { rc = ENOMEM; goto error2; }
  rc = pthread_mutex_init(m, &attr);
  if (rc != 0) goto error3;
  pthread_mutexattr_destroy(&attr);
  *res = m;
  return 0;
error3:
  caml_stat_free(m);
error2:
  pthread_mutexattr_destroy(&attr);
error1:
  return rc;
}

Caml_inline int sync_mutex_destroy(sync_mutex m)
{
  int rc;
  rc = pthread_mutex_destroy(m);
  caml_stat_free(m);
  return rc;
}

Caml_inline int sync_mutex_lock(sync_mutex m)
{
  return pthread_mutex_lock(m);
}

#define MUTEX_PREVIOUSLY_UNLOCKED 0
#define MUTEX_ALREADY_LOCKED EBUSY

Caml_inline int sync_mutex_trylock(sync_mutex m)
{
  return pthread_mutex_trylock(m);
}

Caml_inline int sync_mutex_unlock(sync_mutex m)
{
  return pthread_mutex_unlock(m);
}

/* Condition variables */

Caml_inline int sync_condvar_create(sync_condvar * res)
{
  int rc;
  sync_condvar c = caml_stat_alloc_noexc(sizeof(pthread_cond_t));
  if (c == NULL) return ENOMEM;
  rc = pthread_cond_init(c, NULL);
  if (rc != 0) { caml_stat_free(c); return rc; }
  *res = c;
  return 0;
}

Caml_inline int sync_condvar_destroy(sync_condvar c)
{
  int rc;
  rc = pthread_cond_destroy(c);
  caml_stat_free(c);
  return rc;
}

Caml_inline int sync_condvar_signal(sync_condvar c)
{
 return pthread_cond_signal(c);
}

Caml_inline int sync_condvar_broadcast(sync_condvar c)
{
    return pthread_cond_broadcast(c);
}

Caml_inline int sync_condvar_wait(sync_condvar c, sync_mutex m)
{
  return pthread_cond_wait(c, m);
}

#endif /* CAML_SYNC_POSIX_H */
