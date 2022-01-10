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

#define CAML_INTERNALS

#include "alloc.h"
#include "custom.h"
#include "fail.h"
#include "memory.h"

#include <pthread.h>

typedef int sync_retcode;

/* Mutexes */

typedef pthread_mutex_t * sync_mutex;

#define Mutex_val(v) (* ((sync_mutex *) Data_custom_val(v)))

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

typedef pthread_cond_t * sync_condvar;

#define Condition_val(v) (* (sync_condvar *) Data_custom_val(v))

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

/* Reporting errors */

Caml_inline void sync_check_error(int retcode, char * msg)
{
  char * err;
  int errlen, msglen;
  value str;

  if (retcode == 0) return;
  if (retcode == ENOMEM) caml_raise_out_of_memory();
  err = strerror(retcode);
  msglen = strlen(msg);
  errlen = strlen(err);
  str = caml_alloc_string(msglen + 2 + errlen);
  memcpy (&Byte(str, 0), msg, msglen);
  memcpy (&Byte(str, msglen), ": ", 2);
  memcpy (&Byte(str, msglen + 2), err, errlen);
  caml_raise_sys_error(str);
}
