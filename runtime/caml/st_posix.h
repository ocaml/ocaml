
/* POSIX thread implementation of the user facing Mutex and Condition */

#define CAML_INTERNALS

#include "alloc.h"
#include "custom.h"
#include "fail.h"
#include "memory.h"

#include <pthread.h>

typedef int st_retcode;

/* Mutexes */

typedef pthread_mutex_t * st_mutex;

Caml_inline int st_mutex_lock(st_mutex m)
{
  return pthread_mutex_lock(m);
}

#define MUTEX_PREVIOUSLY_UNLOCKED 0
#define MUTEX_ALREADY_LOCKED EBUSY

Caml_inline int st_mutex_trylock(st_mutex m)
{
  return pthread_mutex_trylock(m);
}

Caml_inline int st_mutex_unlock(st_mutex m)
{
  return pthread_mutex_unlock(m);
}

/* Condition variables */

typedef pthread_cond_t * st_condvar;

Caml_inline int st_condvar_signal(st_condvar c)
{
 return pthread_cond_signal(c);
}

Caml_inline int st_condvar_broadcast(st_condvar c)
{
    return pthread_cond_broadcast(c);
}

Caml_inline int st_condvar_wait(st_condvar c, st_mutex m)
{
  return pthread_cond_wait(c, m);
}

/* Reporting errors */

static void st_check_error(int retcode, char * msg)
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
  memmove (&Byte(str, 0), msg, msglen);
  memmove (&Byte(str, msglen), ": ", 2);
  memmove (&Byte(str, msglen + 2), err, errlen);
  caml_raise_sys_error(str);
}