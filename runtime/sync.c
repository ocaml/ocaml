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

#define CAML_INTERNALS

#include <pthread.h>
#include <signal.h>
#include <stdio.h>

#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/domain_state.h"
#include "caml/fail.h"
#include "caml/memory.h"

#include "caml/sync.h"
#include "caml/eventlog.h"

/* Mutex operations */

static int sync_mutex_create(sync_mutex * res)
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

static int sync_mutex_destroy(sync_mutex m)
{
  int rc;
  rc = pthread_mutex_destroy(m);
  caml_stat_free(m);
  return rc;
}

static void caml_mutex_finalize(value wrapper)
{
  sync_mutex_destroy(Mutex_val(wrapper));
}

static int caml_mutex_compare(value wrapper1, value wrapper2)
{
  sync_mutex mut1 = Mutex_val(wrapper1);
  sync_mutex mut2 = Mutex_val(wrapper2);
  return mut1 == mut2 ? 0 : mut1 < mut2 ? -1 : 1;
}

static intnat caml_mutex_hash(value wrapper)
{
  return (intnat) (Mutex_val(wrapper));
}

static const struct custom_operations caml_mutex_ops = {
  "_mutex",
  caml_mutex_finalize,
  caml_mutex_compare,
  caml_mutex_hash,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_ml_mutex_new(value unit)        /* ML */
{
  sync_mutex mut = NULL;
  value wrapper;

  sync_check_error(sync_mutex_create(&mut), "Mutex.create");
  wrapper = caml_alloc_custom(&caml_mutex_ops, sizeof(pthread_mutex_t *),
                              0, 1);
  Mutex_val(wrapper) = mut;
  return wrapper;
}

CAMLprim value caml_ml_mutex_lock(value wrapper)     /* ML */
{
  sync_retcode retcode;
  sync_mutex mut = Mutex_val(wrapper);

  /* PR#4351: first try to acquire mutex without releasing the master lock */
  if (sync_mutex_trylock(mut) == MUTEX_PREVIOUSLY_UNLOCKED) return Val_unit;
  /* If unsuccessful, block on mutex */
  Begin_root(wrapper)
    caml_enter_blocking_section();
    retcode = sync_mutex_lock(mut);
    caml_leave_blocking_section();
  End_roots();
  sync_check_error(retcode, "Mutex.lock");
  return Val_unit;
}

CAMLprim value caml_ml_mutex_unlock(value wrapper)           /* ML */
{
  sync_retcode retcode;
  sync_mutex mut = Mutex_val(wrapper);
  /* PR#4351: no need to release and reacquire master lock */
  retcode = sync_mutex_unlock(mut);
  sync_check_error(retcode, "Mutex.unlock");
  return Val_unit;
}

CAMLprim value caml_ml_mutex_try_lock(value wrapper)           /* ML */
{
  sync_mutex mut = Mutex_val(wrapper);
  sync_retcode retcode;
  retcode = sync_mutex_trylock(mut);
  if (retcode == MUTEX_ALREADY_LOCKED) return Val_false;
  sync_check_error(retcode, "Mutex.try_lock");
  return Val_true;
}


/* Conditions operations */

static int sync_condvar_create(sync_condvar * res)
{
  int rc;
  sync_condvar c = caml_stat_alloc_noexc(sizeof(pthread_cond_t));
  if (c == NULL) return ENOMEM;
  rc = pthread_cond_init(c, NULL);
  if (rc != 0) { caml_stat_free(c); return rc; }
  *res = c;
  return 0;
}

static int sync_condvar_destroy(sync_condvar c)
{
  int rc;
  rc = pthread_cond_destroy(c);
  caml_stat_free(c);
  return rc;
}

static void caml_condition_finalize(value wrapper)
{
  sync_condvar_destroy(Condition_val(wrapper));
}

static int caml_condition_compare(value wrapper1, value wrapper2)
{
  sync_condvar cond1 = Condition_val(wrapper1);
  sync_condvar cond2 = Condition_val(wrapper2);
  return cond1 == cond2 ? 0 : cond1 < cond2 ? -1 : 1;
}

static intnat caml_condition_hash(value wrapper)
{
  return (intnat) (Condition_val(wrapper));
}

static struct custom_operations caml_condition_ops = {
  "_condition",
  caml_condition_finalize,
  caml_condition_compare,
  caml_condition_hash,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_ml_condition_new(value unit)        /* ML */
{
  value wrapper;
  sync_condvar cond = NULL;

  sync_check_error(sync_condvar_create(&cond), "Condition.create");
  wrapper = caml_alloc_custom(&caml_condition_ops, sizeof(sync_condvar *),
                              0, 1);
  Condition_val(wrapper) = cond;
  return wrapper;
}

CAMLprim value caml_ml_condition_wait(value wcond, value wmut)     /* ML */
{
  sync_condvar cond = Condition_val(wcond);
  sync_mutex mut = Mutex_val(wmut);
  sync_retcode retcode;

  CAML_EV_BEGIN(EV_DOMAIN_CONDITION_WAIT);
  Begin_roots2(wcond, wmut)
    caml_enter_blocking_section();
    retcode = sync_condvar_wait(cond, mut);
    caml_leave_blocking_section();
  End_roots();
  sync_check_error(retcode, "Condition.wait");
  CAML_EV_END(EV_DOMAIN_CONDITION_WAIT);

  return Val_unit;
}

CAMLprim value caml_ml_condition_signal(value wrapper)           /* ML */
{
  sync_check_error(sync_condvar_signal(Condition_val(wrapper)),
                 "Condition.signal");
  return Val_unit;
}

CAMLprim value caml_ml_condition_broadcast(value wrapper)           /* ML */
{
  sync_check_error(sync_condvar_broadcast(Condition_val(wrapper)),
                 "Condition.broadcast");
  return Val_unit;
}
