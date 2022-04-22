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

#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/domain_state.h"
#include "caml/fail.h"
#include "caml/memory.h"

#include "caml/signals.h"
#include "caml/sync.h"
#include "caml/sys.h"
#include "caml/runtime_events.h"

/* System-dependent part */
#include "sync_posix.h"

/* Mutex operations */

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

CAMLexport int caml_mutex_lock(sync_mutex mut)
{
  return sync_mutex_lock(mut);
}

CAMLexport int caml_mutex_unlock(sync_mutex mut)
{
  return sync_mutex_unlock(mut);
}

CAMLprim value caml_ml_mutex_new(value unit)
{
  sync_mutex mut = NULL;
  value wrapper;

  sync_check_error(sync_mutex_create(&mut), "Mutex.create");
  wrapper = caml_alloc_custom(&caml_mutex_ops, sizeof(pthread_mutex_t *),
                              0, 1);
  Mutex_val(wrapper) = mut;
  return wrapper;
}

CAMLprim value caml_ml_mutex_lock(value wrapper)
{
  CAMLparam1(wrapper);
  sync_retcode retcode;
  sync_mutex mut = Mutex_val(wrapper);

  /* PR#4351: first try to acquire mutex without releasing the master lock */
  if (sync_mutex_trylock(mut) != MUTEX_PREVIOUSLY_UNLOCKED) {
    /* If unsuccessful, block on mutex */
    caml_enter_blocking_section();
    retcode = sync_mutex_lock(mut);
    caml_leave_blocking_section();
    sync_check_error(retcode, "Mutex.lock");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ml_mutex_unlock(value wrapper)
{
  sync_retcode retcode;
  sync_mutex mut = Mutex_val(wrapper);
  /* PR#4351: no need to release and reacquire master lock */
  retcode = sync_mutex_unlock(mut);
  sync_check_error(retcode, "Mutex.unlock");
  return Val_unit;
}

CAMLprim value caml_ml_mutex_try_lock(value wrapper)
{
  sync_mutex mut = Mutex_val(wrapper);
  sync_retcode retcode;
  retcode = sync_mutex_trylock(mut);
  if (retcode == MUTEX_ALREADY_LOCKED) return Val_false;
  sync_check_error(retcode, "Mutex.try_lock");
  return Val_true;
}

/* Condition variables operations */

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

CAMLprim value caml_ml_condition_new(value unit)
{
  value wrapper;
  sync_condvar cond = NULL;

  sync_check_error(sync_condvar_create(&cond), "Condition.create");
  wrapper = caml_alloc_custom(&caml_condition_ops, sizeof(sync_condvar *),
                              0, 1);
  Condition_val(wrapper) = cond;
  return wrapper;
}

CAMLprim value caml_ml_condition_wait(value wcond, value wmut)
{
  CAMLparam2(wcond, wmut);
  sync_condvar cond = Condition_val(wcond);
  sync_mutex mut = Mutex_val(wmut);
  sync_retcode retcode;

  CAML_EV_BEGIN(EV_DOMAIN_CONDITION_WAIT);
  caml_enter_blocking_section();
  retcode = sync_condvar_wait(cond, mut);
  caml_leave_blocking_section();
  sync_check_error(retcode, "Condition.wait");
  CAML_EV_END(EV_DOMAIN_CONDITION_WAIT);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_ml_condition_signal(value wrapper)
{
  sync_check_error(sync_condvar_signal(Condition_val(wrapper)),
                 "Condition.signal");
  return Val_unit;
}

CAMLprim value caml_ml_condition_broadcast(value wrapper)
{
  sync_check_error(sync_condvar_broadcast(Condition_val(wrapper)),
                 "Condition.broadcast");
  return Val_unit;
}
