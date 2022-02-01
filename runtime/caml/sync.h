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

/* Operations on mutexes and condition variables from the OCaml stdlib */

#ifndef CAML_SYNC_H
#define CAML_SYNC_H

#ifdef CAML_INTERNALS

#include "mlvalues.h"

/* Mutex.lock: Mutex.t -> unit */
CAMLextern value caml_ml_mutex_lock(value mut);

/* Mutex.unlock: Mutex.t -> unit */
CAMLextern value caml_ml_mutex_unlock(value mut);

/* Mutex.try_lock: Mutex.t -> bool */
CAMLextern value caml_ml_mutex_try_lock(value mut);

/* Condition.wait: Condition.t -> Mutex.t -> unit */
CAMLextern value caml_ml_condition_wait(value cond, value mut);

/* Condition.signal: Condition.t -> unit */
CAMLextern value caml_ml_condition_signal(value cond);

/* Condition.broadcast: Condition.t -> unit */
CAMLextern value caml_ml_condition_broadcast(value cond);

#endif /* CAML_INTERNALS */

#endif /* CAML_SYNC_H */
