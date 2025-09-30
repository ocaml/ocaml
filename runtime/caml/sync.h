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

/* Operations on mutexes from the OCaml stdlib */

#ifndef CAML_SYNC_H
#define CAML_SYNC_H

#ifdef CAML_INTERNALS

#include "mlvalues.h"
#include "platform.h"

/* Mutator mutexes and condition variables.

   The mutexes defined in this file are intended for use by mutators
   that own the OCaml runtime -- [caml_mutex_*] can be used from
   C mutator code, and [caml_ml_*] from OCaml mutator code.

   They are lower-ranked than the domain mutex: if the [mutex_lock*]
   functions needs to block, they will release the domain lock to do
   so (if it is held). In this sense they are "non-blocking", they
   yield control to the backup thread or other OCaml threads.

   Conversely, it is safe to release the domain lock and perform
   other runtime effects within their critical section.

   When there is a failure, the [caml_mutex_*] functions abort with
   a fatal error while the [caml_ml_mutex_*] functions raise an
   OCaml exception.
 */

typedef caml_plat_mutex * sync_mutex;
typedef caml_plat_cond * sync_condvar;

#define Mutex_val(v) (* ((sync_mutex *) Data_custom_val(v)))
#define Condition_val(v) (* (sync_condvar *) Data_custom_val(v))

CAMLextern void caml_mutex_init(sync_mutex *mut);
CAMLextern void caml_mutex_reinit(sync_mutex *mut);
CAMLextern void caml_mutex_free(sync_mutex *mut);

CAMLextern void caml_mutex_lock_while_yielding_the_runtime_system(
  sync_mutex mut);
CAMLextern void caml_mutex_unlock(sync_mutex mut);

value caml_ml_mutex_lock(value wrapper);
value caml_ml_mutex_unlock(value wrapper);
value caml_ml_condition_broadcast(value wrapper);

#endif /* CAML_INTERNALS */

#endif /* CAML_SYNC_H */
