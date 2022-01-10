/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Callbacks from C to OCaml */

#ifndef CAML_CALLBACK_H
#define CAML_CALLBACK_H

#include "mlvalues.h"
#include "memory.h"

#ifdef __cplusplus
extern "C" {
#endif

void caml_init_callbacks (void);

CAMLextern value caml_callback (value closure, value arg);
CAMLextern value caml_callback2 (value closure, value arg1, value arg2);
CAMLextern value caml_callback3 (value closure, value arg1, value arg2,
                                 value arg3);
CAMLextern value caml_callbackN (value closure, int narg, value args[]);

CAMLextern value caml_callback_exn (value closure, value arg);
CAMLextern value caml_callback2_exn (value closure, value arg1, value arg2);
CAMLextern value caml_callback3_exn (value closure,
                                     value arg1, value arg2, value arg3);
CAMLextern value caml_callbackN_exn (value closure, int narg, value args[]);

CAMLextern const value * caml_named_value (char const * name);
typedef void (*caml_named_action) (const value*, char *);
CAMLextern void caml_iterate_named_values(caml_named_action f);

CAMLextern void caml_main (char_os ** argv);
CAMLextern void caml_startup (char_os ** argv);
CAMLextern value caml_startup_exn (char_os ** argv);
CAMLextern void caml_startup_pooled (char_os ** argv);
CAMLextern value caml_startup_pooled_exn (char_os ** argv);
CAMLextern void caml_shutdown (void);

#ifdef __cplusplus
}
#endif

#endif
