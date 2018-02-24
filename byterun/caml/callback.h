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

#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

CAMLpublic value caml_callback (value closure, value arg);
CAMLpublic value caml_callback2 (value closure, value arg1, value arg2);
CAMLpublic value caml_callback3 (value closure, value arg1, value arg2,
                                 value arg3);
CAMLpublic value caml_callbackN (value closure, int narg, value args[]);

CAMLpublic value caml_callback_exn (value closure, value arg);
CAMLpublic value caml_callback2_exn (value closure, value arg1, value arg2);
CAMLpublic value caml_callback3_exn (value closure, value arg1, value arg2,
                                     value arg3);
CAMLpublic value caml_callbackN_exn (value closure, int narg, value args[]);

#define Make_exception_result(v) ((v) | 2)
#define Is_exception_result(v) (((v) & 3) == 2)
#define Extract_exception(v) ((v) & ~3)

CAMLpublic value * caml_named_value (char const * name);
typedef void (*caml_named_action) (value*, char *);
CAMLpublic void caml_iterate_named_values(caml_named_action f);

CAMLpublic void caml_main (char_os ** argv);
CAMLpublic void caml_startup (char_os ** argv);
CAMLpublic value caml_startup_exn (char_os ** argv);
CAMLpublic void caml_startup_pooled (char_os ** argv);
CAMLpublic value caml_startup_pooled_exn (char_os ** argv);
CAMLpublic void caml_shutdown (void);

CAMLdata int caml_callback_depth;

#ifdef __cplusplus
}
#endif

#endif
