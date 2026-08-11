/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2001 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_PRINTEXC_H
#define CAML_PRINTEXC_H

#include "misc.h"
#include "mlvalues.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Returns a string representation of the exception [exn], allocated with
   [caml_stat_alloc], or NULL if the allocation failed. It is the
   responsibility of the caller to free it with [caml_stat_free]. */
CAMLextern char * caml_format_exception (value exn);

/* Same as [caml_format_exception] for the exception carried by [result],
   or NULL if [result] is not an exception. */
Caml_inline char * caml_result_format_exception (caml_result result)
{
  if (! caml_result_is_exception(result)) return NULL;
  return caml_format_exception(result.data);
}

#ifdef __cplusplus
}
#endif

#ifdef CAML_INTERNALS
CAMLnoret void caml_fatal_uncaught_exception (value);
#endif /* CAML_INTERNALS */

#endif /* CAML_PRINTEXC_H */
