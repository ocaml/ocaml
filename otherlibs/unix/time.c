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

#include <time.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "unixsupport.h"

double caml_unix_time_unboxed(value unit)
{
  return ((double) time((time_t *) NULL));
}

CAMLprim value caml_unix_time(value unit)
{
  return caml_copy_double(caml_unix_time_unboxed(unit));
}
