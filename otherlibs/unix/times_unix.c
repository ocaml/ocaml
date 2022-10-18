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

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "unixsupport.h"
#include <time.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/resource.h>

CAMLprim value caml_unix_times(value unit)
{

  value res;
  struct rusage ru;

  res = caml_alloc_small(4 * Double_wosize, Double_array_tag);

  getrusage (RUSAGE_SELF, &ru);
  Store_double_field (res, 0, ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1e6);
  Store_double_field (res, 1, ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1e6);
  getrusage (RUSAGE_CHILDREN, &ru);
  Store_double_field (res, 2, ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1e6);
  Store_double_field (res, 3, ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1e6);
  return res;
}
