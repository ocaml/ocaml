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
#include <time.h>

#include "unixsupport.h"

/* Unix epoch as a Windows timestamp in hundreds of ns */
#define epoch_ft 116444736000000000.0;

double caml_unix_gettimeofday_unboxed(value unit)
{
  FILETIME ft;
  double tm;
  GetSystemTimeAsFileTime(&ft);
  tm = *(uint64_t *)&ft - epoch_ft; /* shift to Epoch-relative time */
  return (tm * 1e-7);  /* tm is in 100ns */
}

CAMLprim value caml_unix_gettimeofday(value unit)
{
  return caml_copy_double(caml_unix_gettimeofday_unboxed(unit));
}
