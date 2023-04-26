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

#define CAML_INTERNALS
#include <caml/winsupport.h>

#include "unixsupport.h"

double caml_unix_gettimeofday_unboxed(value unit)
{
  CAML_ULONGLONG_FILETIME utime;
  double tm;
  GetSystemTimeAsFileTime(&utime.ft);
  tm = utime.ul - CAML_NT_EPOCH_100ns_TICKS;
  return (tm * 1e-7);  /* tm is in 100ns */
}

CAMLprim value caml_unix_gettimeofday(value unit)
{
  return caml_copy_double(caml_unix_gettimeofday_unboxed(unit));
}
