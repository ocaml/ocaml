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
#define epoch_ft 116444736000000000;

CAMLprim value unix_gettimeofday(value unit)
{
  FILETIME ft;
  double tm;
  ULARGE_INTEGER li;
  GetSystemTimeAsFileTime(&ft);
  li.LowPart = ft.dwLowDateTime;
  li.HighPart = ft.dwHighDateTime;
  tm = li.QuadPart - epoch_ft; /* shift to Epoch-relative time */
  return caml_copy_double(tm * 1e-7);  /* tm is in 100ns */
}

CAMLprim value unix_gettimeofday_int(value unit)
{
  value res;
  FILETIME ft;
  uint64_t time;
  int64_t sec;
  int32_t usec;
  ULARGE_INTEGER li;
  GetSystemTimeAsFileTime(&ft);
  li.LowPart = ft.dwLowDateTime;
  li.HighPart = ft.dwHighDateTime;
  time = li.QuadPart - epoch_ft; /* shift to Epoch-relative time */
  sec = time * 1e-7; /* tm is in 100ns */
  usec = (time - sec) * 10;
  res = caml_alloc_small(2, 0);
  Field(res, 0) = caml_copy_int64(sec);
  Field(res, 1) = caml_copy_int32(usec);
  return res;
}
