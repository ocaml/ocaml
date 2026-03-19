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
#include <caml/fail.h>
#include <caml/memory.h>
#include "caml/unixsupport.h"
#include <time.h>
#include <errno.h>

static value alloc_tm(const struct tm *tm)
{
  value res;
  res = caml_alloc_small(9, 0);
  Field(res,0) = Val_int(tm->tm_sec);
  Field(res,1) = Val_int(tm->tm_min);
  Field(res,2) = Val_int(tm->tm_hour);
  Field(res,3) = Val_int(tm->tm_mday);
  Field(res,4) = Val_int(tm->tm_mon);
  Field(res,5) = Val_int(tm->tm_year);
  Field(res,6) = Val_int(tm->tm_wday);
  Field(res,7) = Val_int(tm->tm_yday);
  Field(res,8) = tm->tm_isdst ? Val_true : Val_false;
  return res;
}

CAMLprim value caml_unix_gmtime(value t)
{
  time_t clock = (time_t) Double_val(t);
  struct tm * tm;
#ifdef HAVE_GMTIME_R
  struct tm result;
  tm = gmtime_r(&clock, &result);
#else
  tm = gmtime(&clock);
#endif
  if (tm == NULL) caml_uerror("gmtime", Nothing);
  return alloc_tm(tm);
}

CAMLprim value caml_unix_localtime(value t)
{
  time_t clock = (time_t) Double_val(t);
  struct tm * tm;
#ifdef HAVE_LOCALTIME_R
  struct tm result;
  tm = localtime_r(&clock, &result);
#else
  tm = localtime(&clock);
#endif
  if (tm == NULL) caml_uerror("localtime", Nothing);
  return alloc_tm(tm);
}

#ifdef HAS_MKTIME

CAMLprim value caml_unix_mktime(value t)
{
  CAMLparam0();
  CAMLlocal2(tmval, clkval);
  struct tm tm;
  time_t clock;
  value res;

  tm.tm_sec = Int_val(Field(t, 0));
  tm.tm_min = Int_val(Field(t, 1));
  tm.tm_hour = Int_val(Field(t, 2));
  tm.tm_mday = Int_val(Field(t, 3));
  tm.tm_mon = Int_val(Field(t, 4));
  tm.tm_year = Int_val(Field(t, 5));
  tm.tm_isdst = -1;
  tm.tm_wday = -1;
  clock = mktime(&tm);
  if (clock == (time_t) -1 && tm.tm_wday == -1)
    caml_uerror("mktime", Nothing);
  tmval = alloc_tm(&tm);
  clkval = caml_copy_double((double) clock);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = clkval;
  Field(res, 1) = tmval;
  CAMLreturn(res);
}

#else

CAMLprim value caml_unix_mktime(value t)
{ caml_invalid_argument("mktime not implemented"); }

#endif
