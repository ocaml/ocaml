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
#include "unixsupport.h"
#include <time.h>
#include <errno.h>

static value alloc_tm(struct tm *tm)
{
  value res;
  res = caml_alloc_9(0,
    Val_int(tm->tm_sec),
    Val_int(tm->tm_min),
    Val_int(tm->tm_hour),
    Val_int(tm->tm_mday),
    Val_int(tm->tm_mon),
    Val_int(tm->tm_year),
    Val_int(tm->tm_wday),
    Val_int(tm->tm_yday),
    tm->tm_isdst ? Val_true : Val_false);
  return res;
}

CAMLprim value unix_gmtime(value t)
{
  time_t clock;
  struct tm * tm;
  clock = (time_t) Double_val(t);
  tm = gmtime(&clock);
  if (tm == NULL) unix_error(EINVAL, "gmtime", Nothing);
  return alloc_tm(tm);
}

CAMLprim value unix_localtime(value t)
{
  time_t clock;
  struct tm * tm;
  clock = (time_t) Double_val(t);
  tm = localtime(&clock);
  if (tm == NULL) unix_error(EINVAL, "localtime", Nothing);
  return alloc_tm(tm);
}

#ifdef HAS_MKTIME

CAMLprim value unix_mktime(value t)
{
  struct tm tm;
  time_t clock;
  value res;
  value tmval = Val_unit, clkval = Val_unit;

  Begin_roots2(tmval, clkval);
    tm.tm_sec = Int_field(t, 0);
    tm.tm_min = Int_field(t, 1);
    tm.tm_hour = Int_field(t, 2);
    tm.tm_mday = Int_field(t, 3);
    tm.tm_mon = Int_field(t, 4);
    tm.tm_year = Int_field(t, 5);
    tm.tm_wday = Int_field(t, 6);
    tm.tm_yday = Int_field(t, 7);
    tm.tm_isdst = -1; /* tm.tm_isdst = Bool_val(Field_imm(t, 8)); */
    clock = mktime(&tm);
    if (clock == (time_t) -1) unix_error(ERANGE, "mktime", Nothing);
    tmval = alloc_tm(&tm);
    clkval = copy_double((double) clock);
    res = caml_alloc_2(0, clkval, tmval);
  End_roots ();
  return res;
}

#else

CAMLprim value unix_mktime(value t)
{ invalid_argument("mktime not implemented"); }

#endif
