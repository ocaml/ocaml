/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <mlvalues.h>
#include <alloc.h>
#include <fail.h>
#include <memory.h>
#include "unixsupport.h"
#include <time.h>
#include <errno.h>

static value alloc_tm(struct tm *tm)
{
  value res;
  res = alloc_small(9, 0);
  Init_field(res, 0, Val_int(tm->tm_sec));
  Init_field(res, 1, Val_int(tm->tm_min));
  Init_field(res, 2, Val_int(tm->tm_hour));
  Init_field(res, 3, Val_int(tm->tm_mday));
  Init_field(res, 4, Val_int(tm->tm_mon));
  Init_field(res, 5, Val_int(tm->tm_year));
  Init_field(res, 6, Val_int(tm->tm_wday));
  Init_field(res, 7, Val_int(tm->tm_yday));
  Init_field(res, 8, tm->tm_isdst ? Val_true : Val_false);
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
    tm.tm_sec = Int_val(Field(t, 0));
    tm.tm_min = Int_val(Field(t, 1));
    tm.tm_hour = Int_val(Field(t, 2));
    tm.tm_mday = Int_val(Field(t, 3));
    tm.tm_mon = Int_val(Field(t, 4));
    tm.tm_year = Int_val(Field(t, 5));
    tm.tm_wday = Int_val(Field(t, 6));
    tm.tm_yday = Int_val(Field(t, 7));
    tm.tm_isdst = -1; /* tm.tm_isdst = Bool_val(Field(t, 8)); */
    clock = mktime(&tm);
    if (clock == (time_t) -1) unix_error(ERANGE, "mktime", Nothing);
    tmval = alloc_tm(&tm);
    clkval = copy_double((double) clock);
    res = alloc_small(2, 0);
    Init_field(res, 0, clkval);
    Init_field(res, 1, tmval);
  End_roots ();
  return res;
}

#else

CAMLprim value unix_mktime(value t)
{ invalid_argument("mktime not implemented"); }

#endif
