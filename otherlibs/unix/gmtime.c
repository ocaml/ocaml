/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include "unixsupport.h"
#include <time.h>

static value alloc_tm(struct tm *tm)
{
  value res;
  res = alloc_tuple(9);
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

value unix_gmtime(value t)             /* ML */
{
  time_t clock;
  clock = Long_val(t);
  return alloc_tm(gmtime(&clock));
}

value unix_localtime(value t)          /* ML */
{
  time_t clock;
  clock = Long_val(t);
  return alloc_tm(localtime(&clock));
}

#ifdef HAS_MKTIME

value unix_mktime(value t)            /* ML */
{
  struct tm tm;
  time_t clock;
  value res;
  value tmval = Val_unit;

  Begin_root (tmval);
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
    tmval = alloc_tm(&tm);
    res = alloc_tuple(2);
    Field(res, 0) = Val_long(clock);
    Field(res, 1) = tmval;
  End_roots ();
  return res;
}

#else

value unix_mktime(value t) { invalid_argument("mktime not implemented"); }

#endif
