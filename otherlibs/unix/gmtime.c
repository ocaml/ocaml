#include <mlvalues.h>
#include <alloc.h>
#include "unix.h"
#include <time.h>

static value alloc_tm(tm)
     struct tm * tm;
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

value unix_gmtime(t)             /* ML */
     value t;
{
  int clock;
  clock = Int_val(t);
  return alloc_tm(gmtime(&clock));
}

value unix_localtime(t)          /* ML */
     value t;
{
  int clock;
  clock = Int_val(t);
  return alloc_tm(localtime(&clock));
}
