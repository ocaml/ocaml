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
#include <time.h>

#include "unixsupport.h"

#ifdef HAS_MKTIME
static double initial_time = 0; /* 0 means uninitialized */
#else
static time_t initial_time = 0; /* 0 means uninitialized */
#endif
static DWORD initial_tickcount;

CAMLprim value unix_gettimeofday(value unit)
{
  DWORD tickcount = GetTickCount();
  SYSTEMTIME st;
  struct tm tm;
  if (initial_time == 0 || tickcount < initial_tickcount) {
    initial_tickcount = tickcount;
#ifdef HAS_MKTIME
    GetLocalTime(&st);
    tm.tm_sec = st.wSecond;
    tm.tm_min = st.wMinute;
    tm.tm_hour = st.wHour;
    tm.tm_mday = st.wDay;
    tm.tm_mon = st.wMonth - 1;
    tm.tm_year = st.wYear - 1900;
    tm.tm_wday = 0;
    tm.tm_yday = 0;
    tm.tm_isdst = -1;
    initial_time = ((double) mktime(&tm) + (double) st.wMilliseconds * 1e-3);
#else
    initial_time = time(NULL);
#endif
    return copy_double((double) initial_time);
  } else {
    return copy_double((double) initial_time +
                       (double) (tickcount - initial_tickcount) * 1e-3);
  }
}
