/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include "unix.h"
#include <time.h>
#include <sys/types.h>
#include <sys/times.h>

#ifndef CLK_TCK
#ifdef HZ
#define CLK_TCK HZ
#else
#define CLK_TCK 60
#endif
#endif

value unix_times()               /* ML */
{
  value res;
  struct tms buffer;
  int i;
  Push_roots(t,4);

#ifndef HZ
#define HZ 60
#endif

  times(&buffer);
  t[0] = copy_double((double) buffer.tms_utime / CLK_TCK);
  t[1] = copy_double((double) buffer.tms_stime / CLK_TCK);
  t[2] = copy_double((double) buffer.tms_cutime / CLK_TCK);
  t[3] = copy_double((double) buffer.tms_cstime / CLK_TCK);
  res = alloc_tuple(4);
  for (i = 0; i < 4; i++)
    Field(res, i) = t[i];
  Pop_roots();
  return res;
}
