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
#include <sys/types.h>
#include <sys/times.h>

#ifndef CLK_TCK
#ifdef HZ
#define CLK_TCK HZ
#else
#define CLK_TCK 60
#endif
#endif

value unix_times_bytecode()               /* ML */
{
  value res;
  struct tms buffer;
  int i;
  Push_roots(t,4);

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

value unix_times_native()               /* ML */
{
  value res;
  struct tms buffer;

  times(&buffer);
  res = alloc(4 * Double_wosize, Double_array_tag);
  Store_double_field(res, 0, (double) buffer.tms_utime / CLK_TCK);
  Store_double_field(res, 1, (double) buffer.tms_stime / CLK_TCK);
  Store_double_field(res, 2, (double) buffer.tms_cutime / CLK_TCK);
  Store_double_field(res, 3, (double) buffer.tms_cstime / CLK_TCK);
  return res;
}
