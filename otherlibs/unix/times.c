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
  value u = Val_unit, s = Val_unit, cu = Val_unit, cs = Val_unit;

  Begin_roots4 (u, s, cu, cs);
    times(&buffer);
    u = copy_double((double) buffer.tms_utime / CLK_TCK);
    s = copy_double((double) buffer.tms_stime / CLK_TCK);
    cu = copy_double((double) buffer.tms_cutime / CLK_TCK);
    cs = copy_double((double) buffer.tms_cstime / CLK_TCK);
    res = alloc_tuple(4);
    Field (res, 0) = u;
    Field (res, 1) = s;
    Field (res, 2) = cu;
    Field (res, 3) = cs;
  End_roots();
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
