/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
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

CAMLprim value unix_times(value unit)
{
  value res;
  struct tms buffer;

  times(&buffer);
  res = alloc_small(4 * Double_wosize, Double_array_tag);
  Store_double_field(res, 0, (double) buffer.tms_utime / CLK_TCK);
  Store_double_field(res, 1, (double) buffer.tms_stime / CLK_TCK);
  Store_double_field(res, 2, (double) buffer.tms_cutime / CLK_TCK);
  Store_double_field(res, 3, (double) buffer.tms_cstime / CLK_TCK);
  return res;
}
