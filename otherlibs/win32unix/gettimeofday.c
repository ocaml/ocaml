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
#include <time.h>

#include "unixsupport.h"

static time_t initial_time = 0; /* 0 means uninitialized */
static DWORD initial_tickcount;

CAMLprim value unix_gettimeofday(value unit)
{
  DWORD tickcount = GetTickCount();
  if (initial_time == 0 || tickcount < initial_tickcount) {
    initial_tickcount = tickcount;
    initial_time = time(NULL);
    return copy_double((double) initial_time);
  } else {
    return copy_double((double) initial_time +
                       (double) (tickcount - initial_tickcount) * 1e-3);
  }
}
