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

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <time.h>

#include "unixsupport.h"

static double initial_time = 0; /* 0 means uninitialized */
/* Windows' epoch as a Unix timestamp in hundreds of ns */
static const uint64_t epoch_ft = 116444736000000000llu;
static DWORD initial_tickcount;

CAMLprim value unix_gettimeofday(value unit)
{
  DWORD tickcount = GetTickCount();
  uint64_t ft;
  struct tm tm;
  if (initial_time == 0 || tickcount < initial_tickcount) {
    initial_tickcount = tickcount;
    GetSystemTimeAsFileTime((FILETIME*)&ft);
    ft -= epoch_ft; /* shift to Epoch-relative time */
    initial_time = ft * 1e-7; /* ft is in 100ns */
    return copy_double((double) initial_time);
  } else {
    return copy_double((double) initial_time +
                       (double) (tickcount - initial_tickcount) * 1e-3);
  }
}
