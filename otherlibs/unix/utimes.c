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
#include "unixsupport.h"

#ifdef HAS_UTIME

#include <sys/types.h>
#ifndef _WIN32
#include <utime.h>
#else
#include <sys/utime.h>
#endif

value unix_utimes(value path, value atime, value mtime) /* ML */
{
  struct utimbuf times, * t;
  times.actime = Double_val(atime);
  times.modtime = Double_val(mtime);
  if (times.actime || times.modtime)
    t = &times;
  else
    t = (struct utimbuf *) NULL;
  if (utime(String_val(path),  t) == -1) uerror("utimes", path);
  return Val_unit;
}

#else

#ifdef HAS_UTIMES

#include <sys/types.h>
#include <sys/time.h>

value unix_utimes(value path, value atime, value mtime) /* ML */
{
  struct timeval tv[2], * t;
  double at = Double_val(atime);
  double mt = Double_val(mtime);
  tv[0].tv_sec = at;
  tv[0].tv_usec = (at - tv[0].tv_sec) * 1000000;
  tv[1].tv_sec = mt;
  tv[1].tv_usec = (mt - tv[1].tv_sec) * 1000000;
  if (tv[0].tv_sec || tv[1].tv_sec)
    t = tv;
  else
    t = (struct timeval *) NULL;
  if (utimes(String_val(path),  t) == -1) uerror("utimes", path);
  return Val_unit;
}

#else

value unix_utimes(value path, value atime, value mtime)
{ invalid_argument("utimes not implemented"); }

#endif
#endif
