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
#include "unix.h"

#ifdef HAS_UTIME

#include <sys/types.h>
#include <utime.h>

value unix_utimes(path, atime, mtime) /* ML */
     value path, atime, mtime;
{
  struct utimbuf times, * t;
  times.actime = Int_val(atime);
  times.modtime = Int_val(mtime);
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

value unix_utimes(path, atime, mtime) /* ML */
     value path, atime, mtime;
{
  struct timeval tv[2], * t;
  tv[0].tv_sec = Int_val(atime);
  tv[0].tv_usec = 0;
  tv[1].tv_sec = Int_val(mtime);
  tv[1].tv_usec = 0;
  if (tv[0].tv_sec || tv[1].tv_sec)
    t = tv;
  else
    t = (struct timeval *) NULL;
  if (utimes(String_val(path),  t) == -1) uerror("utime", path);
  return Val_unit;
}

#else

value unix_utimes() { invalid_argument("utimes not implemented"); }

#endif
#endif
