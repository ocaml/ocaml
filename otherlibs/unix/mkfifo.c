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

#ifdef HAS_MKFIFO

value unix_mkfifo(path, mode)
     value path;
     value mode;
{
  if (mkfifo(String_val(path), Int_val(mode)) == -1)
    uerror("mkfifo", path);
  return Val_unit;
}

#else

#include <sys/types.h>
#include <sys/stat.h>

#ifdef S_IFIFO

value unix_mkfifo(path, mode)
     value path;
     value mode;
{
  if (mknod(String_val(path), (Int_val(mode) & 07777) | S_IFIFO, 0) == -1)
    uerror("mkfifo", path);
  return Val_unit;
}

#else

value unix_mkfifo() { invalid_argument("mkfifo not implemented"); }

#endif
#endif
