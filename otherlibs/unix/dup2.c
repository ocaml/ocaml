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
#include "unix.h"

#ifdef HAS_DUP2

value unix_dup2(fd1, fd2)        /* ML */
     value fd1, fd2;
{
  if (dup2(Int_val(fd1), Int_val(fd2)) == -1) uerror("dup2", Nothing);
  return Val_unit;
}

#else

static int do_dup2(fd1, fd2)
     int fd1, fd2;
{
  int fd;
  int res;

  fd = dup(fd1);
  if (fd == -1) return -1;
  if (fd == fd2) return 0;
  res = do_dup2(fd1, fd2);
  close(fd);
  return res;
}

value unix_dup2(fd1, fd2)        /* ML */
     value fd1, fd2;
{
  close(Int_val(fd2));
  if (do_dup2(Int_val(fd1), Int_val(fd2)) == -1) uerror("dup2", Nothing);
  return Val_unit;
}

#endif
