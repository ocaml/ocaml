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

#ifdef HAS_SELECT

#include <sys/types.h>
#include <sys/time.h>

#ifdef FD_ISSET
typedef fd_set file_descr_set;
#else
typedef int file_descr_set;
#define FD_SETSIZE (sizeof(int) * 8)
#define FD_SET(fd,fds) (*(fds) |= 1 << (fd))
#define FD_CLR(fd,fds) (*(fds) &= ~(1 << (fd)))
#define FD_ISSET(fd,fds) (*(fds) & (1 << (fd)))
#define FD_ZERO(fds) (*(fds) = 0)
#endif

static void fdlist_to_fdset(fdlist, fdset)
     value fdlist;
     file_descr_set * fdset;
{
  value l;
  FD_ZERO(fdset);
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    FD_SET(Int_val(Field(l, 0)), fdset);
  }
}

static value fdset_to_fdlist(fdset)
     file_descr_set * fdset;
{
  int i;
  Push_roots(roots, 1)
#define res roots[0]
  res = Val_int(0);
  for (i = FD_SETSIZE - 1; i >= 0; i--) {
    if (FD_ISSET(i, fdset)) {
      value newres = alloc(2, 0);
      Field(newres, 0) = Val_int(i);
      Field(newres, 1) = res;
      res = newres;
    }
  }
  Pop_roots();
  return res;
#undef res
}

value unix_select(readfds, writefds, exceptfds, timeout) /* ML */
     value readfds, writefds, exceptfds, timeout;
{
  file_descr_set read, write, except;
  double tm;
  struct timeval tv;
  struct timeval * tvp;
  int retcode;
  value res;
  Push_roots(roots, 3)
#define read_list roots[0]
#define write_list roots[1]
#define except_list roots[2]

  fdlist_to_fdset(readfds, &read);
  fdlist_to_fdset(writefds, &write);
  fdlist_to_fdset(exceptfds, &except);
  tm = Double_val(timeout);
  if (tm < 0.0)
    tvp = (struct timeval *) NULL;
  else {
    tv.tv_sec = (int) tm;
    tv.tv_usec = (int) (1e6 * (tm - (int) tm));
    tvp = &tv;
  }
  retcode = select(FD_SETSIZE, &read, &write, &except, tvp);
  if (retcode == -1) uerror("select", Nothing);
  read_list = fdset_to_fdlist(&read);
  write_list = fdset_to_fdlist(&write);
  except_list = fdset_to_fdlist(&except);
  res = alloc_tuple(3);
  Field(res, 0) = read_list;
  Field(res, 1) = write_list;
  Field(res, 2) = except_list;
  Pop_roots();
  return res;
#undef read_list
#undef write_list
#undef except_list
}

#else

value unix_select() { invalid_argument("select not implemented"); }

#endif
