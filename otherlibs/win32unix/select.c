/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <signals.h>
#include "unixsupport.h"
#include <winsock.h>

static void fdlist_to_fdset(value fdlist, fd_set *fdset)
{
  value l;
  FD_ZERO(fdset);
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    FD_SET((SOCKET) Handle_val(Field(l, 0)), fdset);
  }
}

static value fdset_to_fdlist(value fdlist, fd_set *fdset)
{
  value res = Val_int(0);
  Begin_roots2(fdlist, res)
    for (/*nothing*/; fdlist != Val_int(0); fdlist = Field(fdlist, 1)) {
      value s = Field(fdlist, 0);
      if (FD_ISSET((SOCKET) Handle_val(s), fdset)) {
	value newres = alloc_small(2, 0);
	Field(newres, 0) = s;
	Field(newres, 1) = res;
	res = newres;
      }
    }
  End_roots();
  return res;
}

value unix_select(value readfds, value writefds, value exceptfds, value timeout) /* ML */
{
  fd_set read, write, except;
  double tm;
  struct timeval tv;
  struct timeval * tvp;
  int retcode;
  value res;
  value read_list = Val_unit, write_list = Val_unit, except_list = Val_unit;

  Begin_roots3 (readfds, writefds, exceptfds)
  Begin_roots3 (read_list, write_list, except_list)
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
    enter_blocking_section();
    retcode = select(FD_SETSIZE, &read, &write, &except, tvp);
    leave_blocking_section();
    if (retcode == -1) unix_error(WSAGetLastError(), "select", Nothing);
    read_list = fdset_to_fdlist(readfds, &read);
    write_list = fdset_to_fdlist(writefds, &write);
    except_list = fdset_to_fdlist(exceptfds, &except);
    res = alloc_small(3, 0);
    Field(res, 0) = read_list;
    Field(res, 1) = write_list;
    Field(res, 2) = except_list;
  End_roots();
  End_roots();
  return res;
}
