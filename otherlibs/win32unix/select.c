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
#include <signals.h>
#include "unixsupport.h"

static void fdlist_to_fdset(value fdlist, fd_set *fdset)
{
  value l;
  FD_ZERO(fdset);
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    FD_SET(Socket_val(Field(l, 0)), fdset);
  }
}

static value fdset_to_fdlist(value fdlist, fd_set *fdset)
{
  value res = Val_int(0);
  Begin_roots2(fdlist, res)
    for (/*nothing*/; fdlist != Val_int(0); fdlist = Field(fdlist, 1)) {
      value s = Field(fdlist, 0);
      if (FD_ISSET(Socket_val(s), fdset)) {
        value newres = alloc_small(2, 0);
        Field(newres, 0) = s;
        Field(newres, 1) = res;
        res = newres;
      }
    }
  End_roots();
  return res;
}

CAMLprim value unix_select(value readfds, value writefds, value exceptfds, value timeout)
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
    tm = Double_val(timeout);
    if (readfds == Val_int(0)
	&& writefds == Val_int(0)
	&& exceptfds == Val_int(0)) {
      if ( tm > 0.0 ) {
	enter_blocking_section();
	Sleep( (int)(tm * 1000));
	leave_blocking_section();
      }
      read_list = write_list = except_list = Val_int(0);
    } else {      
      fdlist_to_fdset(readfds, &read);
      fdlist_to_fdset(writefds, &write);
      fdlist_to_fdset(exceptfds, &except);
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
      if (retcode == -1) {
	win32_maperr(WSAGetLastError());
	uerror("select", Nothing);
      }
      read_list = fdset_to_fdlist(readfds, &read);
      write_list = fdset_to_fdlist(writefds, &write);
      except_list = fdset_to_fdlist(exceptfds, &except);
    }
    res = alloc_small(3, 0);
    Field(res, 0) = read_list;
    Field(res, 1) = write_list;
    Field(res, 2) = except_list;
  End_roots();
  End_roots();
  return res;
}
