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
#include <alloc.h>
#include <memory.h>
#include "unixsupport.h"
#include "socketaddr.h"

value unix_accept(sock)          /* ML */
     value sock;
{
  SOCKET s;
  value res;
  int fd;
  int optionValue;
  HANDLE h;
  Push_roots(a,1);


  /* Set sockets to synchronous mode  */
  optionValue = SO_SYNCHRONOUS_NONALERT;
  setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, 
             (char *)&optionValue, sizeof(optionValue));
  
  sock_addr_len = sizeof(sock_addr);
  enter_blocking_section();
  s = accept((SOCKET) _get_osfhandle(Int_val(sock)),
	     &sock_addr.s_gen, &sock_addr_len);
  leave_blocking_section();
  if (s == INVALID_SOCKET) {
    _dosmaperr(WSAGetLastError());
    uerror("accept", Nothing);
  };
  a[0] = alloc_sockaddr();
  res = alloc_tuple(2);
  fd = _open_osfhandle(s, 0);
  if (fd == -1) uerror("accept", Nothing);
  Field(res, 0) = Val_int(fd);
  Field(res, 1) = a[0];
  Pop_roots();
  return res;
}

