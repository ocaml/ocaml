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
  SOCKET sconn = (SOCKET) Handle_val(sock);
  SOCKET snew;
  value fd = Val_unit, adr = Val_unit, res;
  int optionValue;

  /* Set sockets to synchronous mode  */
  optionValue = SO_SYNCHRONOUS_NONALERT;
  setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, 
             (char *)&optionValue, sizeof(optionValue));

  sock_addr_len = sizeof(sock_addr);
  enter_blocking_section();
  snew = accept(sconn, &sock_addr.s_gen, &sock_addr_len);
  leave_blocking_section();
  if (snew == INVALID_SOCKET) {
    _dosmaperr(WSAGetLastError());
    uerror("accept", Nothing);
  }
  Begin_roots2 (fd, adr)
    fd = win_alloc_handle((HANDLE) snew);
    adr = alloc_sockaddr();
    res = alloc_tuple(2);
    Field(res, 0) = fd;
    Field(res, 1) = adr;
  End_roots();
  return res;
}

