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

#ifdef HAS_SOCKETS

#include "socketaddr.h"

value unix_accept(sock)          /* ML */
     value sock;
{
  int retcode;
  value res;
  Push_roots(a,1);

  sock_addr_len = sizeof(sock_addr);
  enter_blocking_section();
  retcode = accept(Int_val(sock), &sock_addr.s_gen, &sock_addr_len);
  leave_blocking_section();
  if (retcode == -1) uerror("accept", Nothing);
  a[0] = alloc_sockaddr();
  res = alloc_tuple(2);
  Field(res, 0) = Val_int(retcode);
  Field(res, 1) = a[0];
  Pop_roots();
  return res;
}

#else

value unix_accept() { invalid_argument("accept not implemented"); }
  
#endif
