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

#ifdef HAS_SOCKETS

#include "socketaddr.h"

value unix_accept(value sock)          /* ML */
{
  int retcode;
  value res;
  value a;

  sock_addr_len = sizeof(sock_addr);
  enter_blocking_section();
  retcode = accept(Int_val(sock), &sock_addr.s_gen, &sock_addr_len);
  leave_blocking_section();
  if (retcode == -1) uerror("accept", Nothing);
  a = alloc_sockaddr();
  Begin_root (a);
    res = alloc_small(2, 0);
    Field(res, 0) = Val_int(retcode);
    Field(res, 1) = a;
  End_roots();
  return res;
}

#else

value unix_accept(value sock) { invalid_argument("accept not implemented"); }
  
#endif
