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
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

value unix_getpeername(value sock)          /* ML */
{
  int retcode;

  sock_addr_len = sizeof(sock_addr);
  retcode = getpeername(Int_val(sock), &sock_addr.s_gen, &sock_addr_len);
  if (retcode == -1) uerror("getpeername", Nothing);
  return alloc_sockaddr();
}

#else

value unix_getpeername(value sock)
{ invalid_argument("getpeername not implemented"); }
  
#endif
