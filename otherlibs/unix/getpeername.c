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

#ifdef HAS_SOCKETS

#include "socketaddr.h"

value unix_getpeername(sock)          /* ML */
     value sock;
{
  int retcode;

  sock_addr_len = sizeof(sock_addr);
  retcode = getpeername(Int_val(sock), &sock_addr.s_gen, &sock_addr_len);
  if (retcode == -1) uerror("getpeername", Nothing);
  return alloc_sockaddr();
}

#else

value unix_getpeername() { invalid_argument("getpeername not implemented"); }
  
#endif
