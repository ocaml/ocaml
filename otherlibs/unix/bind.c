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
  
value unix_bind(value socket, value address)      /* ML */
{
  int ret;
  get_sockaddr(address);
  ret = bind(Int_val(socket), &sock_addr.s_gen, sock_addr_len);
  if (ret == -1) uerror("bind", Nothing);
  return Val_unit;
}

#else

value unix_bind(value socket, value address)
{ invalid_argument("bind not implemented"); }
  
#endif
