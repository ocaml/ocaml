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
#include <signals.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

value unix_connect(socket, address)   /* ML */
     value socket, address;
{
  int retcode;
  get_sockaddr(address);
  enter_blocking_section();
  retcode = connect(Int_val(socket), &sock_addr.s_gen, sock_addr_len);
  leave_blocking_section();
  if (retcode == -1) uerror("connect", Nothing);
  return Val_unit;
}

#else

value unix_connect() { invalid_argument("connect not implemented"); }
  
#endif
