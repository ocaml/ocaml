/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"
#include "socketaddr.h"

value unix_connect(socket, address)   /* ML */
     value socket, address;
{
  SOCKET s = (SOCKET) Handle_val(socket);
  int retcode;

  get_sockaddr(address);
  enter_blocking_section();
  retcode = connect(s, &sock_addr.s_gen, sock_addr_len);
  leave_blocking_section();
  if (retcode == -1) unix_error(WSAGetLastError(), "connect", Nothing);
  return Val_unit;
}
