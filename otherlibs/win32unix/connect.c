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
	int retcode;

  get_sockaddr(address);
  enter_blocking_section();
  retcode = connect((SOCKET)_get_osfhandle(Int_val(socket)), 
                    &sock_addr.s_gen, sock_addr_len);
  leave_blocking_section();
  if (retcode == -1) {
    _dosmaperr(WSAGetLastError());
    uerror("connect", Nothing);
  }
  return Val_unit;
}
