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

value unix_getsockname(sock)          /* ML */
     value sock;
{
  int retcode;

  sock_addr_len = sizeof(sock_addr);
  retcode = getsockname((SOCKET) _get_osfhandle(Int_val(sock)),
                        &sock_addr.s_gen, &sock_addr_len);
  if (retcode == -1) uerror("getsockname", Nothing);
  return alloc_sockaddr();
}
