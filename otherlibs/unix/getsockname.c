/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

value unix_getsockname(value sock)          /* ML */
{
  int retcode;

  sock_addr_len = sizeof(sock_addr);
  retcode = getsockname(Int_val(sock), &sock_addr.s_gen, &sock_addr_len);
  if (retcode == -1) uerror("getsockname", Nothing);
  return alloc_sockaddr();
}

#else

value unix_getsockname(value sock)
{ invalid_argument("getsockname not implemented"); }
  
#endif
