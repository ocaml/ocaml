/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
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
  retcode = getsockname((SOCKET) Handle_val(sock),
                        &sock_addr.s_gen, &sock_addr_len);
  if (retcode == -1) uerror("getsockname", Nothing);
  return alloc_sockaddr();
}
