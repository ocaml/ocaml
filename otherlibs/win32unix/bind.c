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
#include "socketaddr.h"
  
value unix_bind(socket, address)      /* ML */
     value socket, address;
{
  int ret;
  get_sockaddr(address);
  ret = bind((SOCKET) Handle_val(socket), &sock_addr.s_gen, sock_addr_len);
  if (ret == -1) unix_error(WSAGetLastError(), "bind", Nothing);
  return Val_unit;
}
