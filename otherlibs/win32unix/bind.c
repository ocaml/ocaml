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
  union sock_addr_union addr;
  socklen_param_type addr_len;

  get_sockaddr(address, &addr, &addr_len);
  ret = bind((SOCKET) Handle_val(socket), &addr.s_gen, addr_len);
  if (ret == -1) unix_error(WSAGetLastError(), "bind", Nothing);
  return Val_unit;
}
