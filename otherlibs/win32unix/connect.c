/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <signals.h>
#include "unixsupport.h"
#include "socketaddr.h"

CAMLprim value unix_connect(socket, address)
     value socket, address;
{
  SOCKET s = Socket_val(socket);
  int retcode;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  get_sockaddr(address, &addr, &addr_len);
  enter_blocking_section();
  retcode = connect(s, &addr.s_gen, addr_len);
  leave_blocking_section();
  if (retcode == -1) {
    win32_maperr(WSAGetLastError());
    uerror("connect", Nothing);
  }
  return Val_unit;
}
