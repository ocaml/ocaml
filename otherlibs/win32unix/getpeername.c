/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include "unixsupport.h"
#include "socketaddr.h"

CAMLprim value unix_getpeername(value sock)
{
  int retcode;
  union sock_addr_union addr;
  socklen_param_type addr_len;

  addr_len = sizeof(addr);
  retcode = getpeername(Socket_val(sock), &addr.s_gen, &addr_len);
  if (retcode == -1) {
    win32_maperr(WSAGetLastError());
    uerror("getpeername", Nothing);
  }
  return alloc_sockaddr(&addr, addr_len, -1);
}
