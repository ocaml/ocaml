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

int socket_domain_table[] = {
  PF_UNIX, PF_INET, PF_INET6
};

int socket_type_table[] = {
  SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET
};

CAMLprim value unix_socket(value cloexec, value domain, value type, value proto)
{
  SOCKET s;

  s = socket(socket_domain_table[Int_val(domain)],
                   socket_type_table[Int_val(type)],
                   Int_val(proto));
  if (s == INVALID_SOCKET) {
    win32_maperr(WSAGetLastError());
    uerror("socket", Nothing);
  }
  win_set_cloexec((HANDLE) s, cloexec);
  return win_alloc_socket(s);
}
