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
#include <caml/memory.h>
#include "unixsupport.h"

int socket_domain_table[] = {
  PF_UNIX, PF_INET, PF_INET6
};

int socket_type_table[] = {
  SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET
};

SOCKET win32_socket(int domain, int type, int protocol,
                    LPWSAPROTOCOL_INFO info,
                    BOOL inherit)
{
  SOCKET s;
  DWORD flags = WSA_FLAG_OVERLAPPED;

#ifndef WSA_FLAG_NO_HANDLE_INHERIT
#define WSA_FLAG_NO_HANDLE_INHERIT 0x80
#endif

  if (! inherit)
    flags |= WSA_FLAG_NO_HANDLE_INHERIT;

  s = WSASocket(domain, type, protocol, info, 0, flags);
  if (s == INVALID_SOCKET) {
    if (! inherit && WSAGetLastError() == WSAEINVAL) {
      /* WSASocket probably doesn't suport WSA_FLAG_NO_HANDLE_INHERIT,
       * retry without. */
      flags &= ~(DWORD)WSA_FLAG_NO_HANDLE_INHERIT;
      s = WSASocket(domain, type, protocol, info, 0, flags);
      if (s == INVALID_SOCKET)
        goto err;
      win_set_inherit((HANDLE) s, FALSE);
      return s;
    }
    goto err;
  }

  return s;

err:
  win32_maperr(WSAGetLastError());
  return INVALID_SOCKET;
}

CAMLprim value unix_socket(value cloexec, value domain, value type, value proto)
{
  CAMLparam4(cloexec, domain, type, proto);
  CAMLlocal1(v_socket);
  SOCKET s;
  s = win32_socket(socket_domain_table[Int_val(domain)],
                   socket_type_table[Int_val(type)],
                   Int_val(proto),
                   NULL,
                   ! unix_cloexec_p(cloexec));
  if (s == INVALID_SOCKET)
    uerror("socket", Nothing);
  v_socket = win_alloc_socket(s);
  CAMLreturn(v_socket);
}
