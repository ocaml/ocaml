/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
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
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"
#include "socketaddr.h"

/* from dup_win32.c */
extern SOCKET caml_win32_duplicate_socket(BOOL inherit, SOCKET oldsock);

CAMLprim value caml_unix_accept(value cloexec, value sock)
{
  CAMLparam0();
  CAMLlocal2(fd, adr);
  SOCKET sconn = Socket_val(sock);
  SOCKET snew;
  value res;
  union sock_addr_union addr;
  socklen_param_type addr_len;
  DWORD err = 0, flags;
  BOOL inherit = ! caml_unix_cloexec_p(cloexec);

  addr_len = sizeof(addr);
  caml_enter_blocking_section();
  snew = accept(sconn, &addr.s_gen, &addr_len);
  if (snew == INVALID_SOCKET) err = WSAGetLastError ();
  caml_leave_blocking_section();
  if (snew == INVALID_SOCKET) {
    caml_win32_maperr(err);
    caml_uerror("accept", Nothing);
  }

  /* accept sockets should be non-inheritable (cloexec) by default
   * but let's check anyway */
  if (! GetHandleInformation((HANDLE) snew, &flags)) {
    caml_win32_maperr(GetLastError());
    caml_uerror("accept", Nothing);
  }
  /* duplicate the socket if the requested inheritance bit doesn't
   * match the socket */
  if (!!(flags & HANDLE_FLAG_INHERIT) != inherit) {
    SOCKET snew_dup = caml_win32_duplicate_socket(inherit, snew);
    if (snew_dup == INVALID_SOCKET) {
      caml_uerror("accept", Nothing);
    }
    closesocket(snew);
    snew = snew_dup;
  }

  fd = caml_win32_alloc_socket(snew);
  adr = caml_unix_alloc_sockaddr(&addr, addr_len, snew);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = fd;
  Field(res, 1) = adr;
  CAMLreturn(res);
}
