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

CAMLprim value unix_accept(value cloexec, value sock)
{
  CAMLparam0();
  CAMLlocal2(fd, adr);
  SOCKET sconn = Socket_val(sock);
  SOCKET snew;
  value res;
  union sock_addr_union addr;
  socklen_param_type addr_len;
  DWORD err = 0;

  addr_len = sizeof(addr);
  caml_enter_blocking_section();
  snew = accept(sconn, &addr.s_gen, &addr_len);
  if (snew == INVALID_SOCKET) err = WSAGetLastError ();
  caml_leave_blocking_section();
  if (snew == INVALID_SOCKET) {
    caml_win32_maperr(err);
    uerror("accept", Nothing);
  }
  caml_win32_set_cloexec((HANDLE) snew, cloexec);
  fd = caml_win32_alloc_socket(snew);
  adr = unix_alloc_sockaddr(&addr, addr_len, snew);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = fd;
  Field(res, 1) = adr;
  CAMLreturn(res);
}
