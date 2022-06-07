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
#include "unixsupport.h"
#include <caml/io.h>

CAMLprim value caml_unix_close(value fd)
{
  if (Descr_kind_val(fd) == KIND_SOCKET) {
    if (closesocket(Socket_val(fd)) != 0) {
      caml_win32_maperr(WSAGetLastError());
      caml_uerror("close", Nothing);
    }
  } else {
    /* If we have an fd then closing it also closes
     * the underlying handle. Also, closing only
     * the handle and not the fd leads to fd leaks. */
    int crt_fd = caml_win32_get_CRT_fd(fd);
    if (crt_fd != NO_CRT_FD) {
      if (_close(crt_fd) != 0)
         caml_uerror("close", Nothing);
    } else {
      if (! CloseHandle(Handle_val(fd))) {
        caml_win32_maperr(GetLastError());
        caml_uerror("close", Nothing);
      }
    }
  }
  return Val_unit;
}
