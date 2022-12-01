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

#include <errno.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

CAMLprim value caml_unix_write(value fd, value buf, value vofs, value vlen)
{
  CAMLparam2(fd, buf);
  intnat ofs, len, written;
  DWORD numbytes, numwritten;
  char iobuf[UNIX_BUFFER_SIZE];
  DWORD err = 0;

  ofs = Long_val(vofs);
  len = Long_val(vlen);
  written = 0;
  while (len > 0) {
    numbytes = len > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : len;
    memmove (iobuf, &Byte(buf, ofs), numbytes);
    if (Descr_kind_val(fd) == KIND_SOCKET) {
      int ret;
      SOCKET s = Socket_val(fd);
      caml_enter_blocking_section();
      ret = send(s, iobuf, numbytes, 0);
      if (ret == SOCKET_ERROR) err = WSAGetLastError();
      caml_leave_blocking_section();
      if (ret == SOCKET_ERROR && err == WSAEWOULDBLOCK && written > 0) break;
      numwritten = ret;
    } else {
      HANDLE h = Handle_val(fd);
      caml_enter_blocking_section();
      if (! WriteFile(h, iobuf, numbytes, &numwritten, NULL))
        err = GetLastError();
      caml_leave_blocking_section();
    }
    if (err) {
      caml_win32_maperr(err);
      caml_uerror("write", Nothing);
    }
    written += numwritten;
    ofs += numwritten;
    len -= numwritten;
  }
  CAMLreturn(Val_long(written));
}

CAMLprim value caml_unix_single_write(value fd, value buf, value vofs,
                                      value vlen)
{
  CAMLparam1(buf);
  intnat ofs, len, written;
  DWORD numbytes, numwritten;
  char iobuf[UNIX_BUFFER_SIZE];
  DWORD err = 0;

  ofs = Long_val(vofs);
  len = Long_val(vlen);
  written = 0;
  if (len > 0) {
    numbytes = len > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : len;
    memmove (iobuf, &Byte(buf, ofs), numbytes);
    if (Descr_kind_val(fd) == KIND_SOCKET) {
      int ret;
      SOCKET s = Socket_val(fd);
      caml_enter_blocking_section();
      ret = send(s, iobuf, numbytes, 0);
      if (ret == SOCKET_ERROR) err = WSAGetLastError();
      caml_leave_blocking_section();
      numwritten = ret;
    } else {
      HANDLE h = Handle_val(fd);
      caml_enter_blocking_section();
      if (! WriteFile(h, iobuf, numbytes, &numwritten, NULL))
        err = GetLastError();
      caml_leave_blocking_section();
    }
    if (err) {
      caml_win32_maperr(err);
      caml_uerror("single_write", Nothing);
    }
    written = numwritten;
  }
  CAMLreturn(Val_long(written));
}
