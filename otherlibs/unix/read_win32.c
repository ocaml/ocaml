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

#include <limits.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/bigarray.h>
#include "caml/unixsupport.h"

CAMLprim value caml_unix_read(value fd, value buf, value ofs, value vlen)
{
  CAMLparam1(buf);
  intnat len;
  DWORD numbytes, numread;
  char iobuf[UNIX_BUFFER_SIZE];
  DWORD err = 0;

  len = Long_val(vlen);
  numbytes = len > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : len;
  if (Descr_kind_val(fd) == KIND_SOCKET) {
    int ret;
    SOCKET s = Socket_val(fd);
    caml_enter_blocking_section();
    ret = recv(s, iobuf, numbytes, 0);
    if (ret == SOCKET_ERROR) err = WSAGetLastError();
    caml_leave_blocking_section();
    numread = ret;
  } else {
    HANDLE h = Handle_val(fd);
    caml_enter_blocking_section();
    if (! ReadFile(h, iobuf, numbytes, &numread, NULL))
      err = GetLastError();
    caml_leave_blocking_section();
  }
  if (err) {
    if (err == ERROR_BROKEN_PIPE) {
      // The write handle for an anonymous pipe has been closed. We match the
      // Unix behavior, and treat this as a zero-read instead of a Unix_error.
      err = 0;
      numread = 0;
    } else {
      caml_win32_maperr(err);
      caml_uerror("read", Nothing);
    }
  }
  memmove (&Byte(buf, Long_val(ofs)), iobuf, numread);
  CAMLreturn(Val_int(numread));
}

CAMLprim value caml_unix_read_bigarray(value fd, value vbuf,
                                       value vofs, value vlen)
{
  CAMLparam4(fd, vbuf, vofs, vlen);
  intnat ofs, len;
  void *buf;
  DWORD numread;
  DWORD err = 0;

  buf = Caml_ba_data_val(vbuf);
  ofs = Long_val(vofs);
  len = Long_val(vlen);
  if (Descr_kind_val(fd) == KIND_SOCKET) {
    int ret;
    SOCKET s = Socket_val(fd);
    if (len > INT_MAX) len = INT_MAX;
    caml_enter_blocking_section();
    ret = recv(s, (char *)buf + ofs, len, 0);
    if (ret == SOCKET_ERROR) err = WSAGetLastError();
    caml_leave_blocking_section();
    numread = ret;
  } else {
    HANDLE h = Handle_val(fd);
    if (len > 0xFFFFFFFF) len = 0xFFFFFFFF;
    caml_enter_blocking_section();
    if (! ReadFile(h, (char *)buf + ofs, len, &numread, NULL))
      err = GetLastError();
    caml_leave_blocking_section();
  }
  if (err) {
    if (err == ERROR_BROKEN_PIPE) {
      // The write handle for an anonymous pipe has been closed. We match the
      // Unix behavior, and treat this as a zero-read instead of a Unix_error.
      err = 0;
      numread = 0;
    } else {
      caml_win32_maperr(err);
      caml_uerror("read_bigarray", Nothing);
    }
  }
  CAMLreturn(Val_int(numread));
}
