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
#include <caml/memory.h>
#include <caml/fail.h>
#include "unixsupport.h"

#define _WIN32_LEAN_AND_MEAN
#include <winsock2.h>

CAMLprim value unix_dup(value cloexec, value fd)
{
  CAMLparam2(cloexec, fd);
  CAMLlocal1(newfd);

  switch (Descr_kind_val(fd)) {
  case KIND_HANDLE: {
    HANDLE newh, proc = GetCurrentProcess();
    if (! DuplicateHandle(proc, Handle_val(fd), proc, &newh,
                          0L,
                          ! unix_cloexec_p(cloexec),
                          DUPLICATE_SAME_ACCESS)) {
      win32_maperr(GetLastError());
      uerror("dup", Nothing);
    }
    newfd = win_alloc_handle(newh);
    CAMLreturn(newfd);
  }
  case KIND_SOCKET: {
    WSAPROTOCOL_INFO info;
    SOCKET newsock;

    if (SOCKET_ERROR == WSADuplicateSocket(Socket_val(fd),
                                           GetCurrentProcessId(),
                                           &info)) {
      win32_maperr(WSAGetLastError());
      uerror("dup", Nothing);
    }

    newsock = WSASocket(info.iAddressFamily, info.iSocketType, info.iProtocol,
                        &info, 0, WSA_FLAG_OVERLAPPED);
    if (INVALID_SOCKET == newsock) {
      win32_maperr(WSAGetLastError());
      uerror("dup", Nothing);
    }

    win_set_cloexec((HANDLE) newsock, cloexec);
    newfd = win_alloc_socket(newsock);
    CAMLreturn(newfd);
  }
  default:
    caml_invalid_argument("Invalid file descriptor type");
  }
}
