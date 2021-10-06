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

CAMLprim value unix_dup2(value cloexec, value fd1, value fd2)
{
  CAMLparam3(cloexec, fd1, fd2);

  if (Descr_kind_val(fd1) != Descr_kind_val(fd2))
    caml_invalid_argument("Expected either two file handles or two sockets");

  switch (Descr_kind_val(fd1)) {
  case KIND_HANDLE: {
    HANDLE oldh, newh, proc = GetCurrentProcess();
    oldh = Handle_val(fd2);
    if (! DuplicateHandle(proc, Handle_val(fd1), proc, &newh,
                          0L,
                          ! unix_cloexec_p(cloexec),
                          DUPLICATE_SAME_ACCESS)) {
      win32_maperr(GetLastError());
      uerror("dup2", Nothing);
    }
    Handle_val(fd2) = newh;
    CloseHandle(oldh);
    break;
  }

  case KIND_SOCKET: {
    WSAPROTOCOL_INFO info;
    SOCKET oldsock, newsock;

    oldsock = Socket_val(fd2);
    if (SOCKET_ERROR == WSADuplicateSocket(Socket_val(fd1),
                                           GetCurrentProcessId(),
                                           &info)) {
      win32_maperr(WSAGetLastError());
      uerror("dup2", Nothing);
    }

    newsock = WSASocket(info.iAddressFamily, info.iSocketType, info.iProtocol,
                        &info, 0, WSA_FLAG_OVERLAPPED);
    if (INVALID_SOCKET == newsock) {
      win32_maperr(WSAGetLastError());
      uerror("dup2", Nothing);
    }

    win_set_cloexec((HANDLE) newsock, cloexec);
    Socket_val(fd2) = newsock;
    closesocket(oldsock);
    break;
  }
  default:
    caml_invalid_argument("Invalid file descriptor type");
  }

  /* Reflect the dup2 on the CRT fds, if any */
  if (CRT_fd_val(fd1) != NO_CRT_FD || CRT_fd_val(fd2) != NO_CRT_FD)
    _dup2(win_CRT_fd_of_filedescr(fd1), win_CRT_fd_of_filedescr(fd2));
  CAMLreturn(Val_unit);
}
