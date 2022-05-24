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

static HANDLE duplicate_handle(BOOL inherit, HANDLE oldh)
{
  HANDLE newh, proc = GetCurrentProcess();
  if (! DuplicateHandle(proc, oldh, proc, &newh,
                        0L,
                        inherit,
                        DUPLICATE_SAME_ACCESS)) {
    caml_win32_maperr(GetLastError());
    return INVALID_HANDLE_VALUE;
  }
  return newh;
}

static SOCKET duplicate_socket(BOOL inherit, SOCKET oldsock)
{
  WSAPROTOCOL_INFO info;

  if (SOCKET_ERROR == WSADuplicateSocket(oldsock,
                                         GetCurrentProcessId(),
                                         &info)) {
    caml_win32_maperr(WSAGetLastError());
    return INVALID_SOCKET;
  }

  return caml_win32_socket(info.iAddressFamily, info.iSocketType,
                           info.iProtocol, &info, inherit);
}

CAMLprim value unix_dup(value cloexec, value fd)
{
  CAMLparam2(cloexec, fd);
  CAMLlocal1(newfd);

  switch (Descr_kind_val(fd)) {
  case KIND_HANDLE: {
    HANDLE newh = duplicate_handle(! unix_cloexec_p(cloexec),
                                   Handle_val(fd));
    if (newh == INVALID_HANDLE_VALUE)
      uerror("dup", Nothing);
    newfd = win_alloc_handle(newh);
    CAMLreturn(newfd);
  }
  case KIND_SOCKET: {
    SOCKET newsock = duplicate_socket(! unix_cloexec_p(cloexec),
                                      Socket_val(fd));
    if (newsock == INVALID_SOCKET)
      uerror("dup", Nothing);
    newfd = win_alloc_socket(newsock);
    CAMLreturn(newfd);
  }
  default:
    caml_invalid_argument("Invalid file descriptor type");
  }
}

CAMLprim value unix_dup2(value cloexec, value fd1, value fd2)
{
  CAMLparam3(cloexec, fd1, fd2);

  if (Descr_kind_val(fd1) != Descr_kind_val(fd2))
    caml_invalid_argument("Expected either two file handles or two sockets");

  switch (Descr_kind_val(fd1)) {
  case KIND_HANDLE: {
    HANDLE oldh = Handle_val(fd2),
      newh = duplicate_handle(! unix_cloexec_p(cloexec),
                              Handle_val(fd1));
    if (newh == INVALID_HANDLE_VALUE)
      uerror("dup2", Nothing);
    Handle_val(fd2) = newh;
    CloseHandle(oldh);
    break;
  }
  case KIND_SOCKET: {
    SOCKET oldsock = Socket_val(fd2),
      newsock = duplicate_socket(! unix_cloexec_p(cloexec),
                                 Socket_val(fd1));
    if (newsock == INVALID_SOCKET)
      uerror("dup2", Nothing);
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
