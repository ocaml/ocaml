/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                         Antonin Decimo, Tarides                        */
/*                                                                        */
/*   Copyright 2021 Tarides                                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/misc.h>
#include <caml/signals.h>
#include "unixsupport.h"
#include <errno.h>

#ifdef HAS_SOCKETS

#include "socketaddr.h"
#include <ws2tcpip.h>

extern int unix_socket_domain_table[]; /* from socket.c */
extern int unix_socket_type_table[]; /* from socket.c */

#ifdef HAS_SOCKETPAIR

#error "Windows has defined sockepair! win32unix should be updated."

#else

static int socketpair(int domain, int type, int protocol,
                      SOCKET socket_vector[2],
                      BOOL inherit)
{
  wchar_t dirname[MAX_PATH + 1], path[MAX_PATH + 1];
  union sock_addr_union addr;
  socklen_param_type socklen;

  /* POSIX states that in case of error, the contents of socket_vector
     shall be unmodified. */
  SOCKET listener = INVALID_SOCKET,
    server = INVALID_SOCKET,
    client = INVALID_SOCKET;

  fd_set writefds, exceptfds;
  u_long non_block, peerid = 0UL;

  DWORD drc;
  int rc;

  if (GetTempPath(MAX_PATH + 1, dirname) == 0) {
    caml_win32_maperr(GetLastError());
    goto fail;
  }

  if (GetTempFileName(dirname, L"osp", 0U, path) == 0) {
    caml_win32_maperr(GetLastError());
    goto fail;
  }

  addr.s_unix.sun_family = PF_UNIX;
  socklen = sizeof(addr.s_unix);

  /* sun_path needs to be set in UTF-8 */
  rc = WideCharToMultiByte(CP_UTF8, 0, path, -1, addr.s_unix.sun_path,
                           UNIX_PATH_MAX, NULL, NULL);
  if (rc == 0) {
    caml_win32_maperr(GetLastError());
    goto fail_path;
  }

  listener = caml_win32_socket(domain, type, protocol, NULL, inherit);
  if (listener == INVALID_SOCKET)
    goto fail_wsa;

  /* The documentation requires removing the file before binding the socket. */
  if (DeleteFile(path) == 0) {
    drc = GetLastError();
    if (drc != ERROR_FILE_NOT_FOUND) {
      caml_win32_maperr(drc);
      goto fail_sockets;
    }
  }

  rc = bind(listener, (struct sockaddr *) &addr, socklen);
  if (rc == SOCKET_ERROR)
    goto fail_wsa;

  rc = listen(listener, 1);
  if (rc == SOCKET_ERROR)
    goto fail_wsa;

  client = caml_win32_socket(domain, type, protocol, NULL, inherit);
  if (client == INVALID_SOCKET)
    goto fail_wsa;

  non_block = 1UL;
  if (ioctlsocket(client, FIONBIO, &non_block) == SOCKET_ERROR)
    goto fail_wsa;

  rc = connect(client, (struct sockaddr *) &addr, socklen);
  if (rc != SOCKET_ERROR || WSAGetLastError() != WSAEWOULDBLOCK)
    goto fail_wsa;

  server = accept(listener, NULL, NULL);
  if (server == INVALID_SOCKET)
    goto fail_wsa;

  rc = closesocket(listener);
  listener = INVALID_SOCKET;
  if (rc == SOCKET_ERROR)
    goto fail_wsa;

  FD_ZERO(&writefds);
  FD_SET(client, &writefds);
  FD_ZERO(&exceptfds);
  FD_SET(client, &exceptfds);

  rc = select(0 /* ignored */,
              NULL, &writefds, &exceptfds,
              NULL /* blocking */);
  if (rc == SOCKET_ERROR
      || FD_ISSET(client, &exceptfds)
      || !FD_ISSET(client, &writefds)) {
    /* We're not interested in the socket error status */
    goto fail_wsa;
  }

  non_block = 0UL;
  if (ioctlsocket(client, FIONBIO, &non_block) == SOCKET_ERROR)
    goto fail_wsa;

  if (DeleteFile(path) == 0) {
    caml_win32_maperr(GetLastError());
    goto fail_sockets;
  }

  rc = WSAIoctl(client, SIO_AF_UNIX_GETPEERPID,
                NULL, 0U,
                &peerid, sizeof(peerid), &drc /* Windows bug: always 0 */,
                NULL, NULL);
  if (rc == SOCKET_ERROR || peerid != GetCurrentProcessId())
    goto fail_wsa;

  socket_vector[0] = client;
  socket_vector[1] = server;
  return 0;

fail_wsa:
  caml_win32_maperr(WSAGetLastError());

fail_path:
  DeleteFile(path);

fail_sockets:
  if(listener != INVALID_SOCKET)
    closesocket(listener);
  if(client != INVALID_SOCKET)
    closesocket(client);
  if(server != INVALID_SOCKET)
    closesocket(server);

fail:
  return SOCKET_ERROR;
}

CAMLprim value unix_socketpair(value cloexec, value domain, value type,
                               value protocol)
{
  CAMLparam4(cloexec, domain, type, protocol);
  CAMLlocal1(result);
  SOCKET sv[2];
  int rc;

  caml_enter_blocking_section();
  rc = socketpair(unix_socket_domain_table[Int_val(domain)],
                  unix_socket_type_table[Int_val(type)],
                  Int_val(protocol),
                  sv,
                  ! unix_cloexec_p(cloexec));
  caml_leave_blocking_section();

  if (rc == SOCKET_ERROR)
    uerror("socketpair", Nothing);

  result = caml_alloc_tuple(2);
  Store_field(result, 0, win_alloc_socket(sv[0]));
  Store_field(result, 1, win_alloc_socket(sv[1]));
  CAMLreturn(result);
}

#endif  /* HAS_SOCKETPAIR */

#endif  /* HAS_SOCKETS */
