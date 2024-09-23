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
#include "caml/unixsupport.h"
#include <errno.h>
#include <stdbool.h>

#ifdef HAS_SOCKETS

#include "caml/socketaddr.h"
#include <ws2tcpip.h>

extern const int caml_unix_socket_domain_table[]; /* from socket.c */
extern const int caml_unix_socket_type_table[]; /* from socket.c */

#ifdef HAS_SOCKETPAIR

#error "Windows has defined sockepair! win32unix should be updated."

#else

static int socketpair(int domain, int type, int protocol,
                      SOCKET socket_vector[2],
                      BOOL inherit, char *sun_path)
{
  /* POSIX states that in case of error, the contents of socket_vector
     shall be unmodified. */
  SOCKET listener = INVALID_SOCKET,
    server = INVALID_SOCKET,
    client = INVALID_SOCKET;

  bool defer_delete_file = false;
  int rc;

  listener = caml_win32_socket(domain, type, protocol, NULL, inherit);
  if (listener == INVALID_SOCKET)
    goto fail_wsa;

  union sock_addr_union addr = { 0 };
  socklen_param_type socklen = sizeof(addr.s_unix);
  addr.s_unix.sun_family = PF_UNIX;
  memcpy(addr.s_unix.sun_path, sun_path, strlen(sun_path));

  rc = bind(listener, (struct sockaddr *) &addr, socklen);
  if (rc == SOCKET_ERROR && WSAGetLastError() != WSAEADDRINUSE)
    goto fail_wsa;
  defer_delete_file = true;

  rc = listen(listener, 1);
  if (rc == SOCKET_ERROR)
    goto fail_wsa;

  client = caml_win32_socket(domain, type, protocol, NULL, inherit);
  if (client == INVALID_SOCKET)
    goto fail_wsa;

  rc = connect(client, (struct sockaddr *) &addr, socklen);
  if (rc == SOCKET_ERROR)
    goto fail_wsa;

  server = accept(listener, NULL, NULL);
  if (server == INVALID_SOCKET)
    goto fail_wsa;

  rc = closesocket(listener);
  listener = INVALID_SOCKET;
  if (rc == SOCKET_ERROR)
    goto fail_wsa;

  /* Socket file no longer needed */
  defer_delete_file = false;
  if (!DeleteFileA(addr.s_unix.sun_path)) {
    caml_win32_maperr(GetLastError());
    goto fail_sockets;
  }

  /* Check that the process that connected is this self process. */
  u_long peerid = 0UL;
  DWORD drc;
  rc = WSAIoctl(client, SIO_AF_UNIX_GETPEERPID,
                NULL, 0U,
                &peerid, sizeof(peerid), &drc /* Windows bug: always 0 */,
                NULL, NULL);
  if (rc == SOCKET_ERROR)
    goto fail_wsa;
  if (peerid != GetCurrentProcessId()) {
    errno = EACCES; /* no clear error code */
    goto fail_sockets;
  }

  socket_vector[0] = client;
  socket_vector[1] = server;
  return 0;

fail_wsa:
  caml_win32_maperr(WSAGetLastError());

fail_sockets:
  if(listener != INVALID_SOCKET)
    closesocket(listener);
  if(client != INVALID_SOCKET)
    closesocket(client);
  if(server != INVALID_SOCKET)
    closesocket(server);

  if (defer_delete_file)
    DeleteFileA(addr.s_unix.sun_path);

  return SOCKET_ERROR;
}

CAMLprim value caml_win32_socketpair(value vsun_path, value vcloexec,
                                     value vdomain, value vtype,
                                     value vprotocol)
{
  CAMLparam5(vsun_path, vcloexec, vdomain, vtype, vprotocol);
  CAMLlocal1(result);
  SOCKET sv[2];
  int domain = caml_unix_socket_domain_table[Int_val(vdomain)];
  int type = caml_unix_socket_type_table[Int_val(vtype)];
  int protocol = Int_val(vprotocol);
  BOOL inherit = ! caml_unix_cloexec_p(vcloexec);

  if (domain != PF_UNIX)
    caml_unix_error(EOPNOTSUPP, "socketpair", Nothing);

  caml_unix_check_path(vsun_path, "socketpair");
  if (caml_string_length(vsun_path) + 1 > UNIX_PATH_MAX)
    caml_unix_error(ENOENT, "socketpair", Nothing);
  char *sun_path = caml_stat_strdup(String_val(vsun_path));

  caml_enter_blocking_section();
  int rc = socketpair(domain, type, protocol, sv, inherit, sun_path);
  caml_leave_blocking_section();

  caml_stat_free(sun_path);
  if (rc == SOCKET_ERROR)
    caml_uerror("socketpair", Nothing);

  result = caml_alloc_tuple(2);
  Store_field(result, 0, caml_win32_alloc_socket(sv[0]));
  Store_field(result, 1, caml_win32_alloc_socket(sv[1]));
  CAMLreturn(result);
}

#endif  /* HAS_SOCKETPAIR */

#endif  /* HAS_SOCKETS */
