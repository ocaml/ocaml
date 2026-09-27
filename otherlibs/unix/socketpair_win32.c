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
#include "misc_internals.h"
#include <errno.h>
#include <stdbool.h>

#ifdef HAS_SOCKETS

#include "caml/socketaddr.h"

extern const int caml_unix_socket_domain_table[]; /* from socket.c */
extern const int caml_unix_socket_type_table[]; /* from socket.c */

#ifdef HAS_SOCKETPAIR

#error "Windows has defined sockepair! win32unix should be updated."

#else

#define SOCKETPAIR_BIND_ATTEMPTS 8

/* from win32.c */
extern DWORD (WINAPI *caml_get_temp_path)(DWORD, LPWSTR);
extern INIT_ONCE caml_get_temp_path_init_once;
BOOL WINAPI caml_get_temp_path_init(PINIT_ONCE, PVOID, PVOID *);

/* Generate a unique path without creating a file first.
   This avoids a TOCTOU race between file creation and socket binding. */
static bool gen_sun_path(wchar_t path[MAX_PATH + 1],
                         struct sockaddr_un *addr)
{
  static atomic_ulong socketpair_id = 0;
  DWORD (WINAPI *get_temp_path)(DWORD, LPWSTR);
  wchar_t dirname[MAX_PATH + 1];
  int rc;

  InitOnceExecuteOnce(&caml_get_temp_path_init_once, caml_get_temp_path_init,
                      NULL, (PVOID *) &caml_get_temp_path);

  if(!caml_get_temp_path(countof(dirname), dirname)) {
    caml_win32_maperr(GetLastError());
    return false;
  }

  rc = swprintf(path, MAX_PATH + 1, L"%s\\ocaml_sp_%08lx_%08lx",
                dirname, GetCurrentProcessId(),
                atomic_fetch_add(&socketpair_id, 1));
  if (rc < 0) {
    errno = ENAMETOOLONG;
    return false;
  }

  /* sun_path needs to be set in UTF-8 */
  rc = WideCharToMultiByte(CP_UTF8, 0, path, -1, addr->sun_path,
                           UNIX_PATH_MAX, NULL, NULL);
  if (rc == 0) {
    caml_win32_maperr(GetLastError());
    return false;
  }

  return true;
}

static int socketpair(int domain, int type, int protocol,
                      SOCKET socket_vector[2],
                      BOOL inherit)
{
  wchar_t path[MAX_PATH + 1];
  struct sockaddr_un addr;
  socklen_t socklen;

  /* POSIX states that in case of error, the contents of socket_vector
     shall be unmodified. */
  SOCKET listener = INVALID_SOCKET,
    server = INVALID_SOCKET,
    client = INVALID_SOCKET;

  u_long peerid = 0UL;

  /* Whether the socket file at [path] was created by us, and should be
     removed on failure. */
  bool bound = false;
  DWORD drc;
  int rc;

  addr.sun_family = PF_UNIX;
  socklen = sizeof(addr);

  listener = caml_win32_socket(domain, type, protocol, NULL, inherit);
  if (listener == INVALID_SOCKET)
    goto fail_wsa;

  for (int attempts = SOCKETPAIR_BIND_ATTEMPTS; ; attempts--) {
    if (!gen_sun_path(path, &addr))
      goto fail_sockets;

    /* bind() will atomically create the socket file, or fail if a file
       with the same name exists. In the latter case, the file isn't
       ours: don't delete it, try another name. */
    rc = bind(listener, (struct sockaddr *) &addr, socklen);
    if (rc != SOCKET_ERROR)
      break;
    if (WSAGetLastError() != WSAEADDRINUSE || attempts <= 1)
      goto fail_wsa;
  }
  bound = true;

  rc = listen(listener, 1);
  if (rc == SOCKET_ERROR)
    goto fail_wsa;

  client = caml_win32_socket(domain, type, protocol, NULL, inherit);
  if (client == INVALID_SOCKET)
    goto fail_wsa;

  /* The connection is queued in the listener's backlog, so connect()
     doesn't block waiting for accept(). */
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
  bound = false;
  if (DeleteFile(path) == 0) {
    caml_win32_maperr(GetLastError());
    goto fail_sockets;
  }

  /* Check that the process that connected is this self process. The
     peer of the client is always the process owning the listener, that
     is, this process; the peer of the accepted socket is the process
     that connected to the listener, which may be another process that
     raced to connect to the socket file. */
  rc = WSAIoctl(server, SIO_AF_UNIX_GETPEERPID,
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

  if (bound)
    DeleteFile(path);

  return SOCKET_ERROR;
}

CAMLprim value caml_unix_socketpair(value vcloexec, value vdomain, value vtype,
                                    value vprotocol)
{
  CAMLparam4(vcloexec, vdomain, vtype, vprotocol);
  CAMLlocal1(result);
  SOCKET sv[2];
  int rc;
  int domain = caml_unix_socket_domain_table[Int_val(vdomain)];
  int type = caml_unix_socket_type_table[Int_val(vtype)];
  int protocol = Int_val(vprotocol);
  BOOL inherit = ! caml_unix_cloexec_p(vcloexec);

  /* Only PF_UNIX sockets can be bound to a path. */
  if (domain != PF_UNIX) {
    caml_win32_maperr(WSAEAFNOSUPPORT);
    caml_uerror("socketpair", Nothing);
  }

  caml_enter_blocking_section();
  rc = socketpair(domain, type, protocol, sv, inherit);
  caml_leave_blocking_section();

  if (rc == SOCKET_ERROR)
    caml_uerror("socketpair", Nothing);

  result = caml_alloc_tuple(2);
  Store_field(result, 0, caml_win32_alloc_socket(sv[0]));
  Store_field(result, 1, caml_win32_alloc_socket(sv[1]));
  CAMLreturn(result);
}

#endif  /* HAS_SOCKETPAIR */

#endif  /* HAS_SOCKETS */
