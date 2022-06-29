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

#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/io.h>
#include <caml/memory.h>
#include "unixsupport.h"
#include <fcntl.h>
#include <io.h>
#include <errno.h>

/* Check that the given file descriptor has "stream semantics" and
   can therefore be used as part of buffered I/O.  Things that
   don't have "stream semantics" include block devices and
   UDP (datagram) sockets.
   Returns 0 if OK, a Win32 error code if error. */

static DWORD check_stream_semantics(value handle)
{
  switch (Descr_kind_val(handle)) {
  case KIND_HANDLE:
    switch (GetFileType(Handle_val(handle)) & ~FILE_TYPE_REMOTE) {
    case FILE_TYPE_DISK: case FILE_TYPE_CHAR: case FILE_TYPE_PIPE:
      return 0;
    default: {
      DWORD err = GetLastError();
      return err == NO_ERROR ? ERROR_INVALID_ACCESS : err;
    }
    }
  case KIND_SOCKET: {
    int so_type;
    int so_type_len = sizeof(so_type);
    if (getsockopt(Socket_val(handle), SOL_SOCKET, SO_TYPE,
                   (void *) &so_type, &so_type_len) != 0)
      return WSAGetLastError();
    switch (so_type) {
    case SOCK_STREAM:
      return 0;
    default:
      return ERROR_INVALID_ACCESS;
    }
  }
  default:
    return ERROR_INVALID_ACCESS;
  }
}

#define CRT_field_val(v) (((struct filedescr *) Data_custom_val(v))->crt_fd)

int caml_win32_get_CRT_fd(value handle)
{
  int fd;
  SPIN_WAIT {
    fd = atomic_load(&CRT_field_val(handle));
    if (fd != GETTING_CRT_FD) return fd;
  }
}

int caml_win32_CRT_fd_of_filedescr(value handle)
{
  SPIN_WAIT {
    int fd = atomic_load(&CRT_field_val(handle));
    switch (fd) {
    case NO_CRT_FD:
      if (! atomic_compare_exchange_strong(&CRT_field_val(handle),
                                           &fd, GETTING_CRT_FD))
        break; /* try again */
      fd = _open_osfhandle((intptr_t) Handle_val(handle), O_BINARY);
      if (fd == -1) {
        atomic_store(&CRT_field_val(handle), NO_CRT_FD);
        caml_uerror("channel_of_descr", Nothing);
      }
      atomic_store(&CRT_field_val(handle), fd);
      return fd;
    case GETTING_CRT_FD:
      break; /* try again */
    default:
      return fd;
    }
  }
}

CAMLprim value caml_unix_inchannel_of_filedescr(value handle)
{
  CAMLparam1(handle);
  CAMLlocal1(vchan);
  struct channel * chan;
  DWORD err;

  err = check_stream_semantics(handle);
  if (err != 0) {
    caml_win32_maperr(err);
    caml_uerror("in_channel_of_descr", Nothing);
  }
  chan = caml_open_descriptor_in(caml_win32_CRT_fd_of_filedescr(handle));
  chan->flags |= CHANNEL_FLAG_MANAGED_BY_GC;
                 /* as in caml_ml_open_descriptor_in() */
  if (Descr_kind_val(handle) == KIND_SOCKET)
    chan->flags |= CHANNEL_FLAG_FROM_SOCKET;
  vchan = caml_alloc_channel(chan);
  CAMLreturn(vchan);
}

CAMLprim value caml_unix_outchannel_of_filedescr(value handle)
{
  CAMLparam1(handle);
  CAMLlocal1(vchan);
  int fd;
  struct channel * chan;
  DWORD err;

  err = check_stream_semantics(handle);
  if (err != 0) {
    caml_win32_maperr(err);
    caml_uerror("out_channel_of_descr", Nothing);
  }
  chan = caml_open_descriptor_out(caml_win32_CRT_fd_of_filedescr(handle));
  chan->flags |= CHANNEL_FLAG_MANAGED_BY_GC;
                 /* as in caml_ml_open_descriptor_out() */
  if (Descr_kind_val(handle) == KIND_SOCKET)
    chan->flags |= CHANNEL_FLAG_FROM_SOCKET;
  vchan = caml_alloc_channel(chan);
  CAMLreturn(vchan);
}

CAMLprim value caml_unix_filedescr_of_channel(value vchan)
{
  CAMLparam1(vchan);
  CAMLlocal1(fd);
  struct channel * chan;
  HANDLE h;

  chan = Channel(vchan);
  if (chan->fd == -1) caml_unix_error(EBADF, "descr_of_channel", Nothing);
  h = (HANDLE) _get_osfhandle(chan->fd);
  if (chan->flags & CHANNEL_FLAG_FROM_SOCKET)
    fd = caml_win32_alloc_socket((SOCKET) h);
  else
    fd = caml_win32_alloc_handle(h);
  CRT_field_val(fd) = chan->fd;
  CAMLreturn(fd);
}

CAMLprim value caml_unix_filedescr_of_fd(value vfd)
{
  int crt_fd = Int_val(vfd);
  /* PR#4750: do not use the _or_socket variant as it can cause performance
     degradation and this function is only used with the standard
     handles 0, 1, 2, which are not sockets. */
  value res = caml_win32_alloc_handle((HANDLE) _get_osfhandle(crt_fd));
  CRT_field_val(res) = crt_fd;
  return res;
}
