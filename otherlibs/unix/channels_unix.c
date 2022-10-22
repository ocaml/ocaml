/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Paris                  */
/*                                                                        */
/*   Copyright 2017 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <caml/mlvalues.h>
#include <caml/io.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS
#include <sys/socket.h>
#include "socketaddr.h"
#endif

/* Check that the given file descriptor has "stream semantics" and
   can therefore be used as part of buffered I/O.  Things that
   don't have "stream semantics" include block devices and
   UDP (datagram) sockets.
   Returns 0 if OK, a nonzero error code if error. */

static int caml_unix_check_stream_semantics(int fd)
{
  struct stat buf;

  if (fstat(fd, &buf) == -1) return errno;
  switch (buf.st_mode & S_IFMT) {
  case S_IFREG: case S_IFCHR: case S_IFIFO:
    /* These have stream semantics */
    return 0;
#ifdef HAS_SOCKETS
  case S_IFSOCK: {
    int so_type;
    socklen_param_type so_type_len = sizeof(so_type);
    if (getsockopt(fd, SOL_SOCKET, SO_TYPE, &so_type, &so_type_len) == -1)
      return errno;
    switch (so_type) {
    case SOCK_STREAM:
      return 0;
    default:
      return EINVAL;
    }
    }
#endif
  default:
    /* All other file types are suspect: block devices, directories,
       symbolic links, whatnot. */
    return EINVAL;
  }
}

CAMLprim value caml_unix_inchannel_of_filedescr(value fd)
{
  int err;
  caml_enter_blocking_section();
  err = caml_unix_check_stream_semantics(Int_val(fd));
  caml_leave_blocking_section();
  if (err != 0) caml_unix_error(err, "in_channel_of_descr", Nothing);
  return caml_ml_open_descriptor_in(fd);
}

CAMLprim value caml_unix_outchannel_of_filedescr(value fd)
{
  int err;
  caml_enter_blocking_section();
  err = caml_unix_check_stream_semantics(Int_val(fd));
  caml_leave_blocking_section();
  if (err != 0) caml_unix_error(err, "out_channel_of_descr", Nothing);
  return caml_ml_open_descriptor_out(fd);
}
