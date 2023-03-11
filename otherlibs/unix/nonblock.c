/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2002 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/signals.h>
#include "unixsupport.h"

CAMLprim value caml_unix_set_nonblock(value socket)
{
  u_long non_block = 1;

  if (ioctlsocket(Socket_val(socket), FIONBIO, &non_block) != 0) {
    caml_win32_maperr(WSAGetLastError());
    caml_uerror("caml_unix_set_nonblock", Nothing);
  }
  Flags_fd_val(socket) = Flags_fd_val(socket) & ~FLAGS_FD_IS_BLOCKING;
  return Val_unit;
}

CAMLprim value caml_unix_clear_nonblock(value socket)
{
  u_long non_block = 0;

  if (ioctlsocket(Socket_val(socket), FIONBIO, &non_block) != 0) {
    caml_win32_maperr(WSAGetLastError());
    caml_uerror("caml_unix_clear_nonblock", Nothing);
  }
  Flags_fd_val(socket) = Flags_fd_val(socket) | FLAGS_FD_IS_BLOCKING;
  return Val_unit;
}
