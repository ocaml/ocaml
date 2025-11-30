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
#include "caml/unixsupport.h"
#include "caml/socketaddr.h"

CAMLprim value caml_unix_bind(value socket, value address)
{
  int ret;
  struct sockaddr_storage addr;
  socklen_t addr_len;

  caml_unix_get_sockaddr(address, &addr, &addr_len);
  ret = bind(Socket_val(socket), (struct sockaddr *) &addr, addr_len);
  if (ret == -1) {
    caml_win32_maperr(WSAGetLastError());
    caml_uerror("bind", Nothing);
  }
  return Val_unit;
}
