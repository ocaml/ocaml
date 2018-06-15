/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                         Nicolas Ojeda Bar, LexiFi                      */
/*                                                                        */
/*   Copyright 2018 Institut National de Recherche en Informatique et     */
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

CAMLprim value unix_fsync(value fd)
{
  HANDLE h = Handle_val(fd);
  DWORD ret;

  if (h == INVALID_HANDLE_VALUE) {
    unix_error(EBADF, "fsync", Nothing);
  }

  caml_enter_blocking_section();
  ret = FlushFileBuffers(h);
  caml_leave_blocking_section();

  if (ret == 0) {
    win32_maperr(GetLastError());
    uerror("fsync", Nothing);
  }

  return Val_unit;
}
