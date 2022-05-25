/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Francois Berenger, Kyushu Institute of Technology          */
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

#ifdef _WIN32
#include <io.h>
#define fsync(fd) _commit(fd)
#else
#define fsync(fd) fsync(fd)
#endif

CAMLprim value caml_unix_fsync(value v)
{
  int ret;
#ifdef _WIN32
  int fd = caml_win32_CRT_fd_of_filedescr(v);
#else
  int fd = Int_val(v);
#endif
  caml_enter_blocking_section();
  ret = fsync(fd);
  caml_leave_blocking_section();
  if (ret == -1) caml_uerror("fsync", Nothing);
  return Val_unit;
}
