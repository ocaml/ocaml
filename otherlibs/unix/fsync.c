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
#define fsync(fd) _commit(win_CRT_fd_of_filedescr(fd))
#else
#define fsync(fd) fsync(Int_val(fd))
#endif

CAMLprim value unix_fsync(value fd)
{
  int ret;
  caml_enter_blocking_section();
  ret = fsync(fd);
  caml_leave_blocking_section();
  if (ret == -1) uerror("fsync", Nothing);
  return Val_unit;
}
