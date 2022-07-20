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

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include "unixsupport.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <fcntl.h>

#ifndef O_NONBLOCK
#define O_NONBLOCK O_NDELAY
#endif

CAMLprim value caml_unix_set_nonblock(value fd)
{
  int retcode;
  retcode = fcntl(Int_val(fd), F_GETFL, 0);
  if (retcode == -1 ||
      fcntl(Int_val(fd), F_SETFL, retcode | O_NONBLOCK) == -1)
    caml_uerror("set_nonblock", Nothing);
  return Val_unit;
}

CAMLprim value caml_unix_clear_nonblock(value fd)
{
  int retcode;
  retcode = fcntl(Int_val(fd), F_GETFL, 0);
  if (retcode == -1 ||
      fcntl(Int_val(fd), F_SETFL, retcode & ~O_NONBLOCK) == -1)
    caml_uerror("clear_nonblock", Nothing);
  return Val_unit;
}

CAMLprim value caml_unix_set_close_on_exec(value fd)
{
  caml_unix_set_cloexec(Int_val(fd), "set_close_on_exec", Nothing);
  return Val_unit;
}

CAMLprim value caml_unix_clear_close_on_exec(value fd)
{
  caml_unix_clear_cloexec(Int_val(fd), "set_close_on_exec", Nothing);
  return Val_unit;
}
