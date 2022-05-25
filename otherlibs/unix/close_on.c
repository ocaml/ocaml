/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     */
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
#include "unixsupport.h"
#include <windows.h>

CAMLprim value win_set_close_on_exec(value fd)
{
  if (caml_win32_set_inherit(Handle_val(fd), FALSE) == -1)
    uerror("set_close_on_exec", Nothing);
  return Val_unit;
}

CAMLprim value win_clear_close_on_exec(value fd)
{
  if (caml_win32_set_inherit(Handle_val(fd), TRUE) == -1)
    uerror("clear_close_on_exec", Nothing);
  return Val_unit;
}
