/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <windows.h>
#include "unixsupport.h"

int win_set_inherit(value fd, BOOL inherit)
{
  HANDLE oldh, newh;

  oldh = Handle_val(fd);
  if (! DuplicateHandle(GetCurrentProcess(), oldh,
                        GetCurrentProcess(), &newh,
                        0L, inherit, DUPLICATE_SAME_ACCESS)) {
    win32_maperr(GetLastError());
    return -1;
  }
  Handle_val(fd) = newh;
  CloseHandle(oldh);
  return 0;
}

CAMLprim value win_set_close_on_exec(value fd)
{
  if (win_set_inherit(fd, FALSE) == -1) uerror("set_close_on_exec", Nothing);
  return Val_unit;
}

CAMLprim value win_clear_close_on_exec(value fd)
{
  if (win_set_inherit(fd, TRUE) == -1) uerror("clear_close_on_exec", Nothing);
  return Val_unit;
}
