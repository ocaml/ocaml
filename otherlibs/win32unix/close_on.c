/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
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
    _dosmaperr(GetLastError());
    return -1;
  }
  Handle_val(fd) = newh;
  CloseHandle(oldh);
  return 0;
}

value win_set_close_on_exec(value fd)             /* ML */
{
  if (win_set_inherit(fd, FALSE) == -1) uerror("set_close_on_exec", Nothing);
  return Val_unit;
}

value win_clear_close_on_exec(value fd)             /* ML */
{
  if (win_set_inherit(fd, TRUE) == -1) uerror("clear_close_on_exec", Nothing);
  return Val_unit;
}
