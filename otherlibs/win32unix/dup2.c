/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_dup2(value fd1, value fd2)        /* ML */
{
  HANDLE oldh, newh;

  oldh = Handle_val(fd2);
  if (! DuplicateHandle(GetCurrentProcess(), Handle_val(fd1),
                        GetCurrentProcess(), &newh,
                        0L, TRUE, DUPLICATE_SAME_ACCESS)) {
    _dosmaperr(GetLastError());
    return -1;
  }
  Handle_val(fd2) = newh;
  CloseHandle(oldh);
  return Val_unit;
}
