/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"

extern value win_fd_handle(value);
extern int _dup2(int, int);

CAMLprim value unix_dup2(value fd1, value fd2)
{
  HANDLE oldh, newh;

  oldh = Handle_val(fd2);
  if (! DuplicateHandle(GetCurrentProcess(), Handle_val(fd1),
                        GetCurrentProcess(), &newh,
                        0L, TRUE, DUPLICATE_SAME_ACCESS)) {
    win32_maperr(GetLastError());
    return -1;
  }
  Handle_val(fd2) = newh;
  if (Descr_kind_val(fd2) == KIND_SOCKET)
    closesocket((SOCKET) oldh);
  else
    CloseHandle(oldh);
  Descr_kind_val(fd2) = Descr_kind_val(fd1);
  /* Reflect the dup2 on the CRT fds, if any */
  if (CRT_fd_val(fd1) != NO_CRT_FD || CRT_fd_val(fd2) != NO_CRT_FD)
    _dup2(Int_val(win_fd_handle(fd1)), Int_val(win_fd_handle(fd2)));
  return Val_unit;
}
