/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*   Pascal Cuoq and Xavier Leroy, projet Cristal, INRIA Rocquencourt  */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <windows.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include "unixsupport.h"
#include <sys/types.h>

static value alloc_process_status(HANDLE pid, int status)
{
  value res, st;

  st = alloc(1, 0);
  Field(st, 0) = Val_int(status);
  Begin_root (st);
    res = alloc_small(2, 0);
    Field(res, 0) = Val_long((long) pid);
    Field(res, 1) = st;
  End_roots();
  return res;
}

value win_waitpid(value flags, value vpid_req) /* ML */
{
  int status;
  HANDLE pid_req = (HANDLE) Long_val(vpid_req);

  if (WaitForSingleObject(pid_req, INFINITE) != WAIT_FAILED
      && GetExitCodeProcess(pid_req, &status)) {
    return alloc_process_status(pid_req, status);
  } else {
    _dosmaperr(GetLastError());
    uerror("waitpid", Nothing);
  }
}
