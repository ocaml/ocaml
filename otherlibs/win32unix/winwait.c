/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*              Pascal Cuoq, projet Cristal, INRIA Rocquencourt        */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <windows.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <process.h>
#include "unixsupport.h"
#include <sys/types.h>

static value alloc_process_status(pid, status)
     int pid, status;
{
  value res;
  value st = alloc(1, 0);

  Begin_root (st);
    Field(st, 0) = Val_int(status);
    res = alloc_tuple(2);
    Field(res, 0) = Val_int(pid);
    Field(res, 1) = st;
  End_roots();
  return res;
}

value win_waitpid(flags, vpid_req) /* ML */
     value flags, vpid_req;
{
  int status, pid_req;
  pid_req = Int_val(vpid_req);
  if (_cwait(&status, pid_req, 0/* ignored by win32 */) == -1)
    uerror("waitpid", Nothing);  
  return alloc_process_status(pid_req, status);
}
