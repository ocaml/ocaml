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
  value st, res;
  Push_roots(r, 1);

  
  st = alloc(1, 0);
  Field(st, 0) = Val_int(status);
 
  r[0] = st;
  res = alloc_tuple(2);
  Field(res, 0) = Val_int(pid);
  Field(res, 1) = r[0];
  Pop_roots();
  return res;
}

value win_waitpid(flags, pid_req)
     value flags, pid_req;
{
  int status;
  if (_cwait(&status,Int_val(pid_req), 0/* ignored by win32 */) == -1)
	uerror("waitpid", Nothing);  
  return alloc_process_status(pid_req, status);
}
