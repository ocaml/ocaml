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
#include <alloc.h>
#include "unixsupport.h"
#include <fcntl.h>

#define SIZEBUF 1024

value unix_pipe(value unit)                /* ML */
{
  HANDLE readh, writeh;
  value readfd = Val_unit, writefd = Val_unit, res;

  if (! CreatePipe(&readh, &writeh, NULL, SIZEBUF)) {
    _dosmaperr(GetLastError());
    uerror("pipe", Nothing);
  }
  Begin_roots2(readfd, writefd)
    readfd = win_alloc_handle(readh);
    writefd = win_alloc_handle(writeh);
    res = alloc_tuple(2);
    Field(res, 0) = readfd;
    Field(res, 1) = writefd;
  End_roots();
  return res;
}
