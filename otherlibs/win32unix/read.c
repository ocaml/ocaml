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

#include <string.h>
#include <mlvalues.h>
#include <memory.h>
#include <signals.h>
#include "unixsupport.h"

value unix_read(value fd, value buf, value ofs, value len) /* ML */
{
  DWORD numbytes, numread;
  BOOL ret;
  char iobuf[UNIX_BUFFER_SIZE];
  HANDLE h = Handle_val(fd);

  Begin_root (buf);
    numbytes = Long_val(len);
    if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
    enter_blocking_section();
    ret = ReadFile(h, iobuf, numbytes, &numread, NULL);
    leave_blocking_section();
    if (! ret) {
      _dosmaperr(GetLastError());
      uerror("read", Nothing);
    }
    bcopy(iobuf, &Byte(buf, Long_val(ofs)), numread);
  End_roots();
  return Val_int(numread);
}
