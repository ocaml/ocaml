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

#include <errno.h>
#include <string.h>
#include <mlvalues.h>
#include <memory.h>
#include <signals.h>
#include "unixsupport.h"

value unix_write(value fd, value buf, value vofs, value vlen) /* ML */
{
  long ofs, len, written;
  DWORD numbytes, numwritten;
  BOOL ret;
  char iobuf[UNIX_BUFFER_SIZE];
  HANDLE h = Handle_val(fd);

  Begin_root (buf);
    ofs = Long_val(vofs);
    len = Long_val(vlen);
    written = 0;
    while (len > 0) {
      numbytes = len > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : len;
      bcopy(&Byte(buf, ofs), iobuf, numbytes);
      enter_blocking_section();
      ret = WriteFile(h, iobuf, numbytes, &numwritten, NULL);
      leave_blocking_section();
      if (! ret) {
        _dosmaperr(GetLastError());
        uerror("write", Nothing);
      }
      written += numwritten;
      ofs += numwritten;
      len -= numwritten;
    }
  End_roots();
  return Val_long(written);
}
