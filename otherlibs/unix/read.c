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
#include <memory.h>
#include <signals.h>
#include "unixsupport.h"

value unix_read(fd, buf, ofs, len) /* ML */
     value fd, buf, ofs, len;
{
  long numbytes;
  int ret;
  char iobuf[UNIX_BUFFER_SIZE];
  Push_roots(r, 1);

  r[0] = buf;
  numbytes = Long_val(len);
  if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
  enter_blocking_section();
  ret = read(Int_val(fd), iobuf, (int) numbytes);
  leave_blocking_section();
  if (ret == -1) uerror("read", Nothing);
  bcopy(iobuf, &Byte(r[0], Long_val(ofs)), ret);
  Pop_roots();
  return Val_int(ret);
}
