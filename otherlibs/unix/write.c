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
#include "unix.h"

value unix_write(fd, buf, ofs, len) /* ML */
     value fd, buf, ofs, len;
{
  int ret;
  buf = unix_freeze_buffer(buf);
  enter_blocking_section();
  ret = write(Int_val(fd), &Byte(buf, Long_val(ofs)), Int_val(len));
  leave_blocking_section();
  if (ret == -1) uerror("write", Nothing);
  return Val_int(ret);
}
