/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unix.h"

value unix_read(fd, buf, ofs, len) /* ML */
     value fd, buf, ofs, len;
{
  int ret;
  enter_blocking_section();
  ret = read(Int_val(fd), &Byte(buf, Long_val(ofs)), Int_val(len));
  leave_blocking_section();
  if (ret == -1) uerror("read", Nothing);
  return Val_int(ret);
}
