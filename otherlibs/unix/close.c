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

value unix_close(fd)             /* ML */
     value fd;
{
  if (close(Int_val(fd)) == -1) uerror("close", Nothing);
  return Val_unit;
}

value unix_closeall(last_fd)     /* ML */
     value last_fd;
{
  int fd;
  for (fd = 3; fd <= Int_val(last_fd); fd++) close(fd);
  return Val_unit;
}

