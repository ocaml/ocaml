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
#include <alloc.h>
#include "unixsupport.h"
#include <fcntl.h>

value win_fd_handle(value handle) /* ML */
{
  int fd = _open_osfhandle((long) Handle_val(handle), O_BINARY);
  if (fd == -1) uerror("channel_of_descr", Nothing);
  return Val_int(fd);
}

value win_handle_fd(value fd)   /* ML */
{
  return win_alloc_handle((HANDLE) _get_osfhandle(Int_val(fd)));
}
