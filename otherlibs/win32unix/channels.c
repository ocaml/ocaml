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

static int open_descr_flags[3] = {
  O_BINARY, O_TEXT, O_APPEND
};

value win_fd_handle(value handle, value flags) /* ML */
{
  int fd = _open_osfhandle((long) Handle_val(handle),
			   convert_flag_list(flags, open_descr_flags));
  if (fd == -1) uerror("channel_of_descr", Nothing);
  return Val_int(fd);
}

value win_handle_fd(value fd)   /* ML */
{
  return win_alloc_handle((HANDLE) _get_osfhandle(Int_val(fd)));
}
