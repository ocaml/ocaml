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
#include "unixsupport.h"
#include <fcntl.h>

static int open_descr_flags[10] = {
  0, 0, 0, 0, O_APPEND, 0, 0, 0, O_BINARY, O_TEXT
};

value win_fd_handle(value handle, value flags) /* ML */
{
  return Val_int(_open_osfhandle(Handle_val(handle),
                                 convert_flag_list(open_descr_flags, flags)));
}

value win_handle_fd(value fd)   /* ML */
{
  return win_alloc_handle((HANDLE) _get_osfhandle(Int_val(fd)));
}
