/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"
#include <fcntl.h>

CAMLprim value win_fd_handle(value handle)
{
  int fd = _open_osfhandle((long) Handle_val(handle), O_BINARY);
  if (fd == -1) uerror("channel_of_descr", Nothing);
  return Val_int(fd);
}

CAMLprim value win_handle_fd(value fd)
{
  return win_alloc_handle((HANDLE) _get_osfhandle(Int_val(fd)));
}
