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

extern long _get_osfhandle(int);
extern int _open_osfhandle(long, int);

CAMLprim value win_fd_handle(value handle)
{
  int fd;
  if (CRT_fd_val(handle) != NO_CRT_FD) {
    fd = CRT_fd_val(handle);
  } else {
    fd = _open_osfhandle((long) Handle_val(handle), O_BINARY);
    if (fd == -1) uerror("channel_of_descr", Nothing);
    CRT_fd_val(handle) = fd;
  }
  return Val_int(fd);
}

CAMLprim value win_handle_fd(value vfd)
{
  int crt_fd = Int_val(vfd);
  value res = win_alloc_handle_or_socket((HANDLE) _get_osfhandle(crt_fd));
  CRT_fd_val(res) = crt_fd;
  return res;
}
