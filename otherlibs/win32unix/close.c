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
#include "unixsupport.h"

CAMLprim value unix_close(value fd)
{
  if (Descr_kind_val(fd) == KIND_SOCKET) {
    if (closesocket(Socket_val(fd)) != 0) {
      win32_maperr(WSAGetLastError());
      uerror("close", Nothing);
    }
  } else {
    if (! CloseHandle(Handle_val(fd))) {
      win32_maperr(GetLastError());
      uerror("close", Nothing);
    }
  }
  return Val_unit;
}
