/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"
#include <winsock.h>

value unix_listen(sock, backlog) /* ML */
     value sock, backlog;
{
  if (listen((SOCKET) Handle_val(sock), Int_val(backlog)) == -1) {
    _dosmaperr(WSAGetLastError());
    uerror("listen", Nothing);
  }
  return Val_unit;
}
