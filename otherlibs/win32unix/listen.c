/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"
#include <winsock.h>

value unix_listen(sock, backlog) /* ML */
     value sock, backlog;
{
  if (listen((SOCKET) Handle_val(sock), Int_val(backlog)) == -1)
    unix_error(WSAGetLastError(), "listen", Nothing);
  return Val_unit;
}
