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

static int shutdown_command_table[] = {
  0, 1, 2
};

value unix_shutdown(sock, cmd)   /* ML */
     value sock, cmd;
{
  if (shutdown((SOCKET) Handle_val(sock),
               shutdown_command_table[Int_val(cmd)]) == -1)
    unix_error(WSAGetLastError(), "shutdown", Nothing);
  return Val_unit;
}
