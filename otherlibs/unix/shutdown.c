/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include <sys/socket.h>

static int shutdown_command_table[] = {
  0, 1, 2
};

value unix_shutdown(value sock, value cmd)   /* ML */
{
  if (shutdown(Int_val(sock), shutdown_command_table[Int_val(cmd)]) == -1)
    uerror("shutdown", Nothing);
  return Val_unit;
}

#else

value unix_shutdown(value sock, value cmd)
{ invalid_argument("shutdown not implemented"); }

#endif
