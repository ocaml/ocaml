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
#include <sys/types.h>

static int sockopt[] = {
  SO_DEBUG, SO_BROADCAST, SO_REUSEADDR, SO_KEEPALIVE, 
  SO_DONTROUTE, SO_OOBINLINE };

value unix_getsockopt(socket, option) /* ML */
     value socket, option;
{
  int optval, optsize;
  optsize = sizeof(optval);
  if (getsockopt(_get_osfhandle(Int_val(socket)), SOL_SOCKET, 
                 sockopt[Int_val(option)], (char *) &optval, &optsize) == -1)
    uerror("getsockopt", Nothing);
  return Val_int(optval);
}

value unix_setsockopt(socket, option, status) /* ML */
     value socket, option, status;
{
  int optval = Int_val(status);
  if (setsockopt(_get_osfhandle(Int_val(socket)), SOL_SOCKET,
                 sockopt[Int_val(option)],
                 (char *) &optval, sizeof(optval)) == -1)
    uerror("setsockopt", Nothing);
  return Val_unit;
}
