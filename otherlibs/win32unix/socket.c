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
#include <sys/types.h>
#include <winsock.h>

int socket_domain_table[] = {
  PF_UNIX, PF_INET
};

int socket_type_table[] = {
  SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET
};

value unix_socket(domain, type, proto, synchronous) /* ML */
     value domain, type, proto;
{
  SOCKET s;
  int oldvalue, newvalue, retcode;

  retcode = getsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
                       (char *) &oldvalue, sizeof(oldvalue));
  if (retcode == 0) {
    /* Set sockets to synchronous or asnychronous mode, as requested */
    newvalue = Bool_val(synchronous) ? SO_SYNCHRONOUS_NONALERT : 0;
    setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, 
               (char *) &newvalue, sizeof(newvalue));
  }
  s = socket(socket_domain_table[Int_val(domain)],
                   socket_type_table[Int_val(type)],
                   Int_val(proto));
  if (retcode == 0) {
    /* Restore initial mode */
    setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, 
               (char *) &oldvalue, sizeof(oldvalue));
  }
  if (s == INVALID_SOCKET) unix_error(WSAGetLastError(), "socket", Nothing);
  return win_alloc_handle((HANDLE) s);
}
