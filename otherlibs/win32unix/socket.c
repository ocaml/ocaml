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

value unix_socket(domain, type, proto) /* ML */
     value domain, type, proto;
{
  SOCKET s;
  int optionValue;

  /* Set sockets to synchronous mode  */
  optionValue = SO_SYNCHRONOUS_NONALERT;
  setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE, 
             (char *)&optionValue, sizeof(optionValue));

  s = socket(socket_domain_table[Int_val(domain)],
                   socket_type_table[Int_val(type)],
                   Int_val(proto));
  if (s == INVALID_SOCKET) {
    _dosmaperr(WSAGetLastError());
    uerror("socket", Nothing);
  }
  return win_alloc_handle((HANDLE) s);
}
