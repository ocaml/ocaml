/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <string.h>
#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <str.h>
#include <errno.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

#ifdef _WIN32
#define EAFNOSUPPORT WSAEAFNOSUPPORT
#endif

union sock_addr_union sock_addr;
int sock_addr_len;

value alloc_inet_addr(unsigned int a)
{
  value res;
  res = alloc_string(sizeof(uint32));
  GET_INET_ADDR(res) = a;
  return res;
}

void get_sockaddr(value a)
{
  switch(Tag_val(a)) {
#ifndef _WIN32
  case 0:                       /* ADDR_UNIX */
    { value path;
      mlsize_t len;
      path = Field(a, 0);
      len = string_length(path);
      sock_addr.s_unix.sun_family = AF_UNIX;
      if (len >= sizeof(sock_addr.s_unix.sun_path)) {
        unix_error(ENAMETOOLONG, "", path);
      }
      bcopy(String_val(path), sock_addr.s_unix.sun_path, (int) len + 1);
      sock_addr_len =
        ((char *)&(sock_addr.s_unix.sun_path) - (char *)&(sock_addr.s_unix))
        + len;
      break;
    }
#endif
  case 1:                       /* ADDR_INET */
    {
      char * p;
      int n;
      for (p = (char *) &sock_addr.s_inet, n = sizeof(sock_addr.s_inet);
           n > 0; p++, n--)
        *p = 0;
      sock_addr.s_inet.sin_family = AF_INET;
      sock_addr.s_inet.sin_addr.s_addr = GET_INET_ADDR(Field(a, 0));
      sock_addr.s_inet.sin_port = htons(Int_val(Field(a, 1)));
      sock_addr_len = sizeof(struct sockaddr_in);
      break;
    }
  }
}

value alloc_sockaddr(void)
{
  value res;
  switch(sock_addr.s_gen.sa_family) {
#ifndef _WIN32
  case AF_UNIX:
    { value n = copy_string(sock_addr.s_unix.sun_path);
      Begin_root (n);
        res = alloc_small(1, 0);
	Field(res,0) = n;
      End_roots();
      break;
    }
#endif
  case AF_INET:
    { value a = alloc_inet_addr(sock_addr.s_inet.sin_addr.s_addr);
      Begin_root (a);
        res = alloc_small(2, 1);
	Field(res,0) = a;
	Field(res,1) = Val_int(ntohs(sock_addr.s_inet.sin_port));
      End_roots();
      break;
    }
  default:
    unix_error(EAFNOSUPPORT, "", Nothing);
  }
  return res;
}

#endif
