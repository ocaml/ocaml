/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include "unix.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"
#include <netdb.h>

static int entry_h_length;

extern int socket_domain_table[];

static value alloc_one_addr(a)
     char * a;
{
  bcopy(a, &sock_addr.s_inet.sin_addr, entry_h_length);
  return alloc_inet_addr(sock_addr.s_inet.sin_addr.s_addr);
}

static value alloc_host_entry(entry)
     struct hostent * entry;
{
  value res;
  Push_roots(r, 4);

  r[0] = copy_string(entry->h_name);
  r[1] = copy_string_array(entry->h_aliases);
  entry_h_length = entry->h_length;
#ifdef h_addr
  r[2] = alloc_array(alloc_one_addr, entry->h_addr_list);
#else
  r[3] = alloc_one_addr(entry->h_addr);
  r[2] = alloc_tuple(1);
  Field(r[2], 0) = r[3];
#endif
  res = alloc_tuple(4);
  Field(res, 0) = r[0];
  Field(res, 1) = r[1];
  Field(res, 2) = entry->h_addrtype == PF_UNIX ? Val_int(0) : Val_int(1);
  Field(res, 3) = r[2];
  Pop_roots();
  return res;
}

value unix_gethostbyaddr(a)   /* ML */
     value a;
{
  uint32 addr;
  struct hostent * entry;
  addr = GET_INET_ADDR(a);
  enter_blocking_section();
  entry = gethostbyaddr((char *) &addr, 4, AF_INET);
  leave_blocking_section();
  if (entry == (struct hostent *) NULL) raise_not_found();
  return alloc_host_entry(entry);
}

value unix_gethostbyname(name)   /* ML */
     value name;
{
  char hostname[256];
  struct hostent * entry;
  strncpy(hostname, String_val(name), sizeof(hostname) - 1);
  hostname[sizeof(hostname) - 1] = 0;
  enter_blocking_section();
  entry = gethostbyname(hostname);
  leave_blocking_section();
  if (entry == (struct hostent *) NULL) raise_not_found();
  return alloc_host_entry(entry);
}

#else

value unix_gethostbyaddr()
{ invalid_argument("gethostbyaddr not implemented"); }
  
value unix_gethostbyname()
{ invalid_argument("gethostbyname not implemented"); }
 
#endif
