/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include <signals.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"
#ifndef _WIN32
#include <netdb.h>
#endif

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
  value name = Val_unit, aliases = Val_unit;
  value addr_list = Val_unit, addr = Val_unit;

  Begin_roots4 (name, aliases, addr_list, addr);
    name = copy_string(entry->h_name);
    aliases = copy_string_array(entry->h_aliases);
    entry_h_length = entry->h_length;
#ifdef h_addr
    addr_list = alloc_array(alloc_one_addr, entry->h_addr_list);
#else
    addr = alloc_one_addr(entry->h_addr);
    addr_list = alloc_tuple(1);
    Field(addr_list, 0) = addr;
#endif
    res = alloc_tuple(4);
    Field(res, 0) = name;
    Field(res, 1) = aliases;
    Field(res, 2) = entry->h_addrtype == PF_UNIX ? Val_int(0) : Val_int(1);
    Field(res, 3) = addr_list;
  End_roots();
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
