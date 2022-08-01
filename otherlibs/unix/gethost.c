/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"
#ifndef _WIN32
#include <sys/types.h>
#include <netdb.h>
#endif

#define NETDB_BUFFER_SIZE 10000

#ifdef _WIN32
#define GETHOSTBYADDR_IS_REENTRANT 1
#define GETHOSTBYNAME_IS_REENTRANT 1
#endif

static value alloc_one_addr_4(char const *a)
{
  return caml_alloc_initialized_string(4, a);
}

static value alloc_one_addr_16(char const *a)
{
  return caml_alloc_initialized_string(16, a);
}

static value alloc_host_entry(struct hostent *entry)
{
  CAMLparam0();
  CAMLlocal4(name, aliases, addr_list, adr);
  value (*alloc_one_addr)(char const *);
  value res;

  name = caml_copy_string((char *)(entry->h_name));
  /* PR#4043: protect against buggy implementations of gethostbyname()
     that return a NULL pointer in h_aliases */
  if (entry->h_aliases)
    aliases = caml_copy_string_array((const char**)entry->h_aliases);
  else
    aliases = Atom(0);
  if (entry->h_length == 16) {
    alloc_one_addr = &alloc_one_addr_16;
  } else {
    CAMLassert(entry->h_length == 4);
    alloc_one_addr = &alloc_one_addr_4;
  }
  addr_list =
    caml_alloc_array(alloc_one_addr, (const char**)entry->h_addr_list);
  res = caml_alloc_small(4, 0);
  Field(res, 0) = name;
  Field(res, 1) = aliases;
  switch (entry->h_addrtype) {
  case PF_UNIX:          Field(res, 2) = Val_int(0); break;
  case PF_INET:          Field(res, 2) = Val_int(1); break;
  default: /*PF_INET6 */ Field(res, 2) = Val_int(2); break;
  }
  Field(res, 3) = addr_list;
  CAMLreturn(res);
}

CAMLprim value caml_unix_gethostbyaddr(value a)
{
  char * adr;
  struct in_addr in4;
  struct hostent * hp;
  int addr_type = AF_INET;
  socklen_t addr_len = 4;
#if HAS_IPV6
  struct in6_addr in6;
  if (caml_string_length(a) == 16) {
    addr_type = AF_INET6;
    addr_len = 16;
    in6 = GET_INET6_ADDR(a);
    adr = (char *)&in6;
  } else {
#endif
    in4 = GET_INET_ADDR(a);
    adr = (char *)&in4;
#if HAS_IPV6
  }
#endif
#if HAS_GETHOSTBYADDR_R == 7
  struct hostent h;
  char buffer[NETDB_BUFFER_SIZE];
  int h_errnop;
  caml_enter_blocking_section();
  hp = gethostbyaddr_r(adr, addr_len, addr_type,
                       &h, buffer, sizeof(buffer), &h_errnop);
  caml_leave_blocking_section();
#elif HAS_GETHOSTBYADDR_R == 8
  struct hostent h;
  char buffer[NETDB_BUFFER_SIZE];
  int h_errnop, rc;
  caml_enter_blocking_section();
  rc = gethostbyaddr_r(adr, addr_len, addr_type,
                       &h, buffer, sizeof(buffer), &hp, &h_errnop);
  caml_leave_blocking_section();
  if (rc != 0) hp = NULL;
#else
#ifdef GETHOSTBYADDR_IS_REENTRANT
  caml_enter_blocking_section();
#endif
  hp = gethostbyaddr(adr, addr_len, addr_type);
#ifdef GETHOSTBYADDR_IS_REENTRANT
  caml_leave_blocking_section();
#endif
#endif
  if (hp == (struct hostent *) NULL) caml_raise_not_found();
  return alloc_host_entry(hp);
}

CAMLprim value caml_unix_gethostbyname(value name)
{
  struct hostent * hp;
  char * hostname;
#if HAS_GETHOSTBYNAME_R
  struct hostent h;
  char buffer[NETDB_BUFFER_SIZE];
  int err;
#endif

  if (! caml_string_is_c_safe(name)) caml_raise_not_found();

  hostname = caml_stat_strdup(String_val(name));

#if HAS_GETHOSTBYNAME_R == 5
  {
    caml_enter_blocking_section();
    hp = gethostbyname_r(hostname, &h, buffer, sizeof(buffer), &err);
    caml_leave_blocking_section();
  }
#elif HAS_GETHOSTBYNAME_R == 6
  {
    int rc;
    caml_enter_blocking_section();
    rc = gethostbyname_r(hostname, &h, buffer, sizeof(buffer), &hp, &err);
    caml_leave_blocking_section();
    if (rc != 0) hp = NULL;
  }
#else
#ifdef GETHOSTBYNAME_IS_REENTRANT
  caml_enter_blocking_section();
#endif
  hp = gethostbyname(hostname);
#ifdef GETHOSTBYNAME_IS_REENTRANT
  caml_leave_blocking_section();
#endif
#endif

  caml_stat_free(hostname);

  if (hp == (struct hostent *) NULL) caml_raise_not_found();
  return alloc_host_entry(hp);
}

#else

CAMLprim value caml_unix_gethostbyaddr(value name)
{ caml_invalid_argument("gethostbyaddr not implemented"); }

CAMLprim value caml_unix_gethostbyname(value name)
{ caml_invalid_argument("gethostbyname not implemented"); }

#endif
