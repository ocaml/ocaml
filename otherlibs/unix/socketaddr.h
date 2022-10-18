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

#ifndef CAML_SOCKETADDR_H
#define CAML_SOCKETADDR_H

#include "caml/misc.h"

#ifdef _WIN32

/* Code duplication with runtime/debugger.c is inevitable, because
 * pulling winsock2.h creates many naming conflicts. */
#include <winsock2.h>
#ifdef HAS_AFUNIX_H
#include <afunix.h>
#else
#define UNIX_PATH_MAX 108

struct sockaddr_un {
  ADDRESS_FAMILY sun_family;
  char sun_path[UNIX_PATH_MAX];
};

#define SIO_AF_UNIX_GETPEERPID _WSAIOR(IOC_VENDOR, 256)

#endif

#else
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

union sock_addr_union {
  struct sockaddr s_gen;
  struct sockaddr_un s_unix;
  struct sockaddr_in s_inet;
  struct sockaddr_in6 s_inet6;
};

typedef socklen_t socklen_param_type;

#ifdef __cplusplus
extern "C" {
#endif

/* Compatibility definitions for the pre-5.0 names of these functions */
#ifndef CAML_BUILDING_UNIX
#define get_sockaddr caml_unix_get_sockaddr
#define alloc_sockaddr caml_unix_alloc_sockaddr
#define alloc_inet_addr caml_unix_alloc_inet_addr
#endif /* CAML_BUILDING_UNIX */

extern void caml_unix_get_sockaddr (value mladdr,
                               union sock_addr_union * addr /*out*/,
                               socklen_param_type * addr_len /*out*/);
extern value caml_unix_alloc_sockaddr (union sock_addr_union * addr /*in*/,
                                  socklen_param_type addr_len,
                                  int close_on_error);
extern value caml_unix_alloc_inet_addr (struct in_addr * inaddr);
#define GET_INET_ADDR(v) (*((struct in_addr *) (v)))

extern value caml_unix_alloc_inet6_addr (struct in6_addr * inaddr);
#define GET_INET6_ADDR(v) (*((struct in6_addr *) (v)))

/* Compatibility definition for the pre-5.0 name of this function */
#ifndef CAML_BUILDING_UNIX
#define alloc_inet6_addr caml_unix_alloc_inet6_addr
#endif /* CAML_BUILDING_UNIX */

#ifdef __cplusplus
}
#endif

#endif /* CAML_SOCKETADDR_H */
