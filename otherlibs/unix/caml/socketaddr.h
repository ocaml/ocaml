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
#include <ws2tcpip.h>
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

/* Deprecated: use struct sockaddr_storage. */
union sock_addr_union {
  struct sockaddr s_gen;
  struct sockaddr_un s_unix;
  struct sockaddr_in s_inet;
#ifdef HAS_IPV6
  struct sockaddr_in6 s_inet6;
#endif
  struct sockaddr_storage s_storage;
};

/* Deprecated: use socklen_t */
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

extern void caml_unix_get_sockaddr (value vaddr,
                               struct sockaddr_storage * addr /*out*/,
                               socklen_t * addr_len /*out*/);
extern value caml_unix_alloc_sockaddr (struct sockaddr * addr /*in*/,
                                  socklen_t addr_len,
                                  int close_on_error);
extern value caml_unix_alloc_inet_addr (struct in_addr * inaddr);
#define GET_INET_ADDR(v) (*((struct in_addr *) (v)))

#ifdef HAS_IPV6
extern value caml_unix_alloc_inet6_addr (struct in6_addr * inaddr);
#define GET_INET6_ADDR(v) (*((struct in6_addr *) (v)))

/* Compatibility definition for the pre-5.0 name of this function */
#ifndef CAML_BUILDING_UNIX
#define alloc_inet6_addr caml_unix_alloc_inet6_addr
#endif /* CAML_BUILDING_UNIX */
#endif /* HAS_IPV6 */

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
/* API compatibility between union sock_addr_union and
   struct sockaddr_storage.
   C++ doesn't support C11 _Generic, so use function overloading. */
static inline void
caml_unix_get_sockaddr(value vaddr, union sock_addr_union * addr /*out*/,
                       socklen_param_type * addr_len /*out*/) {
  caml_unix_get_sockaddr(vaddr, &addr->s_storage, addr_len);
}
static inline value
caml_unix_alloc_sockaddr(union sock_addr_union * addr /*in*/,
                         socklen_param_type addr_len,
                         int close_on_error) {
  return caml_unix_alloc_sockaddr(&addr->s_gen, addr_len, close_on_error);
}
#else
/* API compatibility between union sock_addr_union and
   struct sockaddr_storage. */
#define caml_unix_get_sockaddr(vaddr, addr, addr_len)         \
  caml_unix_get_sockaddr(                                     \
    (vaddr),                                                  \
    _Generic((addr),                                          \
             union sock_addr_union *:                         \
               &((union sock_addr_union *)(addr))->s_storage, \
             default: (addr)),                                \
    (addr_len))
#define caml_unix_alloc_sockaddr(addr, addr_len, close_on_error)  \
  caml_unix_alloc_sockaddr(                                       \
    _Generic((addr),                                              \
             union sock_addr_union *:                             \
               &((union sock_addr_union *)(addr))->s_gen,         \
             default: (addr)),                                    \
    (addr_len),                                                   \
    (close_on_error))
#endif

#endif /* CAML_SOCKETADDR_H */
