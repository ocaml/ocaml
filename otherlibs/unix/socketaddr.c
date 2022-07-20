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
#include <caml/memory.h>
#include <errno.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include "socketaddr.h"

#ifdef _WIN32
#undef EAFNOSUPPORT
#define EAFNOSUPPORT WSAEAFNOSUPPORT
#endif

CAMLexport value caml_unix_alloc_inet_addr(struct in_addr * a)
{
  value res;
  /* Use a string rather than an abstract block so that it can be
     marshaled safely.  Remember that a is in network byte order,
     hence is marshaled in an endian-independent manner. */
  res = caml_alloc_initialized_string(4, (char *)a);
  return res;
}

#ifdef HAS_IPV6

CAMLexport value caml_unix_alloc_inet6_addr(struct in6_addr * a)
{
  value res;
  res = caml_alloc_initialized_string(16, (char *)a);
  return res;
}

#endif

void caml_unix_get_sockaddr(value mladr,
                       union sock_addr_union * adr /*out*/,
                       socklen_param_type * adr_len /*out*/)
{
  switch(Tag_val(mladr)) {
  case 0:                       /* ADDR_UNIX */
    { value path;
      mlsize_t len;
      path = Field(mladr, 0);
      len = caml_string_length(path);
      adr->s_unix.sun_family = AF_UNIX;
      if (len >= sizeof(adr->s_unix.sun_path)) {
        caml_unix_error(ENAMETOOLONG, "", path);
      }
      /* "Abstract" sockets in Linux have names starting with '\0' */
      if (Byte(path, 0) != 0 && ! caml_string_is_c_safe(path)) {
        caml_unix_error(ENOENT, "", path);
      }
      memmove (adr->s_unix.sun_path, String_val(path), len + 1);
      *adr_len =
        ((char *)&(adr->s_unix.sun_path) - (char *)&(adr->s_unix))
        + len;
      break;
    }
  case 1:                       /* ADDR_INET */
#ifdef HAS_IPV6
    if (caml_string_length(Field(mladr, 0)) == 16) {
      memset(&adr->s_inet6, 0, sizeof(struct sockaddr_in6));
      adr->s_inet6.sin6_family = AF_INET6;
      adr->s_inet6.sin6_addr = GET_INET6_ADDR(Field(mladr, 0));
      adr->s_inet6.sin6_port = htons(Int_val(Field(mladr, 1)));
#ifdef SIN6_LEN
      adr->s_inet6.sin6_len = sizeof(struct sockaddr_in6);
#endif
      *adr_len = sizeof(struct sockaddr_in6);
      break;
    }
#endif
    memset(&adr->s_inet, 0, sizeof(struct sockaddr_in));
    adr->s_inet.sin_family = AF_INET;
    adr->s_inet.sin_addr = GET_INET_ADDR(Field(mladr, 0));
    adr->s_inet.sin_port = htons(Int_val(Field(mladr, 1)));
#ifdef SIN6_LEN
    adr->s_inet.sin_len = sizeof(struct sockaddr_in);
#endif
    *adr_len = sizeof(struct sockaddr_in);
    break;
  }
}

static value alloc_unix_sockaddr(value path) {
  CAMLparam1(path);
  CAMLlocal1(res);
  res = caml_alloc_small(1, 0);
  Field(res,0) = path;
  CAMLreturn(res);
}

value caml_unix_alloc_sockaddr(union sock_addr_union * adr /*in*/,
                          socklen_param_type adr_len, int close_on_error)
{
  CAMLparam0();
  CAMLlocal1(a);
  value res;
  if (adr_len < offsetof(struct sockaddr, sa_data)) {
    // Only possible for an unnamed AF_UNIX socket, in
    // which case sa_family might be uninitialized.
    return alloc_unix_sockaddr(caml_alloc_string(0));
  }

  switch(adr->s_gen.sa_family) {
  case AF_UNIX:
    { /* Based on recommendation in section BUGS of Linux unix(7). See
         http://man7.org/linux/man-pages/man7/unix.7.html. */
      mlsize_t struct_offset = offsetof(struct sockaddr_un, sun_path);
      mlsize_t path_length = 0;
      if (adr_len > struct_offset) {
        path_length = adr_len - struct_offset;

        /* paths _may_ be null-terminated, but Linux abstract sockets
         * start with a null, and may contain internal nulls. */
        path_length = (
#ifdef __linux__
          (adr->s_unix.sun_path[0] == '\0') ? path_length :
#endif
          strnlen(adr->s_unix.sun_path, path_length)
        );
      }

      res = alloc_unix_sockaddr(
        caml_alloc_initialized_string(path_length, (char *)adr->s_unix.sun_path)
      );
      break;
    }
  case AF_INET:
    { a = caml_unix_alloc_inet_addr(&adr->s_inet.sin_addr);
      res = caml_alloc_small(2, 1);
      Field(res,0) = a;
      Field(res,1) = Val_int(ntohs(adr->s_inet.sin_port));
      break;
    }
#ifdef HAS_IPV6
  case AF_INET6:
    { a = caml_unix_alloc_inet6_addr(&adr->s_inet6.sin6_addr);
      res = caml_alloc_small(2, 1);
      Field(res,0) = a;
      Field(res,1) = Val_int(ntohs(adr->s_inet6.sin6_port));
      break;
    }
#endif
  default:
    if (close_on_error != -1) close (close_on_error);
    caml_unix_error(EAFNOSUPPORT, "", Nothing);
  }
  CAMLreturn(res);
}

#endif
